;;; whisper.el --- Speech-to-Text interface using OpenAI's whisper model -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Imran Khan.

;; Author: Imran Khan <imran@khan.ovh>
;; URL: https://github.com/natrys/whisper.el
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Speech-to-Text interface for Emacs using OpenAI's whisper model
;; Uses the awesome C/C++ port that runs on CPU.
;; See: https://github.com/ggerganov/whisper.cpp
;;
;;; Code:

(require 'cl-lib)

;;; User facing options

(defgroup whisper ()
  "Speech-to-text interface using OpenAI's whisper model."
  :group 'external)

(defcustom whisper-enable-speed-up nil
  "Whether to sacrifices some accuracy to speed up transcribing.

Basically whether to use \"-su\" flag in whisper.cpp.  You should experiment
enabling it to see if it works well enough for you.

It's currently disabled by upstream because of bugs, so does nothing."
  :type 'boolean
  :group 'whisper)

(defcustom whisper-use-threads nil
  "How many threads to use for transcribing.

Default is whisper.cpp default (which is number of cores but maxed at 4)."
  :type 'integer
  :group 'whisper)

(defcustom whisper-install-directory (locate-user-emacs-file ".cache/")
  "Location of where whisper.cpp is installed."
  :type 'directory
  :group 'whisper)

(defcustom whisper-recording-timeout 300
  "Number of seconds after which recording will be automatically stopped."
  :type '(choice integer (const nil))
  :group 'whisper)

(defcustom whisper-model "base"
  "Which whisper model to use (default is base).

Choose between: tiny, base, small, medium, large.

The first four comes with .en variant that only works with English, but
might speed up transcribing."
  :type 'string
  :group 'whisper)

(defcustom whisper-language "en"
  "Set spoken language for audio.

When dealing with unknown language, set this to `auto'.

Be sure to use generic model (without .en suffix) when language is not English."
  :type 'string
  :group 'whisper)

(defcustom whisper-translate nil
  "Whether to translate to English first, before transcribing."
  :type 'boolean
  :group 'whisper)

(defcustom whisper-quantize nil
  "Whether to use quantized version of the model in whisper.cpp.

Quantization is a technique to reduce the computational and memory costs of
running inference by representing the weights and activations with low-precision
data types.  This sacrifices precision for resource efficiency.  The idea is
that quantized version of bigger model may afford you to use it (if you are RAM
constrained e.g.) with some penalty, while still being better than the smaller
model you would be using otherwise.

Valid values are (from lowest to highest quality):
- q4_0
- q4_1
- q4_k
- q5_0
- q5_1
- q5_k
- q6_k
- q8_0"
  :type '(choice string (const nil))
  :group 'whisper)

(defcustom whisper-install-whispercpp t
  "Specify whether to install whisper.cpp automatically.

By default whisper.el compiles whisper.cpp automatically.   But if you are on a
platform where our automatic whisper.cpp install doesn't work but you are able
to do so manually, you can set this to `manual' to skip our try (and failure)
to install it automatically.  Note that in case a functional install is found
at `whisper-install-directory', we can still do model download, quantization
automatically.

But if you are planning to use something other than whisper.cpp entirely, as
such don't want to install it nor run checks for it, you may opt out of
whisper.cpp as a whole by setting this to nil.  In that case it's your
responsibility to override `whisper-command' with appropriate function."
  :type '(choice boolean (const manual))
  :group 'whisper)

(defcustom whisper-insert-text-at-point t
  "Whether to put whisper output under point in current buffer.

When nil, instead of inserting text under current point, a temporary buffer
containing whisper output text is displayed.  The buffer name is distinguised
with current timestamp and it's the user's responsibility to kill the buffer if
they want to."
  :type 'boolean
  :group 'whisper)

(defcustom whisper-return-cursor-to-start t
  "Whether to re-position the cursor after transcription.

When non-nil, the cursor is returned to the original invocation point.
Otherwise, the cursor remains at the end of the inserted transcription."
  :type 'boolean
  :group 'whisper)

(defcustom whisper-show-progress-in-mode-line t
  "Whether to show transcription progress in mode line."
  :type 'boolean
  :group 'whisper)

(define-obsolete-variable-alias 'whisper-pre-process-hook 'whisper-before-transcription-hook "0.3.0")
(defcustom whisper-before-transcription-hook '(whisper--check-buffer-read-only-p)
  "Hook run before whisper.el does anything."
  :type 'hook
  :group 'whisper)

(define-obsolete-variable-alias 'whisper-post-process-hook 'whisper-after-transcription-hook "0.3.0")
(defcustom whisper-after-transcription-hook nil
  "Hook run after whisper command finishes producing output.

If you want to transform the command output text in some way before they are
inserted into the original buffer, add your function here.  Each function in
the hook will be run in a buffer containing the whisper command output text
as its current buffer, and with point set to beginning of that buffer."
  :type 'hook
  :group 'whisper)

(defcustom whisper-after-insert-hook nil
  "Hook run after whisper command has inserted the transcription.

This hook will be run from the buffer in which the transcription was inserted."
  :type 'hook
  :group 'whisper)

;;; Internal variables

(defvar whisper--stdout-buffer-name "*whisper-stdout*")
(defvar whisper--stderr-buffer-name "*whisper-stderr*")
(defvar whisper--compilation-buffer-name "*whisper-compilation*")

(defvar whisper--point-buffer nil)
(defvar whisper--compilation-buffer nil)

(defvar whisper--recording-process nil)
(defvar whisper--transcribing-process nil)
(defvar whisper--marker (make-marker))

(defvar whisper--install-path nil)

(defvar whisper--temp-file
  (concat (temporary-file-directory) "emacs-whisper.wav")
  "Location of the temporary audio file.")

(defvar whisper--ffmpeg-input-format
  (pcase system-type
    ('gnu/linux (if (or (executable-find "pulseaudio")
                        (executable-find "pipewire-pulse"))
                    "pulse"
                  "alsa"))
    ('darwin "avfoundation")
    ('windows-nt "dshow")
    (_ nil)))

(defvar whisper--ffmpeg-input-device
  (pcase whisper--ffmpeg-input-format
    ("pulse" "default")
    (_ nil)))

(defvar whisper--ffmpeg-input-file nil)

(defvar whisper--using-whispercpp nil)

(defvar whisper--progress-level "0")

(defvar whisper--mode-line-recording-indicator
  (propertize "" 'face font-lock-warning-face))

(defvar whisper--mode-line-transcribing-indicator
  (propertize "" 'face font-lock-warning-face))

(defun whisper--check-buffer-read-only-p ()
  "Error out if current buffer is read-only."
  (when (and whisper-insert-text-at-point buffer-read-only)
    (error "Buffer is read-only, can't insert text here")))

;; Maybe sox would be a lighter choice for something this simple?
(defun whisper--record-command (output-file)
  "Produces FFmpeg command to be run given location of OUTPUT-FILE."
  (unless (executable-find "ffmpeg")
    (error "Needs FFmpeg to record audio"))

  (unless (or whisper--ffmpeg-input-file
              whisper--ffmpeg-input-format)
    (error "Set a suitable value for whisper--ffmpeg-input-format"))

  (unless (or whisper--ffmpeg-input-file
              whisper--ffmpeg-input-device)
    (error "Set a suitable value for whisper--ffmpeg-input-device"))

  `("ffmpeg"
    ,@(unless whisper--ffmpeg-input-file
        (list "-f" whisper--ffmpeg-input-format))
    "-i" ,(or whisper--ffmpeg-input-file whisper--ffmpeg-input-device)
    ,@(when (and (not whisper--ffmpeg-input-file) whisper-recording-timeout)
        (list "-t" (number-to-string whisper-recording-timeout)))
    "-ar" "16000"
    "-y" ,output-file))

(defun whisper-command (input-file)
  "Produces whisper.cpp command to be run on the INPUT-FILE.

If you want to use something other than whisper.cpp, you should override this
function to produce the command for the inference engine of your choice."
  (let ((base (expand-file-name (file-name-as-directory whisper--install-path))))
    `(,(concat base (if (eq system-type 'windows-nt) "main.exe" "main"))
      ,@(when whisper-use-threads (list "--threads" (number-to-string whisper-use-threads)))
      ;; ,@(when whisper-enable-speed-up '("--speed-up"))
      ,@(when whisper-translate '("--translate"))
      ,@(when whisper-show-progress-in-mode-line '("--print-progress"))
      "--language" ,whisper-language
      "--model" ,(whisper--model-file whisper-quantize)
      "--no-timestamps"
      "--file" ,input-file)))

(defalias 'whisper--transcribe-command 'whisper-command)
(make-obsolete 'whisper--transcribe-command 'whisper-command "0.1.6")

(defun whisper--mode-line-indicator (phase)
  "Determine what to show in mode line depending on PHASE."
  (if (eq phase 'recording)
      whisper--mode-line-recording-indicator
    (if whisper--using-whispercpp
        '(:eval (concat whisper--mode-line-transcribing-indicator whisper--progress-level "%%%%"))
      whisper--mode-line-transcribing-indicator)))

(defun whisper--setup-mode-line (command phase)
  "Set up PHASE appropriate indicator in the mode line.

Depending on the COMMAND we either show the indicator or hide it."
  (when whisper-show-progress-in-mode-line
    (let ((indicator `(t ,(whisper--mode-line-indicator phase))))
      (if (eq command :show)
          (cl-pushnew indicator global-mode-string :test #'equal)
        (setf global-mode-string (remove indicator global-mode-string))
        (setq whisper--progress-level "0")))
    (force-mode-line-update)))

(defun whisper--get-whispercpp-progress (_process output)
  "Notify user of transcription progress by parsing whisper.cpp OUTPUT."
  (let ((marker "whisper_print_progress_callback: progress ="))
    (when (string-match (rx-to-string `(seq bol ,marker (* blank) (group (+ digit)) "%")) output)
      (setq whisper--progress-level (match-string 1 output))
      (force-mode-line-update))))

(defun whisper--using-whispercpp-p ()
  "Crude way to check we are in fact using whisper.cpp."
  (let ((command (car (whisper-command whisper--temp-file))))
    (or (string-match-p (rx-to-string '(seq "whisper.cpp" (any "/\\") "main")) command)
        ;; for the staunch Nix user
        (string-equal command "whisper-cpp"))))

(defun whisper--record-audio ()
  "Start audio recording process in the background."
  (when whisper-insert-text-at-point
    (with-current-buffer whisper--point-buffer
      (setq whisper--marker (point-marker))))
  (if whisper--ffmpeg-input-file
      (message "[*] Pre-processing media file")
    (message "[*] Recording audio")
    (whisper--setup-mode-line :show 'recording))
  (setq whisper--recording-process
        (make-process
         :name "whisper-recording"
         :command (whisper--record-command whisper--temp-file)
         :connection-type nil
         :buffer nil
         :sentinel (lambda (_process event)
                     (whisper--setup-mode-line :hide 'recording)
                     (cond ((or (string-equal "finished\n" event)
                                ;; this is would be sane
                                (string-equal "terminated\n" event)
                                ;; but this is reality
                                (string-equal "exited abnormally with code 255\n" event))
                            (whisper--transcribe-audio))
                           ((string-equal "exited abnormally with code 1\n" event)
                            (if whisper--ffmpeg-input-file
                                (error "FFmpeg failed to convert given file")
                              (error "FFmpeg failed to record audio"))))))))

(defun whisper--transcribe-audio ()
  "Start audio transcribing process in the background."
  (message "[-] Transcribing/Translating audio")
  (setq whisper--using-whispercpp (whisper--using-whispercpp-p))
  (whisper--setup-mode-line :show 'transcribing)
  (setq whisper--transcribing-process
        (make-process
         :name "whisper-transcribing"
         :command (whisper-command whisper--temp-file)
         :connection-type nil
         :buffer (get-buffer-create whisper--stdout-buffer-name)
         :stderr (if (and whisper-show-progress-in-mode-line whisper--using-whispercpp)
                     (make-pipe-process
                      :name "whisper-stderr"
                      :filter #'whisper--get-whispercpp-progress)
                   (get-buffer-create whisper--stderr-buffer-name))
         :coding 'utf-8
         :sentinel (lambda (_process event)
                     (unwind-protect
                         (when-let* ((whisper--stdout-buffer (get-buffer whisper--stdout-buffer-name))
                                     (finished (and (buffer-live-p whisper--stdout-buffer)
                                                    (string-equal "finished\n" event))))
                           (with-current-buffer whisper--stdout-buffer
                             (goto-char (point-min))
                             (skip-chars-forward " \n")
                             (when (> (point) (point-min))
                               (delete-region (point-min) (point)))
                             (goto-char (point-max))
                             (skip-chars-backward " \n")
                             (when (> (point-max) (point))
                               (delete-region (point) (point-max)))
                             (when (= (buffer-size) 0)
                               (error "Whisper command produced no output"))
                             (goto-char (point-min))
                             (run-hook-wrapped 'whisper-after-transcription-hook
                                               (lambda (f)
                                                 (with-current-buffer whisper--stdout-buffer
                                                   (save-excursion
                                                     (funcall f)))
                                                 nil))
                             (when (> (buffer-size) 0)
                               (if whisper-insert-text-at-point
                                   (with-current-buffer (marker-buffer whisper--marker)
                                     (goto-char whisper--marker)
                                     (insert-buffer-substring whisper--stdout-buffer)
                                     (when whisper-return-cursor-to-start
                                       (goto-char whisper--marker)))
                                 (with-current-buffer
                                     (get-buffer-create
                                      (format "*whisper-%s*" (format-time-string "%+4Y%m%d%H%M%S")))
                                   (insert-buffer-substring whisper--stdout-buffer)
                                   (display-buffer (current-buffer)))))))
                       (set-marker whisper--marker nil)
                       (setq whisper--point-buffer nil)
                       (kill-buffer whisper--stdout-buffer-name)
                       (unless whisper-show-progress-in-mode-line (kill-buffer whisper--stderr-buffer-name))
                       (whisper--setup-mode-line :hide 'transcribing)
                       (message nil)
                       (run-hooks 'whisper-after-insert-hook))))))

(defun whisper--check-model-consistency ()
  "Check if chosen language and model are consistent."
  (when (and (not (string-equal "en" whisper-language))
             (string-suffix-p ".en" whisper-model))
    (error "Use generic model (non .en version) for non-English languages"))

  (unless (or (= 2 (length whisper-language))
              (string-equal "auto" whisper-language))
    (error (concat "Unknown language shortcode. If unsure use 'auto'. For full list, see: "
                   "https://github.com/ggerganov/whisper.cpp/blob/master/whisper.cpp")))

  (let ((model-pattern (rx (seq (or "tiny" "base" "small" "medium" (seq "large" (opt (seq "-v" (any "1-2")))))
                                (opt (seq "." (= 2 (any "a-z")))))))
        (quantization-pattern (rx (or "q4_0" "q4_1" "q4_k" "q5_0" "q5_1" "q5_k" "q6_k" "q8_0"))))
    (unless (string-match-p model-pattern whisper-model)
      (error (concat "Speech recognition model " whisper-model " not recognised. For the list, see: "
                     "https://github.com/ggerganov/whisper.cpp/tree/master/models")))
    (when whisper-quantize
      (unless (string-match-p quantization-pattern whisper-quantize)
        (error "Quantization format not recognized")))))

(defun whisper--model-file (quantized)
  "Return path of QUANTIZED model file relative to `whisper-install-directory'."
  (let ((base (concat
               (expand-file-name (file-name-as-directory whisper-install-directory))
               "whisper.cpp/"))
        (name (if quantized (concat whisper-model "-" whisper-quantize) whisper-model)))
    (concat base "models/ggml-" name ".bin")))

(defun whisper--check-install-and-run (buffer status)
  "Run whisper after ensuring installation correctness.

This is a horrible function, and in time due a rewrite. But for now I find this
amusing and a little bit instructive as to how it became a mess.

To conduct and display installation progress, `compilation-mode' is used because
it's asynchronous and most importantly capable of handling progress output
of programs like wget.  However the asynchronicity comes with some complexity
cost when more than one tasks are run (but only one after another), as there is
no built-in async/await support, so need to use callbacks instead.

That is done by adding the callback to `compilation-finish-functions'.  Arguably
it would be simpler to use one function per task and then chain these callbacks.
However personally I preferred to logically group these together and handle
synchronisation and cleaning up in one place, hence this big function.

Conventionally, these callbacks are going to be called by passing current
compilation-buffer in BUFFER and what event triggered the callback in STATUS so
that's the function signature here.  Checking if BUFFER is indeed originating
from this particular compilation buffer and not something the user have running
elsewhere is necessary.  It's possible to make the hook buffer local, but
compilation command starts before the hook could be added so I have some
theoretical concern about possible race condition in that approach.

Small Downside of re-using same function is that we need to differentiate
whether this run is a callback or first normal call, that's what the made up
status \"whisper-start\" does.

The unfortunate price of asynchronicity is that this breaks dynamic binding
because call stack is disrupted, callbacks are executed at a later time outside
of the original dynamic context.  The fault not only lies here, but ultimately
`make-process' itself is async, so it will likely take some insane hacks that
escapes me right now, to get let bindings work like synchronous code."
  (catch 'early-return
    (unless (string-equal "whisper-start" status)
      ;; shouldn't do anything when triggered by compilation buffers from elsewhere
      (unless (eq buffer whisper--compilation-buffer)
        (throw 'early-return nil))

      ;; being here means this compilation job either finished or was interrupted
      (remove-hook 'compilation-finish-functions #'whisper--check-install-and-run)
      (kill-buffer whisper--compilation-buffer))

    (let ((base (concat
                 (expand-file-name (file-name-as-directory whisper-install-directory))
                 "whisper.cpp/"))
          (compilation-buffer-name-function '(lambda (_) whisper--compilation-buffer-name)))

      (setq whisper--install-path base)

      (when (and (not (file-exists-p (concat base (if (eq system-type 'windows-nt) "main.exe" "main"))))
                 (not (string-equal "interrupt\n" status)))

        (when (eq whisper-install-whispercpp 'manual)
          (error (format "Couldn't find whisper.cpp install at: %s" base)))

        (if (yes-or-no-p (format "Couldn't find whisper.cpp, install it at: %s ?" base))
            (let ((make-commands
                   (concat
                    "mkdir -p " whisper-install-directory " && "
                    "cd " whisper-install-directory " && "
                    "git clone https://github.com/ggerganov/whisper.cpp && "
                    "cd whisper.cpp && "
                    "make")))
              (setq whisper--compilation-buffer (get-buffer-create whisper--compilation-buffer-name))
              (add-hook 'compilation-finish-functions #'whisper--check-install-and-run)
              (compile make-commands)
              (throw 'early-return nil))
          (error "Needs whisper.cpp to be installed")))

      (when (and (not (file-exists-p (whisper--model-file nil)))
                 (not (string-equal "interrupt\n" status)))
        (if (yes-or-no-p (format "Speech recognition model \"%s\" isn't available, download now?" whisper-model))
            (let ((make-commands
                   (concat
                    "cd " base " && "
                    "models/download-ggml-model.sh " whisper-model)))
              (setq whisper--compilation-buffer (get-buffer-create whisper--compilation-buffer-name))
              (add-hook 'compilation-finish-functions #'whisper--check-install-and-run)
              (compile make-commands)
              (throw 'early-return nil))
          (error "Needs speech recognition model to run whisper")))

      (when (and whisper-quantize
                 (not (file-exists-p (whisper--model-file t)))
                 (not (string-equal "interrupt\n" status)))
        (let ((make-commands
               (concat
                "cd " base " && "
                "make quantize" " && "
                "echo 'Quantizing the model....'" " && "
                "./quantize " (whisper--model-file nil) " " (whisper--model-file t) " " whisper-quantize)))
          (setq whisper--compilation-buffer (get-buffer-create whisper--compilation-buffer-name))
          (add-hook 'compilation-finish-functions #'whisper--check-install-and-run)
          (compile make-commands)
          (throw 'early-return nil)))

      (when (string-equal "interrupt\n" status)
        ;; double check to be sure before cleaning up
        (when (and (file-directory-p base) (string-suffix-p "/whisper.cpp/" base))
          (if (file-exists-p (concat base (if (eq system-type 'windows-nt) "main.exe" "main")))
              ;; model download interrupted probably, should delete partial file
              (progn
                (message "Download interrupted, cleaning up.")
                (delete-file (concat base "models/" "ggml-" whisper-model ".bin")))
            ;; otherwise whisper.cpp compilation got interrupted
            ;; doesn't hurt to nuke it too and start later from fresh point
            (message "Installation interrupted, cleaning up.")
            (delete-directory whisper--install-path t)))
        (throw 'early-return nil))

      (when (string-equal "finished\n" status)
        (unless (or whisper--ffmpeg-input-file
                    (yes-or-no-p "Speech recognition model download completed, want to record audio now?"))
          (throw 'early-return nil)))

      ;; finally
      (whisper--record-audio))))

;;;###autoload
(defun whisper-run (&optional arg)
  "Transcribe/translate audio using whisper.

When ARG is given, uses a local file as input. Otherwise records the audio.

This is a dwim function that does different things depending on current state:

- When inference engine (whisper.cpp) isn't installed, installs it first.
- When speech recognition model isn't available, downloads it.
- When installation/download is already in progress, cancels those.
- When installation is valid, starts recording audio.
- When recording is in progress, stops it and starts transcribing.
- When transcribing is in progress, cancels it."
  (interactive "P")
  (if (process-live-p whisper--transcribing-process)
      (when (yes-or-no-p "A transcribing is already in progress, kill it?")
        (kill-process whisper--transcribing-process))

    (cond
     ((process-live-p whisper--recording-process)
      (interrupt-process whisper--recording-process))
     ((and (buffer-live-p whisper--compilation-buffer)
           (process-live-p (get-buffer-process whisper--compilation-buffer)))
      (when-let ((proc (get-buffer-process whisper--compilation-buffer)))
	(interrupt-process proc)))
     (t
      (setq whisper--point-buffer (current-buffer))
      (run-hooks 'whisper-before-transcription-hook)
      (when whisper-install-whispercpp
        (whisper--check-model-consistency))
      (setq-default whisper--ffmpeg-input-file nil)
      (when (equal arg '(4))
        (when-let ((file (expand-file-name (read-file-name "Media file: " nil nil t))))
          (unless (file-readable-p file)
            (error "Media file doesn't exist or isn't readable"))
          (setq-default whisper--ffmpeg-input-file file)))
      (setq whisper--using-whispercpp nil)
      (if whisper-install-whispercpp
          (whisper--check-install-and-run nil "whisper-start")
        ;; if user is bringing their own inference engine, we at least check the command exists
        (let ((command (car (whisper-command whisper--temp-file))))
          (if (or (file-exists-p command)
                  (executable-find command))
              (whisper--record-audio)
            (error (format "Couldn't find %s in PATH, nor is it a file" command)))))))))

;;;###autoload
(defun whisper-file ()
  "Transcribe/translate local file using whisper."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'whisper-run)))

(provide 'whisper)
;;; whisper.el ends here
