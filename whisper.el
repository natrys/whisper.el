;;; whisper.el --- Speech-to-Text interface using OpenAI's whisper model -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Imran Khan.

;; Author: Imran Khan <imran@khan.ovh>
;; URL: https://github.com/natrys/whisper.el
;; Version: 0.1.3
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

;;; User facing options

(defgroup whisper ()
  "Speech-to-text interface using OpenAI's whisper model."
  :group 'external)

(defcustom whisper-enable-speed-up nil
  "Whether to sacrifices some accuracy to speed up transcribing.

Basically whether to use \"-su\" flag in whisper.cpp.  You should experiment
enabling it to see if it works well enough for you."
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

When non-English, use generic model (without .en suffix)"
  :type 'string
  :group 'whisper)

(defcustom whisper-translate nil
  "Whether to translate to English first, before transcribing."
  :type 'boolean
  :group 'whisper)

;;; Internal variables

(defvar whisper--stdout-buffer-name "*whisper-stdout*")
(defvar whisper--stderr-buffer-name "*whisper-stderr*")
(defvar whisper--compilation-buffer-name "*whisper-compilation*")

(defvar whisper--point-buffer nil)
(defvar whisper--compilation-buffer nil)

(defvar whisper--recording-process nil)
(defvar whisper--transcribing-process nil)
(defvar whisper--marker nil)

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

(defun whisper--transcribe-command (input-file)
  "Produces whisper.cpp command to be run given location of INPUT-FILE."
  (let ((base (expand-file-name (file-name-as-directory whisper--install-path))))
    `(,(concat base "main")
      ,@(when whisper-use-threads (list "--threads" (number-to-string whisper-use-threads)))
      ,@(when whisper-enable-speed-up '("--speed-up"))
      ,@(when whisper-translate '("--translate"))
      "--language" ,whisper-language
      "--model" ,(concat base "models/" "ggml-" whisper-model ".bin")
      "--no-timestamps"
      "--file" ,input-file)))

(defun whisper--record-audio ()
  "Start audio recording process in the background."
  (with-current-buffer whisper--point-buffer
    (setq whisper--marker (point-marker)))
  (if whisper--ffmpeg-input-file
      (message "[*] Pre-processing media file")
    (message "[*] Recording audio"))
  (setq whisper--recording-process
        (make-process
         :name "whisper-recording"
         :command (whisper--record-command whisper--temp-file)
         :connection-type nil
         :buffer nil
         :sentinel (lambda (_process event)
                     (cond ((or (string-equal "finished\n" event)
                                ;; this is would be sane
                                (string-equal "terminated\n" event)
                                ;; but this is reality
                                (string-equal "exited abnormally with code 255\n" event))
                            (whisper--transcribe-audio))
                           ((string-equal "exited abnormally with code 1\n" event)
                            (error "FFmpeg command failed to record audio")))))))

(defun whisper--transcribe-audio ()
  "Start audio transcribing process in the background."
  (message "[-] Transcribing/Translating audio")
  (setq whisper--transcribing-process
        (make-process
         :name "whisper-transcribing"
         :command (whisper--transcribe-command whisper--temp-file)
         :connection-type nil
         :buffer (get-buffer-create whisper--stdout-buffer-name)
         :stderr (get-buffer-create whisper--stderr-buffer-name)
         :coding 'utf-8
         :sentinel (lambda (_process event)
                     (unwind-protect
                         (let ((whisper--stdout-buffer (get-buffer whisper--stdout-buffer-name)))
                           (with-current-buffer whisper--stdout-buffer
                             (when (string-equal "finished\n" event)
                               (goto-char (point-min))
                               (when (search-forward-regexp "." nil t)
                                 (delete-region (point-min) (point))
                                 (goto-char (point-max))
                                 (skip-chars-backward "\n")
                                 (delete-region (point) (point-max))
                                 (with-current-buffer (marker-buffer whisper--marker)
                                   (goto-char whisper--marker)
                                   (insert-buffer-substring whisper--stdout-buffer)
                                   (goto-char whisper--marker))))))
                       (set-marker whisper--marker nil)
                       (setq whisper--point-buffer nil)
                       (kill-buffer whisper--stdout-buffer-name)
                       (kill-buffer whisper--stderr-buffer-name)
                       (message nil))))))

(defun whisper--check-model-consistency ()
  "Check if chosen language and model are consistent."
  (when (and (not (string-equal "en" whisper-language))
             (string-suffix-p ".en" whisper-model))
    (error "Use generic model (non .en version) for non-English languages"))

  (unless (or (= 2 (length whisper-language))
              (string-equal "auto" whisper-language))
    (error (concat "Unknown language shortcode. For the list, see: "
                   "https://github.com/ggerganov/whisper.cpp/blob/master/whisper.cpp")))

  (let ((model-pattern (rx (seq (or "tiny" "base" "small" "medium" (seq "large" (opt "-v1")))
                                (opt (seq "." (= 2 (any "a-z"))))))))
    (unless (string-match-p model-pattern whisper-model)
      (error (concat "Speech recognition model " whisper-model " not recognised. For the list, see: "
                     "https://github.com/ggerganov/whisper.cpp/tree/master/models")))))

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

But the bigger issue I didn't think of at first is that callback model breaks
dynamic binding because call stack is disrupted. Work needs to happen when we
are coming from callbacks run from some random hook.  But then the implication
is that in such runs the free variables are no longer shadowed in dynamic scope,
so lookup fallbacks to global scope to initial non-useful values.  The messy
but easy to write workaround is to use setq-default everywhere.  That might
require reverting values in a different part of code, such spooky entanglements
are terrible although I didn't need to do that here.

Anyway, I chose that thinking passing values via function parameter is not
possible here because we are locked to this argument signature.  But that's
obviously not true, the entire callback could be defined inside a lexical scope
using something like cl-label and then inside the body the free variable would
be bound to the correct non-global value, and then they could be passed to the
downstream functions through parameters which could have done the trick."
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

      (when (and (not (file-exists-p (concat base "main")))
                 (not (string-equal "interrupt\n" status)))
        (if (yes-or-no-p (format "Inference engine whisper.cpp is not installed, install it at %s ?"
                                 whisper--install-path))
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

      (when (and (not (file-exists-p (concat base "models/ggml-" whisper-model ".bin")))
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

      (when (string-equal "interrupt\n" status)
        ;; double check to be sure before cleaning up
        (when (and (file-directory-p base) (string-suffix-p "/whisper.cpp/" base))
          (if (file-exists-p (concat base "main"))
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
- When installation is sound, starts recording audio.
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
      (whisper--check-model-consistency)
      (setq-default whisper--ffmpeg-input-file nil)
      (when (equal arg '(4))
        (when-let ((file (read-file-name "Media file: ")))
          (unless (file-readable-p file)
            (error "Media file doesn't exist or isn't readable"))
          (setq-default whisper--ffmpeg-input-file file)))
      (whisper--check-install-and-run nil "whisper-start")))))

;;;###autoload
(defun whisper-file ()
  "Transcribe/translate local file using whisper."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'whisper-run)))

(provide 'whisper)
;;; whisper.el ends here
