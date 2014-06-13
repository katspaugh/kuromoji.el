;;; -*- lexical-binding: t -*-
;;; kuromoji.el --- Japanese part-of-speech highlighting using Kuromoji

;;; See http://atilika.org/kuromoji/

;;; Code:

(defgroup kuromoji '() "Kuromoji")

(defcustom kuromoji-jar-path "kuromoji-0.7.7.jar" "Path to Kuromoji jar.")

(defface kuromoji-face-noun
  `((((class color) (background light))
     (:foreground  "blue"))
    (((class color) (background dark))
     (:foreground  "blue")))
  "Face for nouns."
  :group 'kuromoji)

(defface kuromoji-face-alt
  `((((class color) (background light))
     (:foreground  "darkblue"))
    (((class color) (background dark))
     (:foreground  "darkblue")))
  "Face for nouns."
  :group 'kuromoji)

(defface kuromoji-face-verb
  `((((class color) (background light))
     (:foreground  "darkred"))
    (((class color) (background dark))
     (:foreground  "darkred")))
  "Face for verbs."
  :group 'kuromoji)

(defface kuromoji-face-morpheme
  `((((class color) (background light))
     (:foreground  "darkgreen"))
    (((class color) (background dark))
     (:foreground  "darkgreen")))
  "Face for verbs."
  :group 'kuromoji)

(defface kuromoji-face-adverb
  `((((class color) (background light))
     (:foreground  "purple"))
    (((class color) (background dark))
     (:foreground  "purple")))
  "Face for adverbs."
  :group 'kuromoji)

(defface kuromoji-face-adjective
  `((((class color) (background light))
     (:foreground  "orange"))
    (((class color) (background dark))
     (:foreground  "orange")))
  "Face for adjectives."
  :group 'kuromoji)

(defface kuromoji-face-particle
  `((((class color) (background light))
     (:foreground  "darkgrey"))
    (((class color) (background dark))
     (:foreground  "darkgrey")))
  "Face for particles."
  :group 'kuromoji)

(defface kuromoji-face-punctuation
  `((((class color) (background light))
     (:foreground  "black"))
    (((class color) (background dark))
     (:foreground  "black")))
  "Face for particles."
  :group 'kuromoji)

(defvar kuromoji-pos-table '(
  ("名詞" . kuromoji-face-noun)
  ("動詞" . kuromoji-face-verb)
  ("助詞" . kuromoji-face-particle)
  ("副詞" . kuromoji-face-adverb)
  ("記号" . kuromoji-face-punctuation)
  ("助動詞" . kuromoji-face-morpheme)
  ("形容詞" . kuromoji-face-adjective)))

(defvar kuromoji-process nil "The variable to hold a single Kuromoji process.")
(defvar kuromoji-cursor 1 "Tracks highlighting position.")
(defvar kuromoji-last-face nil "Tracks highlighting face.")

(defun kuromoji-output-filter (proc output)
  (mapc
   (lambda (line)
     (when (and (< 0 (length line)) (not (string= line "Tokenizer ready.")))
       (let ((tab-sep (split-string line "\t")))
         (let ((surface (car tab-sep))
               (features (split-string (cadr tab-sep) ",")))
           (let ((pos (nth 0 features))
                 (reading (nth 7 features))
                 (start kuromoji-cursor)
                 (end (+ kuromoji-cursor (length surface))))
             (setq kuromoji-cursor end)
             (let ((face (cdr (assoc pos kuromoji-pos-table))))
               (when (eq face kuromoji-last-face)
                 (setq face 'kuromoji-face-alt))
               (setq kuromoji-last-face face)
               (kuromoji-highlight reading start end face)))))))
   (split-string output "\n"))
  (with-current-buffer (process-buffer proc)
    (insert output)))

(defun kuromoji-highlight (msg beg end face)
  "Highlight text from BEG to END with FACE and help MSG."
  (let ((ovl (make-overlay beg end)))
    (overlay-put ovl 'kuromoji t)
    (overlay-put ovl 'font-lock-face face))
  (put-text-property beg end 'help-echo msg)
  (put-text-property beg end 'point-entered #'kuromoji-echo-help))

(defun kuromoji-echo-help (_old-point new-point)
  "Called by point-motion hooks."
  (let ((msg (get-text-property new-point 'help-echo)))
    (when (and (stringp msg)
               (not (active-minibuffer-window))
               (not (current-message)))
      (message msg))))

(defun kuromoji-clear ()
  (let ((beg (point-min))
        (end (point-max)))
    (save-excursion
      (dolist (o (overlays-in beg end))
        (when (overlay-get o 'kuromoji)
          (delete-overlay o))))))

(defun kuromoji-analyze (proc)
  (setq kuromoji-cursor 1)
  (setq kuromoji-last-face nil)
  (let* ((proc-mark (process-mark proc))
         (output-start (marker-position proc-mark)))
    (process-send-region proc (point-min) (point-max))
    (process-send-string proc "\n")
    (accept-process-output proc 0.5)))

(defun kuromoji-start ()
  (interactive)
  (kuromoji-clear)
  (unless (and kuromoji-process (process-live-p kuromoji-process))
    (message "Starting Kuromoji process...")
    (setq kuromoji-process
          (let ((process-connection-type nil))
            (start-process-shell-command
             "Kuromoji" "*Kuromoji Process*"
             (concat "java -Dfile.encoding=UTF-8 -cp "
                     kuromoji-jar-path
                     " org.atilika.kuromoji.TokenizerRunner"))))
    (set-process-filter kuromoji-process #'kuromoji-output-filter))
  (kuromoji-analyze kuromoji-process))

(defun kuromoji-stop ()
  (interactive)
  (kuromoji-clear)
  (message "Kuromoji process killed.")
  (when (and kuromoji-process (process-live-p kuromoji-process))
    (let ((buf (process-buffer kuromoji-process)))
      (process-kill-without-query kuromoji-process)
      (kill-buffer buf)
      (setq kuromoji-process nil))))

(provide 'kuromoji)
