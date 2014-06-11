;;; -*- lexical-binding: t -*-
;;; kuromoji.el --- Japanese part-of-speech highlighting using Kuromoji
;;; See http://atilika.org/kuromoji/

;;; Code:

(require 'url)
(require 'json)

(defface kuromoji-noun
  `((((class color) (background light))
     (:foreground  "blue"))
    (((class color) (background dark))
     (:foreground  "blue")))
  "Face for nouns."
  :group 'kuromoji)

(defface kuromoji-verb
  `((((class color) (background light))
     (:foreground  "darkred"))
    (((class color) (background dark))
     (:foreground  "darkred")))
  "Face for verbs."
  :group 'kuromoji)

(defface kuromoji-aux-verb
  `((((class color) (background light))
     (:foreground  "darkgreen"))
    (((class color) (background dark))
     (:foreground  "darkgreen")))
  "Face for verbs."
  :group 'kuromoji)

(defface kuromoji-adverb
  `((((class color) (background light))
     (:foreground  "purple"))
    (((class color) (background dark))
     (:foreground  "purple")))
  "Face for adverbs."
  :group 'kuromoji)

(defface kuromoji-adjective
  `((((class color) (background light))
     (:foreground  "orange"))
    (((class color) (background dark))
     (:foreground  "orange")))
  "Face for adjectives."
  :group 'kuromoji)

(defface kuromoji-particle
  `((((class color) (background light))
     (:foreground  "darkgrey"))
    (((class color) (background dark))
     (:foreground  "darkgrey")))
  "Face for particles."
  :group 'kuromoji)

(defface kuromoji-punctuation
  `((((class color) (background light))
     (:foreground  "black"))
    (((class color) (background dark))
     (:foreground  "black")))
  "Face for particles."
  :group 'kuromoji)


(defvar kuromoji-pos-table '(
  ("名詞" . kuromoji-noun)
  ("動詞" . kuromoji-verb)
  ("助詞" . kuromoji-particle)
  ("副詞" . kuromoji-adverb)
  ("記号" . kuromoji-punctuation)
  ("助動詞" . kuromoji-aux-verb)
  ("形容詞" . kuromoji-adjective)))


(defvar kuromoji-url "http://atilika.org/kuromoji/rest/tokenizer/tokenize"
  "Kuromoji server URL")

(defun kuromoji-request (text)
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data (concat "text=" (url-hexify-string text))))
    (let (body)
      (with-current-buffer (url-retrieve-synchronously kuromoji-url)
        (when (eq 200 (url-http-parse-response))
          (setq body (decode-coding-string (buffer-substring-no-properties
                                            (+ 1 url-http-end-of-headers) (point-max))
                                           'utf-8))))
      (let ((json-array-type 'list)
            (json-object-type 'plist))
        (kuromoji-parse-response (json-read-from-string body))))))


(defun kuromoji-parse-response (data)
  (let ((cursor 1))
    (mapc (lambda (token)
            (let ((end (+ cursor (length (plist-get token :surface)))))
              (kuromoji-highlight
               (plist-get token :reading)
               cursor end
               (cdr (assoc (car (split-string (plist-get token :pos) ","))
                           kuromoji-pos-table)))
              ;; move cursor
              (setq cursor end)))
          (plist-get data :tokens))))

(defun kuromoji-highlight (msg beg end face)
  "Highlight text from BEG to END with FACE and help MSG."
  (let ((ovl (make-overlay beg end)))
    (overlay-put ovl 'kuromoji t)
    (overlay-put ovl 'font-lock-face face))
  (put-text-property beg end 'help-echo msg)
  (put-text-property beg end 'point-entered #'kuromoji-echo))


(defun kuromoji-echo (_old-point new-point)
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

(defun kuromoji-start ()
  (interactive)
  (kuromoji-clear)
  (kuromoji-request
   (buffer-substring-no-properties (point-min) (point-max))))

(defun kuromoji-stop ()
  (interactive)
  (kuromoji-clear))

(provide 'kuromoji)
