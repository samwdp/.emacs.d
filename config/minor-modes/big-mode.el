;; -*- lexical-binding: t; -*-

(defvar big-font-height 300)
(defvar big-font-original-font-height)
(defvar big-font-original-font-face)
(defvar big-font-text-string "")
(defvar big-font-text-height)

(defun big-font-set-default-frame-font ()
  "Set default frame font."
  (interactive)
  (set-face-attribute 'default nil :height big-font-height))

(defun toggle-big-font ()
  "Toggle global font settings."
  (interactive)
  (big-font-mode (if big-font-mode 0 1)))

(define-minor-mode big-font-mode
  "Toggle global font settings."
  :lighter " Font"
  :global t
  (if big-font-mode
      (progn
        (setq big-font-original-font-height (face-attribute 'default :height))
        (setq big-font-original-font-face (face-attribute 'default :family))
        (big-font-set-default-frame-font))
    (set-face-attribute 'default nil :height big-font-original-font-height :font (font-spec :family big-font-original-font-face))))

(provide 'big-font-mode)
