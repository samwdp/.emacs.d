;; -*- lexical-binding: t; -*-

(defvar sp/keys-keymap (make-keymap)
  "Keymap for my/keys-mode")

(define-minor-mode sp/keys-mode
  "Minor mode for my personal keybindings."
  :init-value t
  :global t
  :keymap sp/keys-keymap)

;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists
             `((sp/keys-mode . ,sp/keys-keymap)))

(define-key sp/keys-keymap (kbd "C-j") 'windmove-down)
(define-key sp/keys-keymap (kbd "C-h") 'windmove-left)
(define-key sp/keys-keymap (kbd "C-k") 'windmove-up)
(define-key sp/keys-keymap (kbd "C-l") 'windmove-right)
(define-key sp/keys-keymap (kbd "M-n") 'harpoon-go-to-1)
(define-key sp/keys-keymap (kbd "M-e") 'harpoon-go-to-2)
(define-key sp/keys-keymap (kbd "M-o") 'harpoon-go-to-3)
(define-key sp/keys-keymap (kbd "M-i") 'harpoon-go-to-4)
(define-key sp/keys-keymap (kbd "M-'") 'harpoon-go-to-5)

(provide 'sp/keys-mode)
