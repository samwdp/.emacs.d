(defvar +lsp--default-read-process-output-max nil)
(defvar +lsp--default-gcmh-high-cons-threshold nil)
(defvar +lsp--optimization-init-p nil)

(define-minor-mode +lsp-optimization-mode
  "Deploys universal GC and IPC optimizations for `lsp-mode' and `eglot'."
  :global t
  :init-value nil
  (if (not +lsp-optimization-mode)
      (setq-default read-process-output-max +lsp--default-read-process-output-max
                    gcmh-high-cons-threshold +lsp--default-gcmh-high-cons-threshold
                    +lsp--optimization-init-p nil)
    ;; Only apply these settings once!
    (unless +lsp--optimization-init-p
      (setq +lsp--default-read-process-output-max
            ;; DEPRECATED Remove check when 26 support is dropped
            (if (boundp 'read-process-output-max)
                (default-value 'read-process-output-max))
            +lsp--default-gcmh-high-cons-threshold
            (default-value 'gcmh-high-cons-threshold))
      ;; `read-process-output-max' is only available on recent development
      ;; builds of Emacs 27 and above.
      (setq-default read-process-output-max (* 1024 1024))
      ;; REVIEW LSP causes a lot of allocations, with or without Emacs 27+'s
      ;;        native JSON library, so we up the GC threshold to stave off
      ;;        GC-induced slowdowns/freezes. Doom uses `gcmh' to enforce its
      ;;        GC strategy, so we modify its variables rather than
      ;;        `gc-cons-threshold' directly.
      (setq-default gcmh-high-cons-threshold (* 2 +lsp--default-gcmh-high-cons-threshold))
      (gcmh-set-high-threshold)
      (setq +lsp--optimization-init-p t))))

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
