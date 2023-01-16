;; grey #555556
;; red #fb4934
;; magenta #fb2874
;; voilet #d3869b
;; orange #fe8019
;; yellow #fabd2f
   ;; (yellow     '("#fabd2f" "#fabd2f" "yellow"))
   ;; (dark-green '("#689d6a" "#689d6a" "green"))
   ;; (green      '("#8ec07c" "#8ec07c" "green"))
   ;; (teal       '("#98971a" "#98971a" "green"))
   ;; (olive      '("#b8bb26" "#b8bb26" "green"))
   ;; (blue       '("#268bd2" "#2686D6" "brightblue"))
   ;; (dark-blue  '("#727280" "#727280" "blue"))
   ;; (cyan       '("#83a598" "#83a598" "brightcyan"))
   ;; (dark-cyan  '("#458588" "#458588" "cyan"))

(deftheme gruvbox-ts "Gruvbox Tree Sitter Theme")

(custom-theme-set-faces
 'gruvbox-ts
 '(default ((t (:background "#282828" :foreground "#"))))
 '(font-lock-type-face ((t (:foreground "#d3869b"))))
 '(font-lock-builtin-face ((t (:foreground "#fabd2f"))))
 '(font-lock-comment-face ((t (:foreground "#98971a"))))
 '(font-lock-constant-face ((t (:foreground "#d3869b"))))
 '(font-lock-doc-face ((t (:foreground "#98971a"))))
 '(font-lock-function-name-face ((t (:foreground "#268bd2"))))
 '(font-lock-keyword-face ((t (:foreground "#fabd2f" :weight bold))))
 '(font-lock-preprocessor-face ((t (:foreground "cornflower blue"))))
 '(font-lock-string-face ((t (:foreground "#b8bb26"))))
 '(font-lock-variable-name-face ((t (:foreground "#fe8019"))))
 '(font-lock-warning-face ((t (:foreground "hot pink" :weight bold))))

 )

(provide-theme gruvbox-ts)
