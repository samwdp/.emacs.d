(use-package pdf-tools
  :hook (pdf-view-mode . (lambda () (beacon-mode -1)))
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (when IS-WINDOWS
    (setq pdf-info-epdfinfo-program "c:/tools/epdfino/epdfinfo.exe")))

(use-package saveplace-pdf-view)
