;;; custom.el -- emacs customization file
;;; Commentary:
;;; Code:
;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ad-redefinition-action (quote accept))
 '(ahk-syntax-directory "c:/Program Files (x86)/AutoHotkey/Extras/Editors/Syntax/")
 '(ansi-color-names-vector
   [solarized-bg red green yellow blue magenta cyan solarized-fg])
 '(anything-command-map-prefix-key "<f1>")
 '(auto-save-default nil)
 '(auto-save-list-file-prefix nil)
 '(blink-cursor-mode t)
 '(column-number-mode t)
 '(cua-keep-region-after-copy nil)
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "be7eadb2971d1057396c20e2eebaa08ec4bfd1efe9382c12917c6fe24352b7c1" "374e79a81930979e673b8e0869e135fb2450b18c6474ca145f104e0c6f003267" "baed08a10ff9393ce578c3ea3e8fd4f8c86e595463a882c55f3bd617df7e5a45" "54d1bcf3fcf758af4812f98eb53b5d767f897442753e1aa468cfeb221f8734f9" "7acc0466fce1bc967ce1561c8c4fdcbf4358b4ae692577562a3ed747c109f9d7" default)))
 '(delete-active-region (quote kill))
 '(delete-selection-mode t)
 '(display-time-mode t)
 '(doc-view-ghostscript-options
   (quote
    ("-dMaxBitmap=2147483647" "-dSAFER" "-dNOPAUSE" "-sDEVICE=png16m" "-dTextAlphaBits=4" "-dBATCH" "-dGraphicsAlphaBits=4" "-dQUIET")))
 '(doc-view-resolution 300)
 '(el-get-byte-compile nil)
 '(fci-rule-color "#383838")
 '(global-hl-line-mode t)
 '(helm-mode 1)
 '(ido-everywhere nil)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-echo-area-message "jeff")
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(kill-whole-line t)
 '(make-backup-files nil)
 '(minimap-window-location (quote right))
 '(org-agenda-window-setup (quote current-window))
 '(org-babel-load-languages (quote ((sh . t))))
 '(org-enforce-todo-dependencies t)
 '(org-habit-following-days 1)
 '(org-habit-graph-column 46)
 '(org-id-locations-file "~/Dropbox/workspace/org/.org-id-locations")
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-habit org-irc org-mhe org-rmail org-w3m)))
 '(org-support-shift-select (quote always))
 '(org-toodledo-password "czd8Vbj0aKoH")
 '(org-toodledo-preserve-drawers t)
 '(org-toodledo-sync-on-save "yes")
 '(org-toodledo-userid "td45445ecf93551")
 '(password-cache-expiry 900)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(size-indication-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-ff-directory ((t (:foreground "deep sky blue"))))
 '(helm-ff-file ((t (:foreground "gainsboro"))))
 '(helm-ff-symlink ((t (:foreground "cyan"))))
 '(highlight ((t (:background "black"))))
 '(org-agenda-current-time ((t (:inherit org-time-grid :background "dim gray"))) t)
 '(org-agenda-done ((t (:foreground "dim gray"))))
 '(org-scheduled-previously ((t (:foreground "#bc8383"))))
 '(org-warning ((t (:foreground "#cc9393" :weight bold))))
 '(region ((t (:background "dim gray")))))

(provide 'custom)
;;; custom.el ends here
