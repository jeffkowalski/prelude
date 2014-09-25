;;; custom.el -- emacs customization file
;;; Commentary:
;;; Code:
;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes t))
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
