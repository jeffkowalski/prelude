;;; preload.el -- emacs personalization preload file
;;; Commentary:
;;; Code:
;;;


(or (fboundp 'scroll-bar-mode) (defun scroll-bar-mode (&optional args) "" nil))


(setq ad-redefinition-action (quote accept))


(require 'windmove)
(defun windmove-default-keybindings (&optional modifier)
    "Override keybindings for `windmove', Ignore MODIFIER."
  nil)


(provide 'preload)
;;; preload.el ends here
