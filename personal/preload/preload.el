;;; preload.el -- emacs personalization preload file
;;; Commentary:
;;; Code:
;;;

(or (fboundp 'scroll-bar-mode) (defun scroll-bar-mode (&optional args) "" nil))

(setq ad-redefinition-action (quote accept))

(eval-after-load "windmove"
  '(defun windmove-default-keybindings (&optional modifier)
    "Override keybindings for `windmove', Ignore MODIFIER."
    (interactive)
    nil))

(customize-set-variable 'prelude-minimalistic-ui t)

(provide 'preload)
;;; preload.el ends here
