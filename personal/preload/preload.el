;;; preload.el -- emacs personalization preload file
;;; Commentary:
;;; Code:
;;;


(setq ad-redefinition-action (quote accept))


(require 'windmove)
(defun windmove-default-keybindings (&optional modifier)
    "Override keybindings for `windmove', Ignore MODIFIER."
  nil)


(provide 'preload)
;;; preload.el ends here
