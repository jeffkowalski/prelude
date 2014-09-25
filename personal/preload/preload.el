;;; preload.el -- emacs personalization preload file
;;; Commentary:
;;; Code:
;;;


(setq ad-redefinition-action (quote accept))


(require 'windmove)
(defun windmove-default-keybindings (&optional modifier)
    "Override keybindings for `windmove', Ignore MODIFIER."
  nil)


;; FIXME: defines to overcome warning about reference to
;; free variables in packages
;;     packages that use ido...
(defvar predicate nil)
(defvar inherit-input-method nil)
(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)
(defvar ido-context-switch-command nil)
;;     helm-swoop...
(defvar helm-swoop-last-prefix-number nil)


(provide 'preload)
;;; preload.el ends here
