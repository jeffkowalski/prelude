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
;;     helm-swoop...
(defvar helm-swoop-last-prefix-number nil)

;; FIXME: define-abbrev to workaround startup error
;;   Compiler-macro error for python-syntax-context: (void-function python-syntax--context-compiler-macro) [2 times]
;;   /home/jeff/.emacs.d/elpa/expand-region-20141223.1328/python-el-fgallina-expansions.el
;;   http://lists.gnu.org/archive/html/emacs-diffs/2013-04/msg00113.html
(unless (fboundp 'python-syntax--context-compiler-macro)
  (defun python-syntax--context-compiler-macro (a b c) nil))


(provide 'preload)
;;; preload.el ends here
