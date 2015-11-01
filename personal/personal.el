;;; personal.el -- emacs personalization file
;;; Commentary:
;;; Code:
;;;

;; Set repositories

(when (>= emacs-major-version 24)
  (setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                           ("org" . "http://orgmode.org/elpa/")
                           ("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.org/packages/")
                           ("melpa-stable" . "http://stable.melpa.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           )))

;; Setup use-package

(dolist (p '(use-package
             ))
  (unless (package-installed-p p)
    (package-install p)))

(require 'use-package)
(use-package use-package
  :config (setq use-package-verbose t
                use-package-minimum-reported-time 0))
(use-package req-package
  :config (progn (setq req-package-log-level 'trace)
                 (req-package--log-set-level req-package-log-level)))

;; Setup el-get first

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(when (not (require 'el-get))
  (req-package-force el-get))

(progn
  (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/el-get/recipes")
  (setq el-get-sources '(
                         (:name evernote-mode
                                :description "Functions for editing Evernote notes directly from Emacs"
                                :type github
                                :pkgname "jeffkowalski/evernote-mode"
                                :features evernote-mode)
                         (:name nyan-mode
                                :description "Nyan Cat for Emacs! Nyanyanyanyanyanyanyanyanyan!"
                                :type github
                                :pkgname "jeffkowalski/nyan-mode"
                                :features nyan-mode)
                         (:name org-cua-dwim
                                :description "Org-mode and CUA-mode compatibility layer"
                                :type github
                                :pkgname "jeffkowalski/org-cua-dwim"
                                :features org-cua-dwim)
                         (:name org-ehtml
                                :description "Export Org-mode files as editable web pages"
                                :type github
                                :pkgname "jeffkowalski/org-ehtml"
                                :load-path "src")
                         (:name org-reveal
                                :description "Exports Org-mode contents to Reveal.js HTML presentation"
                                :type github
                                :pkgname "jeffkowalski/org-reveal"
                                :features ox-reveal)
                         ))
  (defun req-package-providers-present-el-get-local (package)
    "Return t if PACKAGE is available in el-get-sources."
    (memq package (mapcar (lambda (x) (plist-get x :name)) el-get-sources)))
  (puthash 'el-get-local '(req-package-providers-install-el-get
                           req-package-providers-present-el-get-local)
           req-package-providers-map))
(el-get 'sync)

;; Override function defined in use-package, so that packages from el-get are considered as well as those from the package manager.

(defun use-package-ensure-elpa (package &optional no-refresh)
  (if (or (package-installed-p package) (el-get-package-exists-p package))
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (use-package-ensure-elpa package t)))))

;; Enable sorting on all columns in package menu's tabular list.
;; Note my naive mapping removes the final properties (like :right-align) if present.

(add-hook 'package-menu-mode-hook
          (lambda () (setq tabulated-list-format
                             (vconcat (mapcar (lambda (arg) (list (nth 0 arg) (nth 1 arg)
                                                            (or (nth 2 arg) t)))
                                       tabulated-list-format)))))

(define-key package-menu-mode-map "o" 'delete-other-windows)

;; package-utils and upgrades

(req-package package-utils
  :init
  (defun U nil
    (interactive)
    "Upgrade all packages"
    (package-utils-upgrade-all)
    (el-get-update-all t)
    (message "upgrade complete"))
)

;; ----------------------------------------------------------- [ cua ]

(req-package cua-base
  :init (cua-mode t)
  :config (setq cua-keep-region-after-copy nil))

;; FIXME workaround problem in CUA which doesn't seem to obey delete-selection behavior on paste

(defadvice cua-paste (before clobber-region (&optional arg))
  "Delete the region before pasting."
  (when (region-active-p) (delete-region (region-beginning) (region-end))))
(ad-activate 'cua-paste)

;; ----------------------------------------------------------- [ adornments ]

;; off
(scroll-bar-mode -1)
;;(horizontal-scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
;; on
(blink-cursor-mode t)
(column-number-mode t)
(size-indication-mode t)
(global-hl-line-mode t)
(show-paren-mode t)
(display-time)
(set-default 'cursor-type '(bar . 2))

(setq frame-title-format '(buffer-file-name "emacs - %f %*" ("%b %*"))
      icon-title-format  '(buffer-file-name "emacs - %f %*" ("%b %*"))
      indicate-empty-lines t
      inhibit-startup-echo-area-message "jeff"
      inhibit-startup-screen t
      initial-scratch-message nil
      show-trailing-whitespace t
      indent-tabs-mode nil
      redisplay-dont-pause t)

;; ----------------------------------------------------------- [ miscellaneous ]

;; Enable all commands
(setq disabled-command-function nil)

(auto-revert-mode)

(setq
 auto-save-list-file-prefix nil ;; startup
 auto-save-default nil ;; files
 kill-whole-line t ;; simple
 make-backup-files nil ;; files
 help-window-select t ;; help
 enable-recursive-minibuffers t
 password-cache-expiry 900) ;; password-cache

 ;; hide trailing whitespaces in some programming modes:
 (mapc (lambda (hook)
         (add-hook hook (lambda ()
                          (setq show-trailing-whitespace nil))))
       '(eshell-mode-hook term-mode-hook))

;; compile

(req-package compile
  :bind (("<f5>" . recompile)))

;; cperl mode

(req-package cperl-mode
  :ensure t
  :init (defalias 'perl-mode 'cperl-mode))

;; clang-format mode

(req-package clang-format
  :bind (("C-M-\\" . clang-format-buffer))
  :config (setq clang-format-executable "clang-format-3.8"))

;; fish mode

(req-package fish-mode)

;; make mode

(req-package make-mode
  ;; re-tabbing during whitespace-cleanup would kill makefiles
  :config (add-hook 'makefile-mode-hook
                    (lambda () (remove-hook 'before-save-hook 'whitespace-cleanup t))))

;; doc view

(req-package doc-view
  :config (setq doc-view-ghostscript-options
                '("-dMaxBitmap=2147483647" "-dSAFER" "-dNOPAUSE" "-sDEVICE=png16m" "-dTextAlphaBits=4" "-dBATCH" "-dGraphicsAlphaBits=4" "-dQUIET")
                doc-view-resolution 300))

;; ----------------------------------------------------------- [ emacs prelude ]

(req-package prelude-mode
  :diminish (prelude-mode . " π")
  :defines (prelude-mode-map)
  :init (progn
          ;; fix keyboard behavior on terminals that send ^[O{ABCD} for arrows
          (defvar ALT-O-map (make-sparse-keymap) "ALT-O keymap.")
          (define-key prelude-mode-map (kbd "M-O") ALT-O-map)))

(req-package prelude-programming
  :init (add-hook 'prelude-prog-mode-hook
                  (lambda ()
                    (guru-mode -1)
                    (whitespace-mode -1)) t))

;; ----------------------------------------------------------- [ hydra ]

(req-package hydra
  :require (windmove ace-window)
  :init (progn
          (global-set-key
           (kbd "C-M-o")
           (defhydra hydra-window ()
             "window"
             ("<left>" windmove-left "left")
             ("<down>" windmove-down "down")
             ("<up>" windmove-up "up")
             ("<right>" windmove-right "right")
             ("a" (lambda ()
                    (interactive)
                    (ace-window 1)
                    (add-hook 'ace-window-end-once-hook
                              'hydra-window/body))
              "ace")
             ("v" (lambda ()
                    (interactive)
                    (split-window-right)
                    (windmove-right))
              "vert")
             ("x" (lambda ()
                    (interactive)
                    (split-window-below)
                    (windmove-down))
              "horz")
             ("s" (lambda ()
                    (interactive)
                    (ace-window 4)
                    (add-hook 'ace-window-end-once-hook
                              'hydra-window/body))
              "swap")
             ("d" (lambda ()
                    (interactive)
                    (ace-window 16)
                    (add-hook 'ace-window-end-once-hook
                              'hydra-window/body))
              "del")
             ("o" delete-other-windows "1" :color blue)
             ("i" ace-maximize-window "a1" :color blue)
             ("q" nil "cancel")))))

;; ----------------------------------------------------------- [ keyboard macros ]

(defvar defining-key)

(defun end-define-macro-key nil
  "Ends the current macro definition."
  (interactive)
  (end-kbd-macro nil)
  (global-set-key defining-key last-kbd-macro)
  (global-set-key [f8] 'define-macro-key))

(defun define-macro-key (key)
  "Bind a set of keystrokes to a single KEY."
  (interactive "kKey to define: ")
  (setq defining-key key)
  (global-set-key [f8] 'end-define-macro-key)
  (start-kbd-macro nil))

(global-set-key (kbd "<f8>")            'define-macro-key)

;; ----------------------------------------------------------- [ smartparens ]

(req-package smartparens
  :diminish " Φ"
  :config (progn (define-key smartparens-strict-mode-map (kbd "M-<delete>")    'sp-unwrap-sexp)
                 (define-key smartparens-strict-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)))

;; ----------------------------------------------------------- [ registers ]
;; Registers allow you to jump to a file or other location quickly.
;; To jump to a register, use C-x r j followed by the letter of the register.

(mapc
 (lambda (r)
   (set-register (car r) (cons 'file (cdr r))))
 '((?p . "~/.emacs.d/personal/personal.org")
   (?i . "~/Dropbox/sync-linux/installation.txt")
   (?c . "~/.emacs.d/personal/custom.el")
   (?f . "~/.config/fish/config.fish")
   (?m . "~/Dropbox/sync-linux/mac_addrs.org")
   (?z . "~/.zshrc")
   (?s . "~/bin/sauron.rb")))

;; ----------------------------------------------------------- [ shell / eshell ]

(add-hook 'emacs-startup-hook
          (lambda ()
              (let ((default-directory (getenv "HOME")))
                (command-execute 'eshell)
                (bury-buffer))))

;; (add-hook 'eshell-mode-hook
;;           (lambda ()
;;               (define-key eshell-mode-map
;;                 [remap pcomplete]
;;                 'helm-esh-pcomplete)))
;; (add-hook 'eshell-mode-hook
;;           (lambda ()
;;               (define-key eshell-mode-map
;;                 (kbd "M-p")
;;                 'helm-eshell-history)))

;; ----------------------------------------------------------- [ multi-term ]

(req-package multi-term
  :bind* (("C-c t" . multi-term-dedicated-toggle))
  :config (progn (setq multi-term-dedicated-close-back-to-open-buffer-p t
                       multi-term-dedicated-select-after-open-p t
                       multi-term-program-switches "--login")
                 (bind-key "C-c t" 'multi-term-dedicated-toggle prelude-mode-map)))

;; ----------------------------------------------------------- [ undo-tree ]

(req-package undo-tree
  :diminish " τ"
  :bind* (("C-z" . undo-tree-undo))
  :init (progn
          (global-undo-tree-mode)))

;; ----------------------------------------------------------- [ image+ ]

(req-package image+
  :init (progn
          (imagex-global-sticky-mode)
          (imagex-auto-adjust-mode)
          (let ((map imagex-sticky-mode-map))
            (define-key map "+" 'imagex-sticky-zoom-in)
            (define-key map "-" 'imagex-sticky-zoom-out)
            (define-key map "l" 'imagex-sticky-rotate-left)
            (define-key map "r" 'imagex-sticky-rotate-right)
            (define-key map "m" 'imagex-sticky-maximize)
            (define-key map "o" 'imagex-sticky-restore-original)
            (define-key map "\C-x\C-s" 'imagex-sticky-save-image))))

;; ----------------------------------------------------------- [ cmake ]

(req-package cmake-mode
  :config (add-hook 'cmake-mode-hook
                    (lambda () (setq cmake-tab-width 4))))

;; ----------------------------------------------------------- [ dired ]

(req-package dired-single
  :require (dired dired+)
  :config (progn
            (setq-default auto-revert-interval 1)
            (setq-default dired-omit-files-p t)
            (setq font-lock-maximum-decoration (quote ((dired-mode) (t . t)))
                  dired-omit-files (concat dired-omit-files "\\."))
            (define-key dired-mode-map [return] 'dired-single-buffer)
            (define-key dired-mode-map [down-mouse-1] 'dired-single-buffer-mouse)
            (define-key dired-mode-map [^]
              (lambda ()
                (interactive)
                (dired-single-buffer "..")))))

;; ----------------------------------------------------------- [ helm ]

(req-package helm
  :diminish " H"
  :init (helm-mode 1)
  :bind (("C-x C-f" . helm-find-files)
         ("M-x"     . helm-M-x)
         ("C-x b"   . helm-buffers-list)
         ("C-M-g"   . helm-do-grep))
  :config (progn
            (helm-adaptive-mode t)
            (defun jeff/find-file-as-root ()
              "Like 'helm-find-file', but automatically edit the file with root-privileges (using tramp/sudo), if the file is not writable by user."
              (interactive)
              (let ((file (helm-read-file-name "Edit as root: ")))
                (unless (file-writable-p file)
                  (setq file (concat "/sudo:root@localhost:" file)))
                (find-file file)))
            (global-set-key (kbd "C-x F") 'jeff/find-file-as-root)))

;; FIXME workaround problem in select-frame-set-input-focus
;;   select-frame-set-input-focus(#<frame *Minibuf-1* * 0x6a44268>)
;;   helm-frame-or-window-configuration(restore)
;;   helm-cleanup()
;;   ...
;;   helm-internal(...)
;;   ...
;; which throws error "progn: Not an in-range integer, float, or cons of integers"

(defun select-frame-set-input-focus (frame &optional norecord)
  "Select FRAME, raise it, and set input focus, if possible.
If `mouse-autoselect-window' is non-nil, also move mouse pointer
to FRAME's selected window.  Otherwise, if `focus-follows-mouse'
is non-nil, move mouse cursor to FRAME.

Optional argument NORECORD means to neither change the order of
recently selected windows nor the buffer list."
  (select-frame frame norecord)
  (raise-frame frame)

  ;; Ensure, if possible, that FRAME gets input focus.
  ;; (when (memq (window-system frame) '(x w32 ns))
  ;;    (x-focus-frame frame))

  ;; Move mouse cursor if necessary.
  (cond
   (mouse-autoselect-window
    (let ((edges (window-inside-edges (frame-selected-window frame))))
      ;; Move mouse cursor into FRAME's selected window to avoid that
      ;; Emacs mouse-autoselects another window.
      (set-mouse-position frame (nth 2 edges) (nth 1 edges))))
   (focus-follows-mouse
    ;; Move mouse cursor into FRAME to avoid that another frame gets
    ;; selected by the window manager.
    (set-mouse-position frame (1- (frame-width frame)) 0))))

;; helm-swoop

(req-package helm-swoop
  :require helm
  :defines (helm-swoop-last-prefix-number)
  :bind (("M-i" . helm-swoop)))

;; ----------------------------------------------------------- [ time ]

(req-package time
  :init (progn
          (setq display-time-world-list '(("America/Los_Angeles" "Berkeley")
                                          ("America/New_York" "New York")
                                          ("UTC" "UTC")
                                          ("Europe/London" "London")
                                          ("Asia/Calcutta" "India")
                                          ("Asia/Shanghai" "China")))
          (global-set-key (kbd "<f9> C") 'helm-world-time)))

;; ----------------------------------------------------------- [ sunshine ]

(req-package sunshine
  :init (progn
          (setq sunshine-location "Berkeley, California")
          (setq sunshine-show-icons t)
          (setq sunshine-units 'imperial)
          (global-set-key (kbd "<f9> w") 'sunshine-forecast)
          (global-set-key (kbd "<f9> W") 'sunshine-quick-forecast)
          ))

;; ----------------------------------------------------------- [ company ]

(req-package company
  :diminish " Ψ"
  :config (progn
            (setq company-auto-complete t
                  company-idle-delay 0.5)
            (add-to-list 'company-backends 'company-dabbrev t)
            (add-to-list 'company-backends 'company-ispell t)
            (add-to-list 'company-backends 'company-files t)
            (add-to-list 'company-transformers 'company-sort-by-occurrence)))

(defun my-pcomplete-capf ()
  "Org-mode completions."
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
(add-hook 'org-mode-hook 'my-pcomplete-capf)

;; ----------------------------------------------------------- [ tramp ]

;; disable version control checks
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; ----------------------------------------------------------- [ ido ]

(req-package ido
  :config (progn
          (setq ido-everywhere nil)
          (add-hook 'ido-minibuffer-setup-hook
                    (lambda ()
                      ;; Locally disable 'truncate-lines'
                      (set (make-local-variable 'truncate-lines) nil)))
          (add-hook 'ido-setup-hook
                    (lambda ()
                      ;; Display ido results vertically, rather than horizontally:
                      (setq ido-decorations (quote ("\n-> "
                                                    ""
                                                    "\n   "
                                                    "\n   ..."
                                                    "[" "]"
                                                    " [No match]"
                                                    " [Matched]"
                                                    " [Not readable]"
                                                    " [Too big]"
                                                    " [Confirm]")))
                      ;;eg. allows "bgorg" to match file "begin.org"
                      (setq ido-enable-flex-matching t)
                      (define-key ido-completion-map (kbd "<up>")   'ido-prev-match)
                      (define-key ido-completion-map (kbd "<down>") 'ido-next-match)))))

;; ----------------------------------------------------------- [ magit ]

(req-package magit
  :diminish "ma"
  :init (setq magit-diff-options '("--ignore-all-space"))) ; ignore whitespace

;; ----------------------------------------------------------- [ ibuffer ]

;; *Nice* buffer switching
(req-package ibuffer
  :require ibuf-ext
  :bind ("C-x C-b" . ibuffer)
  :config (progn
            (setq ibuffer-show-empty-filter-groups nil)
            (setq ibuffer-saved-filter-groups
                  '(("default"
                     ("version control" (or (mode . svn-status-mode)
                                            (mode . svn-log-edit-mode)
                                            (mode . magit-mode)
                                            (mode . magit-status-mode)
                                            (mode . magit-commit-mode)
                                            (mode . magit-log-edit-mode)
                                            (mode . magit-log-mode)
                                            (mode . magit-reflog-mode)
                                            (mode . magit-stash-mode)
                                            (mode . magit-diff-mode)
                                            (mode . magit-wazzup-mode)
                                            (mode . magit-branch-manager-mode)
                                            (name . "^\\*svn-")
                                            (name . "^\\*vc\\*$")
                                            (name . "^\\*Annotate")
                                            (name . "^\\*git-")
                                            (name . "^\\*magit")
                                            (name . "^\\*vc-")))
                     ("emacs" (or (name . "^\\*scratch\\*$")
                                  (name . "^\\*Messages\\*$")
                                  (name . "^\\*Warnings\\*$")
                                  (name . "^TAGS\\(<[0-9]+>\\)?$")
                                  (mode . help-mode)
                                  (mode . package-menu-mode)
                                  (name . "^\\*Apropos\\*$")
                                  (name . "^\\*info\\*$")
                                  (name . "^\\*Occur\\*$")
                                  (name . "^\\*grep\\*$")
                                  (name . "^\\*Compile-Log\\*$")
                                  (name . "^\\*Backtrace\\*$")
                                  (name . "^\\*Process List\\*$")
                                  (name . "^\\*gud\\*$")
                                  (name . "^\\*Man")
                                  (name . "^\\*WoMan")
                                  (name . "^\\*Kill Ring\\*$")
                                  (name . "^\\*Completions\\*$")
                                  (name . "^\\*tramp")
                                  (name . "^\\*Shell Command Output\\*$")
                                  (name . "^\\*Evernote-Client-Output\\*$")
                                  (name . "^\\*compilation\\*$")))
                     ("helm" (or (mode . helm-mode)
                                 (name . "^\\*helm[- ]")
                                 (name . "^\\*Debug Helm Log\\*$")))
                     ("shell" (or (name . "^\\*shell\\*$")
                                  (name . "^\\*ansi-term\\*$")
                                  (name . "^\\*terminal<\d+>\\*$")
                                  (name . "^\\*eshell\\*$")))
                     ("evernote" (or (mode . evernote-browsing-mode)))
                     ("emacs source" (or (mode . emacs-lisp-mode)
                                         (filename . "/Applications/Emacs.app")
                                         (filename . "/bin/emacs")))
                     ("agenda" (or (name . "^\\*Calendar\\*$")
                                   (name . "^diary$")
                                   (name . "^\\*Agenda")
                                   (name . "^\\*org-")
                                   (name . "^\\*Org")
                                   (mode . org-mode)
                                   (mode . muse-mode)))
                     ("latex" (or (mode . latex-mode)
                                  (mode . LaTeX-mode)
                                  (mode . bibtex-mode)
                                  (mode . reftex-mode)))
                     ("dired" (or (mode . dired-mode))))))
            (add-hook 'ibuffer-hook (lambda () (ibuffer-switch-to-saved-filter-groups "default")))))

(defadvice ibuffer-generate-filter-groups (after reverse-ibuffer-groups () activate)
  "Order ibuffer filter groups so the order is : [Default], [agenda], [Emacs]."
  (setq ad-return-value (nreverse ad-return-value)))

;; ----------------------------------------------------------- [ ace-window ]

(req-package ace-window
  :config '(setq aw-scope 'frame))

;; ----------------------------------------------------------- [ org ]

(req-package org
  :diminish "Ο"
  :pin gnu
  :loader 'elpa
  ;; NOTE: org must be manually installed from elpa / gnu since it's
  ;; require'd from init.el in order to tangle personal.org

  :init
  (setq org-directory "~/Dropbox/workspace/org/"
        ;;org-replace-disputed-keys t ; org-CUA-compatible
        org-log-into-drawer t
        org-support-shift-select 'always
        org-default-notes-file (concat org-directory "refile.org")
        org-agenda-files (list (concat org-directory "tasks.org")
                               (concat org-directory "sauron.org")
                               (concat org-directory "gcal.org"))
        org-modules '(org-bbdb org-bibtex org-docview org-gnus org-info org-habit org-irc org-mhe org-rmail org-w3m)
        org-startup-indented t
        org-enforce-todo-dependencies t
        org-src-window-setup 'current-window
        org-babel-load-languages '((sh . t)))
  :config
  (progn
    (defun jeff/org-add-ids-to-headlines-in-file ()
      "Add ID properties to all headlines in the current file which do not already have one."
      (interactive)
      (org-map-entries 'org-id-get-create))
    ;; (add-hook 'org-mode-hook
    ;;           (lambda ()
    ;;             (add-hook 'before-save-hook 'jeff/org-add-ids-to-headlines-in-file nil 'local)))

    (defun org-check-misformatted-subtree ()
      "Check misformatted entries in the current buffer."
      (interactive)
      (show-all)
      (org-map-entries
       (lambda ()
         (when (and (move-beginning-of-line 2)
                    (not (looking-at org-heading-regexp)))
           (if (or (and (org-get-scheduled-time (point))
                        (not (looking-at (concat "^.*" org-scheduled-regexp))))
                   (and (org-get-deadline-time (point))
                        (not (looking-at (concat "^.*" org-deadline-regexp)))))
               (when (y-or-n-p "Fix this subtree? ")
                 (message "Call the function again when you're done fixing this subtree.")
                 (recursive-edit))
             (message "All subtrees checked.")))))))

  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)))

;; org habit

(req-package org-habit
  :require org
  :init (setq org-habit-following-days 1
              org-habit-graph-column 46))

;; htmlize

(req-package htmlize)

;; org agenda

(defun my-org-cmp-tag (a b)
  "Compare the non-context tags of A and B."
  (let ((ta (car (get-text-property 1 'tags a)))
        (tb (car (get-text-property 1 'tags b))))
    (cond ((and (not ta) (not tb)) nil)
          ((not ta) -1)
          ((not tb) +1)
          ;;((string-match-p "^@" tb) -1)
          ;;((string-match-p "^@" ta) +1)
          ((string-lessp ta tb) -1)
          ((string-lessp tb ta) +1)
          (t nil))))

(req-package org-agenda
  :require (org htmlize)
  :init (progn (setq org-agenda-tags-column -97
                     org-agenda-block-separator (let ((retval ""))
                                                  (dotimes (i (- org-agenda-tags-column)) (setq retval (concat retval "=")))
                                                  retval)
                     org-agenda-timegrid-use-ampm t
                     org-agenda-time-grid '((daily weekly today require-timed remove-match)
                                            #("----------------" 0 16 (org-heading t))
                                            (800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000))
                     org-agenda-search-headline-for-time nil
                     org-agenda-window-setup 'current-window
                     org-agenda-log-mode-items '(clock closed state)
                     org-agenda-dim-blocked-tasks nil ; much faster!
                     org-agenda-use-tag-inheritance nil
                     org-agenda-exporter-settings
                     '(
                       ;;(org-agenda-add-entry-text-maxlines 50)
                       ;;(org-agenda-with-colors nil)
                       (org-agenda-write-buffer-name "Agenda")
                       ;;(ps-number-of-columns 2)
                       (ps-landscape-mode nil)
                       (ps-print-color-p (quote black-white))
                       (htmlize-output-type (quote css)))

                     org-agenda-custom-commands
                     '(("d" "Timeline for today" ((agenda "" ))
                        ((org-agenda-ndays 1)
                         (org-agenda-show-log t)
                         (org-agenda-log-mode-items '(clock closed state))
                         (org-agenda-clockreport-mode t)
                         (org-agenda-entry-types '())))

                       ("s" "Startup View"
                        ((agenda ""    ((org-agenda-ndays 3)
                                        (org-agenda-start-on-weekday nil)
                                        ;;(org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                        (org-agenda-skip-scheduled-if-deadline-is-shown t)
                                        (org-agenda-prefix-format "  %-10T %t")
                                        (org-agenda-hide-tags-regexp "^@")
                                        (org-agenda-cmp-user-defined 'my-org-cmp-tag)
                                        (org-agenda-sorting-strategy '(time-up todo-state-down habit-up tag-up priority-down user-defined-up alpha-up))
                                        ;;(org-agenda-todo-ignore-scheduled 'future)
                                        (org-deadline-warning-days 0)))
                         (agenda "TODO" ((org-agenda-time-grid nil)
                                         (org-deadline-warning-days 365)
                                         (org-agenda-prefix-format "  %-10T %s")
                                         (org-agenda-hide-tags-regexp "^@")
                                         (org-agenda-entry-types '(:deadline))
                                         (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
                                         (org-agenda-start-on-weekday nil)
                                         (org-agenda-ndays 1)
                                         (org-agenda-overriding-header "Unscheduled upcoming deadlines:")))
                         (todo "TODO"   ((org-agenda-time-grid nil)
                                         (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "#[A-C]" 'scheduled 'deadline))
                                         ;;(org-agenda-todo-keyword-format "")
                                         (org-agenda-prefix-format "  %-10T %t")
                                         (org-agenda-hide-tags-regexp "^@")
                                         ;;(org-agenda-show-inherited-tags nil)
                                         (org-agenda-cmp-user-defined 'my-org-cmp-tag)
                                         (org-agenda-sorting-strategy '(priority-down tag-up user-defined-up alpha-up))
                                         (org-agenda-overriding-header "Unscheduled, no deadline:")))
                         (todo "TODO"   ((org-agenda-time-grid nil)
                                         (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp "#[A-C]" 'scheduled 'deadline))
                                         ;;(org-agenda-todo-keyword-format "")
                                         (org-agenda-prefix-format "  %-10T %t")
                                         (org-agenda-hide-tags-regexp "^@")
                                         ;;(org-agenda-show-inherited-tags nil)
                                         (org-agenda-cmp-user-defined 'my-org-cmp-tag)
                                         (org-agenda-sorting-strategy '(priority-down tag-up user-defined-up alpha-up))
                                         (org-agenda-overriding-header "Someday:")))))))
               (add-hook 'org-finalize-agenda-hook
                         (lambda () (remove-text-properties
                                     (point-min) (point-max) '(mouse-face t))))
               (add-hook 'org-agenda-mode-hook
                         (lambda () (whitespace-mode -1)) t)

               (defun jeff/org-agenda-edit-headline ()
                 "Go to the Org-mode file containing the item at point, then mark headline for overwriting."
                 (interactive)
                 (org-agenda-goto)
                 (search-backward (org-get-heading t t))
                 (push-mark)
                 (goto-char (match-end 0))
                 (activate-mark))
               (define-key org-agenda-mode-map (kbd "h") 'jeff/org-agenda-edit-headline)

               ;; Remove from agenda time grid lines that are in an appointment The
               ;; agenda shows lines for the time grid. Some people think that these
               ;; lines are a distraction when there are appointments at those
               ;; times. You can get rid of the lines which coincide exactly with the
               ;; beginning of an appointment. Michael Ekstrand has written a piece of
               ;; advice that also removes lines that are somewhere inside an
               ;; appointment: see [[http://orgmode.org/worg/org-hacks.html][Org-hacks]]
               (defun org-time-to-minutes (time)
                 "Convert an HHMM time to minutes"
                 (+ (* (/ time 100) 60) (% time 100)))

               (defun org-time-from-minutes (minutes)
                 "Convert a number of minutes to an HHMM time"
                 (+ (* (/ minutes 60) 100) (% minutes 60)))

               (defun org-extract-window (line)
                 "Extract start and end times from org entries"
                (let ((start (get-text-property 1 'time-of-day line))
                      (dur (get-text-property 1 'duration line)))
                  (cond
                   ((and start dur)
                    (cons start
                          (org-time-from-minutes
                           (+ dur (org-time-to-minutes start)))))
                   (start start)
                   (t nil))))

               (defadvice org-agenda-add-time-grid-maybe (around mde-org-agenda-grid-tweakify
                                                                 (list ndays todayp))
                 (if (member 'remove-match (car org-agenda-time-grid))
                     (let* ((windows (delq nil (mapcar 'org-extract-window list)))
                            (org-agenda-time-grid
                             (list (car org-agenda-time-grid)
                                   (cadr org-agenda-time-grid)
                                   (remove-if
                                    (lambda (time)
                                      (find-if (lambda (w)
                                                 (if (numberp w)
                                                     (equal w time)
                                                   (and (>= time (car w))
                                                        (< time (cdr w)))))
                                               windows))
                                    (caddr org-agenda-time-grid)))))
                       ad-do-it)
                   ad-do-it))

               (ad-activate 'org-agenda-add-time-grid-maybe)

               ;; (defun kiwon/org-agenda-redo-in-other-window ()
               ;;   "Call org-agenda-redo function even in the non-agenda buffer."
               ;;   (interactive)
               ;;   (let ((agenda-window (get-buffer-window org-agenda-buffer-name t)))
               ;;     (when agenda-window
               ;;       (with-selected-window agenda-window (org-agenda-redo)))))
               ;;(run-at-time nil 60 'kiwon/org-agenda-redo-in-other-window)
               ))

;; org clock

(req-package org-clock
  :require org
  :init (progn
          (setq org-clock-into-drawer t)
          (defun jeff/org-mode-ask-effort ()
            "Ask for an effort estimate when clocking in."
            (unless (org-entry-get (point) "Effort")
              (let ((effort
                     (completing-read
                      "Effort: "
                      (org-entry-get-multivalued-property (point) "Effort"))))
                (unless (equal effort "")
                  (org-set-property "Effort" effort)))))
          (add-hook 'org-clock-in-prepare-hook 'jeff/org-mode-ask-effort)))

;; org capture

(req-package org-protocol
  :require org)

(defun adjust-captured-headline (hl)
  "Fixup headlines for amazon orders"
  (if (string-match "amazon\\.com order of \\(.+?\\)\\(\\.\\.\\.\\)?\\( has shipped!\\)? :" hl)
      (let ((item (match-string 1 hl)))
        (cond ((string-match ":@quicken:" hl) (concat "order of " item " :amazon_visa:@quicken:"))
              ((string-match ":@waiting:" hl) (concat "delivery of " item " :amazon:@waiting:"))
              (t hl))
        )
    hl)
  )

(req-package org-capture
  :require (org org-protocol s)
  :init (setq org-capture-templates
              (quote (("b" "entry.html" entry (file+headline (concat org-directory "tasks.org") "TASKS")
                       "* TODO %:description\n%:initial\n" :immediate-finish t)
                      ("t" "todo" entry (file+headline (concat org-directory "tasks.org") "TASKS")
                       "* TODO [#C] %?\n")
                      ;; capture this bookmarklet
                      ;; javascript:capture('@agendas');function enc(s){return encodeURIComponent(typeof(s)=="string"?s.toLowerCase().replace(/"/g, "'"):s);};function capture(context){var re=new RegExp(/(.*) - \S+@gmail.com/);var m=re.exec(document.title);var t=m?m[1]:document.title;javascript:location.href='org-protocol://capture://w/'+encodeURIComponent(location.href)+'/'+enc(t)+' :'+context+':/'+enc(window.getSelection());}
                      ("w" "org-protocol" entry (file+headline (concat org-directory "tasks.org") "TASKS")
                       "* TODO [#C] %?%(adjust-captured-headline \"%:description\")\nSCHEDULED: %t\n:PROPERTIES:\n:END:\n%:link\n%:initial\n")
                      ("h" "Habit" entry (file+headline (concat org-directory "tasks.org") "TASKS")
                       "* TODO [#C] %?\nSCHEDULED: %(s-replace \">\" \" .+1d/3d>\" \"%t\")\n:PROPERTIES:\n:STYLE: habit\n:END:\n"))))
  :config (progn
            (add-hook 'org-capture-prepare-finalize-hook 'org-id-get-create))
  :bind (("C-M-r" . org-capture)
         ("C-c r" . org-capture)))

;; org cua dwim

(req-package org-cua-dwim
  :loader el-get-local
  :require (cua-base org)
  :init (org-cua-dwim-activate))

;; ----------------------------------------------------------- [ org-ehtml ]

(req-package web-server)

(req-package org-ehtml
  :loader el-get-local
  :require (org web-server)
  :init (setq
         org-ehtml-everything-editable t
         org-ehtml-allow-agenda t
         org-ehtml-docroot (expand-file-name "~/Dropbox/workspace/org"))
  :config
  (defun pre-adjust-agenda-for-html nil
    "Adjust agenda buffer before htmlize.
Adds a link overlay to be intercepted by post-adjust-agenda-for-html."
    (goto-char (point-min))
    (let (marker id)
      (while (not (eobp))
        (cond
         ((setq marker (or (get-text-property (point) 'org-hd-marker)
                           (get-text-property (point) 'org-marker)))
          (when (and (setq id (org-id-get marker))
                     (let ((case-fold-search nil))
                       (re-search-forward (get-text-property (point) 'org-not-done-regexp)
                                          (point-at-eol) t)))
            (htmlize-make-link-overlay (match-beginning 0) (match-end 0) (concat "todo:" id)))
          ))
        (beginning-of-line 2))))
  (add-hook 'htmlize-before-hook 'pre-adjust-agenda-for-html)

  (defun post-adjust-agenda-for-html nil
    "Adjust agenda buffer after htmlize.
Intercept link overlay from pre-adjust-agenda-for-html, and
convert to call to javascript function."
    (goto-char (point-min))
    (search-forward "</head>")
    (beginning-of-line)
    (insert "
    <script src=\"http://code.jquery.com/jquery-1.10.2.min.js\"></script>
    <script>
        function todo (id) {
          var xurl   = 'todo/' + id;

          $.ajax({
              url: xurl
          }).success(function() {
              $('#message').text('done ' + xurl).show().fadeOut(1000);
          }).fail(function(jqXHR, textStatus) {
              $('#message').text('failed ' + xurl + ': ' + textStatus).show().fadeOut(5000);
              return false;
          });
        }
    </script>
")
    (search-forward "<body>")
    (beginning-of-line 2)
    (insert "    <span id=\"message\"></span>")
    (while (re-search-forward "<a href=\"todo:\\(.*\\)\">\\(.*\\)</a>" nil t)
      (replace-match "<a href='' onclick='todo(\"\\1\");'>\\2</a>")))
  (add-hook 'htmlize-after-hook 'post-adjust-agenda-for-html)

  (defun jeff/capture-handler (request)
    "Handle REQUEST objects meant for 'org-capture'.
GET header should contain a path in form '/capture/KEY/LINK/TITLE/BODY'."
    (with-slots (process headers) request
      (let ((path (cdr (assoc :GET headers))))
        (if (string-match "/capture:?/\\(.*\\)" path)
            (progn
              (org-protocol-capture (match-string 1 path))
              (ws-response-header process 200))
          (ws-send-404 process)))))

  (defun jeff/todo-handler (request)
    "Handle REQUEST objects meant for 'org-todo'.
GET header should contain a path in form '/todo/ID'."
    (with-slots (process headers) request
      (let ((path (cdr (assoc :GET headers))))
        (if (string-match "/todo:?/\\(.*\\)" path)
            (let* ((id (match-string 1 path))
                   (m (org-id-find id 'marker)))
              (when m
                (save-excursion (org-pop-to-buffer-same-window (marker-buffer m))
                                (goto-char m)
                                (move-marker m nil)
                                (org-todo 'done)
                                (save-buffer)))
              (ws-response-header process 200))
          (ws-send-404 process)))))

  (when (boundp 'ws-servers)
    (mapc (lambda (server)
            (if (= 3333 (port server))
                (ws-stop server)))
          ws-servers)
    (condition-case-unless-debug nil
        (ws-start '(((:GET  . "/capture") . jeff/capture-handler)
                    ((:GET  . "/todo")    . jeff/todo-handler)
                    ((:GET  . ".*")       . org-ehtml-file-handler)
                    ((:POST . ".*")       . org-ehtml-edit-handler))
                  3333)
      (error (message "Failed to create web server"))))
)

;; ----------------------------------------------------------- [ evernote ]

(req-package evernote-mode
  :loader el-get-local
  :init (progn
          (setq evernote-developer-token "S=s1:U=81f:E=1470997a804:C=13fb1e67c09:P=1cd:A=en-devtoken:V=2:H=0b3aafa546daa4a9b43c77a7574390d4"
                evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8") ; optional
                enh-enclient-command "/home/jeff/Dropbox/workspace/evernote-mode/ruby/bin/enclient.rb"))
  :bind (("C-c E c" . evernote-create-note)
         ("C-c E o" . evernote-open-note)
         ("C-c E s" . evernote-search-notes)
         ("C-c E S" . evernote-do-saved-search)
         ("C-c E w" . evernote-write-note)
         ("C-c E p" . evernote-post-region)
         ("C-c E b" . evernote-browser)))

;; ----------------------------------------------------------- [ windmove ]

(req-package windmove
  :bind (("<M-wheel-up>"   . windmove-up)
         ("<M-wheel-down>" . windmove-down)
         ("<M-up>"         . windmove-up)
         ("<M-down>"       . windmove-down)
         ("<M-left>"       . windmove-left)
         ("<M-right>"      . windmove-right)))

;; ----------------------------------------------------------- [ diminished ]
;; Better to put these in the mode-specific sections.
;; These diminish strings are only for those modes not mentioned elsewhere.


(req-package emacs-lisp          :diminish "eλ")
(req-package abbrev              :diminish "")
;(req-package auto-complete       :diminish " α")
;(req-package auto-fill-function  :diminish " φ")
;(req-package autopair            :diminish "")
;(req-package cider-interaction   :diminish " ηζ")
;(req-package cider               :diminish " ηζ")
;(req-package clojure             :diminish "cλ")
;(req-package eldoc               :diminish "")
;(req-package elisp-slime-nav     :diminish " δ")
(req-package flycheck            :diminish " φc")
(req-package flymake             :diminish " φm")
(req-package flyspell            :diminish " φs")
(req-package guide-key           :diminish "")
;(req-package guru                :diminish "")
;(req-package haskell             :diminish "hλ")
;(req-package hi-lock             :diminish "")
(req-package js2                 :diminish "jλ")
;(req-package kibit               :diminish " κ")
;(req-package lambda              :diminish "")
(req-package markdown            :diminish "md")
;(req-package nrepl-interaction   :diminish " ηζ")
;(req-package nrepl               :diminish " ηζ")
(req-package org-indent          :diminish " Οι")
(req-package paredit             :diminish " Φ")
;(req-package processing          :diminish "P5")
;(req-package python              :diminish "pλ")
;(req-package tuareg              :diminish "mλ")
(req-package volatile-highlights :diminish " υ")
;(req-package wrap-region         :diminish "")
;(req-package yas-minor           :diminish " γ")

;; smart mode line

(req-package smart-mode-line
  :require custom
  :init (progn
          (sml/setup))
  :config (progn
            (sml/apply-theme 'automatic)
            (add-to-list 'rm-excluded-modes " MRev" t)
            (add-to-list 'rm-excluded-modes " Guide" t)
            (add-to-list 'rm-excluded-modes " Helm" t)
            (add-to-list 'rm-excluded-modes " company" t)
            (add-to-list 'sml/replacer-regexp-list '("^:DB:workspace" ":WS:")   t)
            (add-to-list 'sml/replacer-regexp-list '("^:WS:/uplands"  ":UP:")   t)
            (add-to-list 'sml/replacer-regexp-list '("^:WS:/autodesk" ":ADSK:") t)
            (setq sml/col-number-format "%03c")
            (setq sml/use-projectile-p 'before-prefixes)
            ))

;; nyan mode

(req-package nyan-mode
  :loader el-get-local
  :init (progn (nyan-mode +1)
               (setq nyan-wavy-trail t)
               (setq nyan-animate-nyancat t)))

;; projectile mode

(req-package projectile
   :init (setq projectile-mode-line '(:eval (format " Π[%s]" (projectile-project-name))))
)

;; powerline
;; see https://github.com/11111000000/emacs-d/blob/master/init.el

;; (set-face-attribute 'mode-line nil
;;                     :family "Terminus"
;;                     :height 100)
(req-package powerline
  ;; :disabled t
  :require nyan-mode
  :init (progn
          (defadvice load-theme (after reset-powerline-cache activate) (pl/reset-cache))
          (defun powerline-jeff-theme ()
            "Set to Jeff's theme."
            (interactive)
            (setq powerline-default-separator 'wave
                  powerline-height 20
                  powerline-default-separator-dir '(left . right))

            (setq-default mode-line-format
                          '("%e"
                            (:eval
                             (let* ((active (powerline-selected-window-active))
                                    (mode-line (if active 'mode-line 'mode-line-inactive))
                                    (face1 (if active 'powerline-active1 'powerline-inactive1))
                                    (face2 (if active 'powerline-active2 'powerline-inactive2))

                                    (separator-left (intern (format "powerline-%s-%s"
                                                                    'wave
                                                                    (car powerline-default-separator-dir))))

                                    (separator-right (intern (format "powerline-%s-%s"
                                                                     'wave
                                                                     (cdr powerline-default-separator-dir))))

                                    (lhs (list
                                          (powerline-raw "%*" nil 'l)
                                          (powerline-buffer-size nil 'l)
                                          (powerline-buffer-id nil 'l)
                                          (powerline-raw " ")
                                          (funcall separator-left mode-line face1)
                                          (powerline-narrow face1 'l)
                                          (powerline-vc face1)))
                                    (rhs (list
                                          (when (bound-and-true-p nyan-mode)
                                            (powerline-raw (list (nyan-create)) face1 'r))
                                          (powerline-raw "%4l" face1 'r)
                                          (powerline-raw ":" face1)
                                          (powerline-raw "%3c" face1 'r)
                                          (funcall separator-right face1 mode-line)
                                          (powerline-raw " ")
                                          (powerline-raw global-mode-string nil 'r)
                                          ;;(powerline-raw "%6p" nil 'r)
                                          ;;(powerline-hud face2 face1)
                                          ))
                                    (ctr (list
                                          ;;(powerline-raw " " face1)
                                          (funcall separator-left face1 face2)
                                          (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                            (powerline-raw erc-modified-channels-object face2 'l))
                                          (powerline-major-mode face2 'l)
                                          (powerline-process face2)
                                          (powerline-raw " :" face2)
                                          (powerline-minor-modes face2 'l)
                                          (powerline-raw " " face2)
                                          (funcall separator-right face2 face1))))

                               (concat (powerline-render lhs)
                                       (powerline-fill-center face1 (/ (powerline-width ctr) 2.0))
                                       (powerline-render ctr)
                                       (powerline-fill face1 (powerline-width rhs))
                                       (powerline-render rhs)))))))
          (powerline-jeff-theme)
          ))

;; ----------------------------------------------------------- [ edit-server ]

(req-package edit-server
  :require edit-server-htmlize
  :config (progn
            (setq edit-server-new-frame nil)
            (autoload 'edit-server-maybe-dehtmlize-buffer "edit-server-htmlize" "edit-server-htmlize" t)
            (autoload 'edit-server-maybe-htmlize-buffer   "edit-server-htmlize" "edit-server-htmlize" t)
            (add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
            (add-hook 'edit-server-done-hook  'edit-server-maybe-htmlize-buffer)
            (edit-server-start))
  :init (add-hook 'edit-server-start-hook
          (lambda ()
            (when (string-match "github.com" (buffer-name))
              (markdown-mode)))))

;; ----------------------------------------------------------- [ theme ]

(req-package custom
  :init (setq custom-safe-themes t))

(req-package solarized-theme
   :require custom
   :init (defun solarized nil
           "Enable solarized theme"
           (interactive)
           (disable-theme 'zenburn)
           (setq solarized-high-contrast-mode-line nil)
           (setq solarized-scale-org-headlines t)
           (load-theme 'solarized-dark t)
           (sml/apply-theme 'respectful)
           (setq x-underline-at-descent-line t)))

(req-package zenburn-theme
  :require custom
  :init (defun zenburn nil
          "Enable zenburn theme"
          (interactive)
          (disable-theme 'solarized-dark)
          (load-theme 'zenburn t)
          (sml/apply-theme 'respectful)))

(deftheme jeff-theme "Jeff's theme.")
(custom-theme-set-faces
 'jeff-theme
 ;; '(helm-ff-directory ((t (:foreground "deep sky blue"))))
 ;; '(helm-ff-file ((t (:foreground "gainsboro"))))
 ;; '(helm-ff-symlink ((t (:foreground "cyan"))))
 ;; '(highlight ((t (:background "black"))))
 ;; '(org-agenda-current-time ((t (:inherit org-time-grid :background "dim gray"))) t)
 ;; '(org-agenda-done ((t (:foreground "dim gray"))))
 ;; '(org-scheduled-previously ((t (:foreground "#bc8383"))))
 ;; '(org-warning ((t (:foreground "#cc9393" :weight bold))))
 ;; '(region ((t (:background "dim gray"))))
 ;; '(mode-line ((t :overline ,unspecified :underline nil :box '(:line-width 1 :color "#969896"))))
 )
(enable-theme 'jeff-theme)

;; ----------------------------------------------------------- [ key bindings ]

(define-key special-event-map [delete-frame] 'save-buffers-kill-terminal)
(global-set-key (kbd "<M-f4>")          'save-buffers-kill-terminal)
(global-set-key (kbd "<f4>")            'next-error)
(global-set-key (kbd "<f7>")            'goto-line)
(global-set-key (kbd "<f10>")           'eval-last-sexp)
(global-set-key (kbd "C-w")             'kill-buffer-and-window)
(global-set-key (kbd "RET")             'newline-and-indent)
(global-set-key (kbd "C-S-a")           'mark-whole-buffer)
(global-set-key (kbd "<C-next>")        'scroll-other-window)
(global-set-key (kbd "<C-prior>")       'scroll-other-window-down)
(global-set-key (kbd "<C-tab>")         'next-buffer)
(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-buffer)

(define-key isearch-mode-map (kbd "<f3>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-f")  'isearch-repeat-forward)

(global-set-key (kbd "<mouse-8>")       'switch-to-prev-buffer)
(global-set-key (kbd "<mouse-9>")       'switch-to-next-buffer)

;; ----------------------------------------------------------- [ quicken ]

(defun number-lines-region (start end &optional beg)
  "Add numbers to all lines from START to ENDs, beginning at number BEG."
  (interactive "*r\np")
  (let* ((lines (count-lines start end))
         (from (or beg 1))
         (to (+ lines (1- from)))
         (numbers (number-sequence from to))
         (width (max (length (int-to-string lines))
                     (length (int-to-string from)))))
    (goto-char start)
    (dolist (n numbers)
      (beginning-of-line)
      (save-match-data
        (if (looking-at " *-?[0-9]+\\. ")
            (replace-match "")))
      (insert (format (concat "%" (int-to-string width) "d. ") n))
      (forward-line))))

(defun quicken-cleanup-uncategorized ()
  "Transform raw data pasted from quicken report into format suitable for email."
  (interactive)

  (goto-char (point-min))

  (save-excursion
    (dotimes (number 4 nil) (kill-line))
    (beginning-of-line 2)
    (kill-line)
    (goto-char (point-max))
    (beginning-of-line 0)
    (kill-line))

  (save-excursion
    (re-search-forward ".*Date.*Account.*Num.*Description.*Amount" nil t)
    (replace-match "| Item | Date | Account | Num | Description | Amount | Category |
|--+")
    (replace-regexp "^[^/]+$" ""))

  (flush-lines "^$")

  (save-excursion
    (while (re-search-forward "\t" nil t)
      (replace-match "|" nil nil)))

  (save-excursion
    (forward-line)(forward-line)
    (number-lines-region (point) (point-max)))

  ;; (save-excursion
  ;;   (while (re-search-forward "^\\([0-9]+\.\\) " nil t)
  ;;     (replace-match "\\1\|")))

  (save-excursion
    (forward-line)(forward-line)
    (while (re-search-forward "^" nil t)
      (replace-match "|" nil nil)))

  (save-excursion
    (goto-char (point-max))
    (beginning-of-line 1)
    (kill-line))

  (org-mode)
  (org-table-align)
  (clipboard-kill-ring-save (point-min) (point-max))
  (message "table saved to clipboard")
  )

;; ----------------------------------------------------------- [ finish ]

(req-package-finish)

(defun jeff/organizer ()
  "Show schedule in fullscreen."
  (interactive)
  (toggle-frame-fullscreen)
  (run-with-idle-timer 1 nil (lambda () (org-agenda nil "s"))))

(add-hook 'after-init-hook
          '(lambda () (if (tty-type (frame-terminal)) (zenburn) (solarized) )))

(provide 'personal)
;;; personal.el ends here
