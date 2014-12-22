;;; personal.el -- emacs personalization file
;;; Commentary:
;;; Code:
;;;

;; ----------------------------------------------------------- [ el-get ]

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
       (lambda (s)
         (let (el-get-master-branch)
           (goto-char (point-max))
           (eval-print-last-sexp))))))

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.

(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
  (require 'el-get))

;; set local recipes

(setq el-get-sources
      '((:name evernote-mode
               :description "Functions for editing Evernote notes directly from Emacs"
               :type git
               :url "https://github.com/jeffkowalski/evernote-mode.git"
               :features evernote-mode)
        (:name nyan-mode
               :description "Nyan Cat for Emacs! Nyanyanyanyanyanyanyanyanyan!"
               :type git
               :url "https://github.com/jeffkowalski/nyan-mode.git"
               :features nyan-mode)
        (:name org
                :website "http://orgmode.org/"
                :description "Org-mode is for keeping notes, maintaining ToDo lists, doing project planning, and authoring with a fast and effective plain-text system."
                :type git
                :url "git://orgmode.org/org-mode.git"
                :info "doc"
                :build/berkeley-unix `,(mapcar
                                        (lambda (target)
                                          (list "gmake" target (concat "EMACS=" (shell-quote-argument el-get-emacs))))
                                        '("oldorg"))
                :build `,(mapcar
                          (lambda (target)
                            (list "make" target (concat "EMACS=" (shell-quote-argument el-get-emacs))))
                          '("oldorg"))
                :load-path ("." "contrib/lisp" "lisp")
                ;; :load ("lisp/org-loaddefs.el")
                :features org
                :load ("lisp/org.el")
                )
        (:name org-cua-dwim
               :description "Org-mode and CUA-mode compatibility layer"
               :type git
               :url "https://github.com/jeffkowalski/org-cua-dwim.git"
               :depends org
               :features org-cua-dwim)
        (:name org-ehtml
               :description "Export Org-mode files as editable web pages"
               :type git
               :url "https://github.com/jeffkowalski/org-ehtml.git"
               :load-path "src")
        (:name org-reveal
               :description "Exports Org-mode contents to Reveal.js HTML presentation"
               :type git
               :url "https://github.com/jeffkowalski/org-reveal.git"
               :depends org
               :features ox-reveal)
        ))

;; ----------------------------------------------------------- [ packages ]
;; Install the missing packages

(dolist (p '(use-package
             ))
  (unless (package-installed-p p)
    (package-install p)))

(require 'use-package)
(use-package use-package
  :config (setq use-package-verbose t
                use-package-minimum-reported-time 0))
(use-package req-package
  :ensure t
  :demand t)

;; Override function defined in use-package, so that packages
;; from el-get are considered as well as those from the package manager.

(defun use-package-ensure-elpa (package)
  "Install PACKAGE if not installed by elpa package manager or el-get."
  (when (not (or (package-installed-p package) (el-get-package-exists-p package)))
    (package-install package)))

;; Override function defined in req-package, so that packages
;; from el-get-sources are considered as well as those from el-get-recipes

(defun req-package-try-el-get (package)
  "Install PACKAGE if available and not already installed."
  (if req-package-el-get-present
      (let* ((AVAIL (or (el-get-recipe-filename package)
                        (memq package (mapcar (lambda (x) (plist-get x :name)) el-get-sources))))
             (INSTALLED (or (el-get-package-exists-p package)
                            (package-installed-p package))))
        (if (and AVAIL (not INSTALLED))
            (or (el-get 'sync package) t) ;; TODO check for success
          INSTALLED))
    nil))

(el-get 'sync)

;; Enable sorting on all columns in package menu's tabular list.
;; Note my naive mapping removes the final properties (like :right-align) if present.

(add-hook 'package-menu-mode-hook
          (lambda () (setq tabulated-list-format
                             (vconcat (mapcar (lambda (arg) (list (nth 0 arg) (nth 1 arg)
                                                            (or (nth 2 arg) t)))
                                       tabulated-list-format)))))

(define-key package-menu-mode-map "o" 'delete-other-windows)

;; ----------------------------------------------------------- [ cua ]

(use-package cua-base
  :init (cua-mode t)
  :config (setq cua-keep-region-after-copy nil))

;; FIXME: this is referenced from smartparens, and used to be in cua-base, but is no longer there
;;        https://github.com/Fuco1/smartparens/issues/271

(eval-when-compile
  (require 'cua-base))
(unless (fboundp 'cua-replace-region)
  (defun cua-replace-region ()
    "Replace the active region with the character you type."
    (interactive)
    (let ((not-empty (and cua-delete-selection (cua-delete-region))))
      (unless (eq this-original-command this-command)
        (let ((overwrite-mode
               (and overwrite-mode
                    not-empty
                    (not (eq this-original-command 'self-insert-command)))))
          (cua--fallback))))))

;; FIXME: workaround problem in CUA which doesn't seem to obey delete-selection
;;        behavior on paste

(defadvice cua-paste (before clobber-region (&optional arg))
  "Delete the region before pasting."
  (when (region-active-p) (delete-region (region-beginning) (region-end))))
(ad-activate 'cua-paste)

;; ----------------------------------------------------------- [ adornments ]

;; off
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
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

;; mode mode

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
   (?m . "~/Dropbox/sync-linux/mac_addrs.org")
   (?z . "~/.zshrc")
   (?s . "~/bin/sauron.pl")))

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
                       multi-term-dedicated-select-after-open-p nil
                       multi-term-program-switches "--login")
                 (bind-key "C-c t" 'multi-term-dedicated-toggle prelude-mode-map)))

;; ----------------------------------------------------------- [ undo-tree ]

(req-package undo-tree
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
  :demand t
  :init (helm-mode 1)
  :bind (("C-x C-f" . helm-find-files)
         ("M-x"     . helm-M-x)
         ("C-x b"   . helm-buffers-list)
         ("C-M-g"   . helm-do-grep))
  :config (progn
            (defun jeff/find-file-as-root ()
              "Like 'helm-find-file', but automatically edit the file with root-privileges (using tramp/sudo), if the file is not writable by user."
              (interactive)
              (let ((file (helm-read-file-name "Edit as root: ")))
                (unless (file-writable-p file)
                  (setq file (concat "/sudo:root@localhost:" file)))
                (find-file file)))
            (global-set-key (kbd "C-x F") 'jeff/find-file-as-root)))

;; FIXME: workaround problem in
;;        select-frame-set-input-focus(#<frame *Minibuf-1* * 0x6a44268>)
;;        helm-frame-or-window-configuration(restore)
;;        helm-cleanup()
;;        ...
;;        helm-internal(...)
;;        ...
;;
;;        which throws error "progn: Not an in-range integer, float, or cons of integers"
;;
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

(req-package helm-swoop
  :require helm
  :defines (helm-swoop-last-prefix-number)
  :demand t
  :bind (("M-i" . helm-swoop)))

;; ----------------------------------------------------------- [ guide-key ]

(req-package guide-key
  :init (progn
          (setq guide-key/guide-key-sequence
                '("C-x r"
                  "C-x 4"
                  (flycheck-mode   "C-c !")
                  (helm-mode       "C-c h")
                  (projectile-mode "C-c p")
                  (org-mode        "C-c C-x")))
          (guide-key-mode 1)))

;; ----------------------------------------------------------- [ company ]

(req-package company
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
  :init (setq magit-diff-options '("--ignore-all-space"))) ; ignore whitespace

;; ----------------------------------------------------------- [ ibuffer ]

;; *Nice* buffer switching
(req-package ibuffer
  :init (use-package ibuf-ext)
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
  :demand t
  :loader req-package-try-el-get
  :init
  (setq org-directory "~/Dropbox/workspace/org/"
        ;;org-replace-disputed-keys t ; org-CUA-compatible
        org-log-into-drawer t
        org-support-shift-select 'always
        org-default-notes-file (concat org-directory "refile.org")
        org-agenda-files (list (concat org-directory "toodledo.org")
                               (concat org-directory "sauron.org")
                               (concat org-directory "gcal.org"))
        org-modules '(org-bbdb org-bibtex org-docview org-gnus org-info org-habit org-irc org-mhe org-rmail org-w3m)
        org-startup-indented t
        org-enforce-todo-dependencies t
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

(req-package ox
  :require org
  :init (setq org-id-locations-file "~/Dropbox/workspace/org/.org-id-locations")
)

;; org habit

(req-package org-habit
  :require org
  :init (setq org-habit-following-days 1
              org-habit-graph-column 46))

;; org mobile

(req-package org-mobile
  :require org
  :init (setq org-mobile-directory "~/Dropbox/mobileorg/"
              org-mobile-agendas '("a")
              org-mobile-inbox-for-pull (concat org-mobile-directory "flagged.org")))

;; htmlize

(req-package htmlize)

;; org agenda

(req-package org-agenda
  :require (org htmlize)
  :init (progn (setq org-agenda-tags-column -97
                     org-agenda-block-separator (let ((retval ""))
                                                  (dotimes (i (- org-agenda-tags-column)) (setq retval (concat retval "=")))
                                                  retval)
                     org-agenda-timegrid-use-ampm t
                     org-agenda-window-setup 'current-window
                     org-agenda-log-mode-items '(clock closed state)
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

(req-package org-protocol)

;; org capture

(req-package org-capture
  :require org
  :init (setq org-capture-templates
              (quote (("b" "entry.html" entry (file+headline (concat org-directory "toodledo.org") "TASKS")
                       "* TODO %:description\n%:initial\n" :immediate-finish t)
                      ("t" "todo" entry (file+headline (concat org-directory "toodledo.org") "TASKS")
                       "* TODO [#C] %?\n")
                      ("w" "org-protocol" entry (file+headline (concat org-directory "toodledo.org") "TASKS")
                       "* TODO [#C] %:description\nSCHEDULED: %t\n%:link\n%:initial\n")
                      ("h" "Habit" entry (file+headline (concat org-directory "toodledo.org") "TASKS")
                       "* TODO [#C] %?\nSCHEDULED: %t .+1d/3d\n:PROPERTIES:\n:STYLE: habit\n:END:\n"))))
  :config (progn
            (add-hook 'org-capture-prepare-finalize-hook 'org-id-get-create))
  :bind (("C-M-r" . org-capture)
         ("C-c r" . org-capture)))

;; org cua dwim

(req-package org-cua-dwim
  :demand t
  :loader req-package-try-el-get
  :require (cua-base org)
  :init (org-cua-dwim-activate))

;; ----------------------------------------------------------- [ org-ehtml ]

(req-package web-server)

(req-package org-ehtml
  :demand t
  :loader req-package-try-el-get
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
  :demand t
  :loader req-package-try-el-get
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

;; ----------------------------------------------------------- [ theme ]

(req-package custom
  :init (setq custom-safe-themes t))

(req-package solarized-theme
   :require custom
   :init (progn (disable-theme 'zenburn)
                (setq solarized-high-contrast-mode-line nil)
                (setq solarized-scale-org-headlines t)
                (load-theme 'solarized-dark t))
   :config (progn (setq x-underline-at-descent-line t)))

;; (req-package color-theme-sanityinc-solarized
;;   :init (progn (disable-theme 'zenburn)
;;                (load-theme 'sanityinc-solarized-dark t)))

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
            (setq projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))
            ))

;; nyan mode

(req-package nyan-mode
  :demand t
  :loader req-package-try-el-get
  :require smart-mode-line
  :init (progn (nyan-mode +1)
               ;;(setq nyan-wavy-trail t)
               (nyan-start-animation)))

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

(global-set-key (kbd "M-z")             'zap-up-to-char)

(define-key isearch-mode-map (kbd "<f3>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-f")  'isearch-repeat-forward)

(global-set-key (kbd "<mouse-8>")       'switch-to-prev-buffer)
(global-set-key (kbd "<mouse-9>")       'switch-to-next-buffer)

;; ----------------------------------------------------------- [ finish ]

(req-package-finish)

(defun jeff/organizer ()
  "Show schedule in fullscreen."
  (interactive)
  (toggle-frame-fullscreen)
  (run-with-idle-timer 1 nil (lambda () (org-agenda nil "s"))))

(provide 'personal)
;;; personal.el ends here
