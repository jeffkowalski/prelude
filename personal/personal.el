;;; personal.el -- emacs personalization file
;;; Commentary:
;;; Code:
;;;

;; Set repositories

(when (>= emacs-major-version 24)
  (customize-set-variable 'package-archives '(("ELPA" . "http://tromey.com/elpa/")
                                              ("org" . "http://orgmode.org/elpa/")
                                              ("gnu" . "http://elpa.gnu.org/packages/")
                                              ("melpa" . "http://melpa.org/packages/")
                                              ("melpa-stable" . "http://stable.melpa.org/packages/")
                                              ("marmalade" . "http://marmalade-repo.org/packages/")
                                              )))

;; Setup req-package

(unless (package-installed-p 'req-package) (package-install 'req-package))
  (require 'req-package)
  (customize-set-variable 'use-package-verbose t)
  (customize-set-variable 'use-package-compute-statistics t)
  (customize-set-variable 'use-package-minimum-reported-time 0)
;;  (customize-set-variable 'use-package-always-ensure t)
  (customize-set-variable 'req-package-log-level 'trace)
  (req-package--log-set-level req-package-log-level)

;; Setup validate package, useful for checking setq symbols and values

(unless (package-installed-p 'validate) (package-install 'validate))
(require 'validate)
;;(defalias 'validate-setq 'setq)

;; Setup el-get first

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(when (not (require 'el-get))
  (req-package-force el-get))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/el-get/recipes")
(customize-set-variable 'el-get-sources
                        '(
                          (:name org-expiry
                                 :description "Expiry mechanism for Org entries"
                                 :type http
                                 :url "https://code.orgmode.org/bzg/org-mode/raw/master/contrib/lisp/org-expiry.el"
                                 :localname "org-expiry.el"
                                 :features org-expiry)
                          (:name flymake
                                 :description "Continuous syntax checking for Emacs"
                                 :type github
                                 :pkgname "jeffkowalski/emacs-flymake")
                          (:name org-cua-dwim
                                 :description "Org-mode and CUA-mode compatibility layer"
                                 :type github
                                 :pkgname "jeffkowalski/org-cua-dwim"
                                 :features org-cua-dwim)
                          (:name eshell-git-prompt
                                 :description "Some Eshell prompts for Git users"
                                 :type github
                                 :pkgname "jeffkowalski/eshell-git-prompt"
                                 :features eshell-git-prompt)
                          (:name org-ehtml
                                 :description "Export Org-mode files as editable web pages"
                                 :type github
                                 :pkgname "jeffkowalski/org-ehtml"
                                 :load-path "src")
                          (:name emacs-web-server
                                 :description "Web server running Emacs Lisp handlers"
                                 :type github
                                 :pkgname "jeffkowalski/emacs-web-server"
                                 :features web-server)
                          ))
(el-get 'sync (mapcar (lambda (x) (plist-get x :name)) el-get-sources))

;; Override function defined in use-package, so that packages from el-get are considered as well as those from the package manager.

(defun use-package-ensure-elpa (name args state &optional no-refresh)
  (dolist (ensure args)
    (let ((package
           (or (and (eq ensure t) (use-package-as-symbol name))
               ensure)))
      (when package
        (require 'package)
        (when (consp package)
          (use-package-pin-package (car package) (cdr package))
          (setq package (car package)))
        (unless (or (package-installed-p package) (el-get-package-exists-p package))
          (condition-case-unless-debug err
              (progn
                (when (assoc package (bound-and-true-p
                                      package-pinned-packages))
                  (package-read-all-archive-contents))
                (if (assoc package package-archive-contents)
                    (package-install package)
                  (package-refresh-contents)
                  (when (assoc package (bound-and-true-p
                                        package-pinned-packages))
                    (package-read-all-archive-contents))
                  (package-install package))
                t)
            (error
             (display-warning 'use-package
                              (format "Failed to install %s: %s"
                                      name (error-message-string err))
                              :error))))))))

(add-to-list 'use-package-keywords :el-get)

(defun use-package-normalize/:el-get (name-symbol keyword args)
  (use-package-only-one (symbol-name keyword) args
    (lambda (label arg)
      (cond
       ((booleanp arg) name-symbol)
       ((symbolp arg) arg)
       (t
        (use-package-error
         ":el-get wants an package name or boolean value"))))))

(defun use-package-handler/:el-get (name-symbol keyword archive-name rest state)
  (let ((body (use-package-process-keywords name-symbol rest state)))
    ;; This happens at macro expansion time, not when the expanded code is
    ;; compiled or evaluated.
    (if (null archive-name)
        body
      (el-get-install archive-name)
      body)))

;; Enable sorting on all columns in package menu's tabular list.
;; Note my naive mapping removes the final properties (like :right-align) if present.

(add-hook 'package-menu-mode-hook
          (lambda () (validate-setq tabulated-list-format
                             (vconcat (mapcar (lambda (arg) (list (nth 0 arg) (nth 1 arg)
                                                            (or (nth 2 arg) t)))
                                       tabulated-list-format)))))

(define-key package-menu-mode-map "o" 'delete-other-windows)

;; package-utils and upgrades

(req-package package-utils
  :init
  (defun upgrade-emacs-packages nil
    (interactive)
    "Upgrade all packages"
    (package-utils-upgrade-all)
    (el-get-update-all t)
    (message "upgrade complete"))
)

;; use-package-chords

(req-package use-package-chords
   :force t ;; load package immediately, no dependency resolution
   :config (key-chord-mode 1))

;; cua

(req-package cua-base
  :config
  (cua-mode t))

;; FIXME workaround problem in CUA which doesn't seem to obey delete-selection behavior on paste

(defadvice cua-paste (before clobber-region (&optional arg))
  "Delete the region before pasting."
  (when (region-active-p) (delete-region (region-beginning) (region-end))))
(ad-activate 'cua-paste)

;; adornments

;; off
(scroll-bar-mode -1)
;;(horizontal-scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
;; on
(blink-cursor-mode t)
(global-hl-line-mode t)
(show-paren-mode t)

(customize-set-variable 'cursor-type '(bar . 2)) ; local
(customize-set-variable 'indicate-empty-lines t) ; local
(customize-set-variable 'inhibit-startup-echo-area-message "jeff")
(customize-set-variable 'inhibit-startup-screen t)
(customize-set-variable 'initial-scratch-message nil)
(customize-set-variable 'show-trailing-whitespace t)
(customize-set-variable 'indent-tabs-mode nil)

(validate-setq frame-title-format '(buffer-file-name "emacs - %f %*" ("%b %*"))
               icon-title-format  '(buffer-file-name "emacs - %f %*" ("%b %*")))

;; miscellaneous

;;(validate-setq disabled-command-function nil)   ; enable all commands

(customize-set-variable 'auto-save-default nil)
(customize-set-variable 'auto-save-list-file-prefix nil)
(customize-set-variable 'bookmark-save-flag nil)
(customize-set-variable 'browse-url-mailto-function nil)
(customize-set-variable 'enable-recursive-minibuffers t)
(customize-set-variable 'help-window-select t)
(customize-set-variable 'kill-whole-line t)
(customize-set-variable 'make-backup-files nil)
(customize-set-variable 'password-cache-expiry 900)
(customize-set-variable 'user-mail-address "jeff.kowalski@gmail.com")

;; auto-revert

(req-package autorevert
  :config
  (auto-revert-mode 1)
  (global-auto-revert-mode 1)
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-use-notify nil)
  (auto-revert-interval 1))

;; editorconfig

(req-package editorconfig)

;; clang-format

(req-package clang-format
  :bind (("C-M-\\" . clang-format-buffer))
  :custom
  (clang-format-executable "clang-format"))

;; cperl mode

(req-package cperl-mode
  :config (defalias 'perl-mode 'cperl-mode))

;; compile

(req-package compile
  :bind (("<f5>" . recompile)))

;; doc view

(req-package doc-view
  :custom
  (doc-view-ghostscript-options
   '("-dMaxBitmap=2147483647" "-dSAFER" "-dNOPAUSE" "-sDEVICE=png16m" "-dTextAlphaBits=4" "-dBATCH" "-dGraphicsAlphaBits=4" "-dQUIET"))
  (doc-view-resolution 300))

;; fish mode

(req-package fish-mode)

;; make mode

(req-package make-mode
  ;; re-tabbing during whitespace-cleanup would kill makefiles
  :hook
  (makefile-mode . (lambda () (remove-hook 'before-save-hook 'whitespace-cleanup t))))

;; whitespace

(defun modi/just-one-space-post-kill-word (&rest _)
  "Function to manage white space after `kill-word' operations.

1. If point is at the beginning of the line after possibly some white space,
   remove that white space and re-indent that line.
2. If there is space before or after the point, ensure that there is only
   one white space around the point.
3. Otherwise, do nothing.

During the whole operation do not change the point position with respect to the
surrounding white space.

abc|   def  ghi <-- point on the left of white space after 'abc'
abc| ghi        <-- point still before white space after calling this function
abc   |def  ghi <-- point on the right of white space before 'def'
abc |ghi        <-- point still after white space after calling this function."
  (save-excursion ; maintain the initial position of the pt with respect to space
    (cond ((looking-back "^ *") ; remove extra space at beginning of line
           (just-one-space 0)
           (indent-according-to-mode))
          ((or (looking-at   " ")
               (looking-back " ")) ; adjust space only if it exists
           (just-one-space 1))
          (t ; do nothing otherwise, includes case where the point is at EOL
           ))))
;; Delete extra horizontal white space after `kill-word' and `backward-kill-word'
(advice-add 'kill-word :after #'modi/just-one-space-post-kill-word)

;; emacs prelude

(req-package prelude-mode
  :defines (prelude-mode-map)
  :config
  ;; fix keyboard behavior on terminals that send ^[O{ABCD} for arrows
  (defvar ALT-O-map (make-sparse-keymap) "ALT-O keymap.")
  (define-key prelude-mode-map (kbd "M-O") ALT-O-map))

(req-package prelude-custom
  :custom
  (prelude-guru nil))

;; smartparens

(req-package smartparens
  :bind (:map smartparens-strict-mode-map
              ("M-<delete>" . sp-unwrap-sexp)
              ("M-<backspace>" . sp-backward-unwrap-sexp)))

;; registers
;; Registers allow you to jump to a file or other location quickly.
;; To jump to a register, use C-x r j followed by the letter of the register.

(mapc
 (lambda (r)
   (set-register (car r) (cons 'file (cdr r))))
 '((?p . "~/.emacs.d/personal/personal.org")
   (?i . "~/Dropbox/sync-linux/installation.txt")
   (?j . "~/Dropbox/workspace/org/journal.org")
   (?c . "~/.emacs.d/personal/custom.el")
   (?f . "~/.config/fish/config.fish")
   (?m . "~/Dropbox/sync-linux/mac_addrs.org")
   (?z . "~/.zshrc")
   (?s . "~/Dropbox/workspace/bots/sauron/sauron.rb")))

;; shell / eshell

(req-package eshell
  :hook
  (emacs-startup . (lambda ()
                     (let ((default-directory (getenv "HOME")))
                       (command-execute 'eshell)
                       (bury-buffer))))

  ;; Visual commands are commands which require a proper terminal.
  ;; eshell will run them in a term buffer when you invoke them.
  :custom
  (eshell-visual-commands
   '("less" "tmux" "htop" "top" "bash" "zsh" "fish"))
  (eshell-visual-subcommands
   '(("git" "log" "l" "diff" "show"))))

(req-package eshell-git-prompt
  :after eshell
  :config
  (set-fontset-font t 'unicode "PowerlineSymbols" nil 'prepend))

;; hide trailing whitespaces in some modes:
 (mapc (lambda (hook)
         (add-hook hook (lambda ()
                          (validate-setq show-trailing-whitespace nil))))
       '(eshell-mode-hook term-mode-hook vterm-mode-hook))

;; multi-term

(req-package multi-term
  :bind* (("C-c t" . multi-term-dedicated-toggle))
  :custom
  (multi-term-dedicated-close-back-to-open-buffer-p t)
  (multi-term-dedicated-select-after-open-p t)
  (multi-term-program-switches "--login"))

;; undo-tree

(req-package undo-tree
  :bind* (("C-z" . undo-tree-undo))
  :config (global-undo-tree-mode))

;; image+

(req-package image+
  :config
  (imagex-global-sticky-mode)
  (imagex-auto-adjust-mode)
  :bind (:map imagex-sticky-mode-map
              ("+" . imagex-sticky-zoom-in)
              ("-" . imagex-sticky-zoom-out)
              ("l" . imagex-sticky-rotate-left)
              ("r" . imagex-sticky-rotate-right)
              ("m" . imagex-sticky-maximize)
              ("o" . imagex-sticky-restore-original)
              ("\C-x\C-s" . imagex-sticky-save-image)))

;; cmake

(req-package cmake-mode
  :custom
  (cmake-tab-width 4))

(req-package cmake-ide ; https://github.com/atilaneves/cmake-ide
  :after rtags         ; https://github.com/Andersbakken/rtags
  :config (cmake-ide-setup))

;; dired

(req-package dired-single
  :after (autorevert dired)
  :custom
  (font-lock-maximum-decoration (quote ((dired-mode) (t . t))))
  (dired-omit-files (concat dired-omit-files "\\."))
  :hook
  (dired-mode . dired-omit-mode)
  :bind (:map dired-mode-map
             ("<RET>" . dired-single-buffer)
             ("<down-mouse-1>" . dired-single-buffer-mouse)
             ("^" . (lambda () (interactive) (dired-single-buffer "..")))))

;; smex

(req-package smex ; remember recently and most frequently used commands
  :custom
  (smex-save-file (expand-file-name ".smex-items" prelude-savefile-dir)))

;; ivy/counsel/swiper

(req-package ivy
  :config (counsel-mode 1)
  ;; Use Enter on a directory to navigate into the directory, not open it with dired
  :bind (:map ivy-minibuffer-map ("RET" . ivy-alt-done)))

;; ruby-tools

(req-package ruby-tools)

;; rbenv

(req-package rbenv
  :custom
  (rbenv-executable (concat (getenv "HOME") "/.linuxbrew/bin/rbenv"))
  (rbenv-show-active-ruby-in-modeline nil)
  :config
  (global-rbenv-mode))

;; inf-ruby

(req-package inf-ruby
  :after rbenv
  :custom
  (inf-ruby-default-implementation "pry"))

;; robe

(req-package robe
  :after (company inf-ruby)
  :config
  (eval-after-load 'company '(push 'company-robe company-backends))
  :hook
  (ruby-mode . robe-mode)
  ;; (add-hook 'robe-mode-hook 'ac-robe-setup)
  ;; (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate) (rvm-activate-corresponding-ruby))
  )

;; rubocop

(req-package rubocop
  :hook
  (ruby-mode . rubocop-mode))

;; realgud

(req-package realgud)
(req-package realgud-pry
  :after realgud)
(req-package realgud-byebug
  :after realgud)

;; company

(req-package company
  :custom
  (company-auto-complete 'company-explicit-action-p)
  (company-idle-delay 0.5)
  :config
  (add-to-list 'company-backends 'company-dabbrev t)
  (add-to-list 'company-backends 'company-ispell t)
  (add-to-list 'company-backends 'company-files t)
  (add-to-list 'company-transformers 'company-sort-by-occurrence))

(defun my-pcomplete-capf ()
  "Org-mode completions."
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))

(add-hook 'org-mode-hook 'my-pcomplete-capf)

;; irony/platformio

(req-package irony
  :config
  ;; Use irony's completion functions.
  (add-hook 'irony-mode-hook
            (lambda ()
              (define-key irony-mode-map [remap completion-at-point]
                'counsel-irony)
              (define-key irony-mode-map [remap complete-symbol]
                'counsel-irony)
              (irony-cdb-autosetup-compile-options))))

(req-package flycheck-irony
  :after (flycheck irony)
  :config
  ;; Setup irony for flycheck.
  (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))

(req-package irony-eldoc
  :after irony
  :config
  (add-hook 'irony-mode-hook #'irony-eldoc))

(req-package platformio-mode
  :config
  (projectile-register-project-type
   'PlatformIO '("platformio.ini")
   :compile "pio run"
   :src-dir "src"
   :test-dir "test"
   :test "pio test"
   :run "pio run -t upload"))

;; edit ino files with arduino mode.
(req-package arduino-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ino$" . arduino-mode)))

;; Enable irony for all c++ files, and platformio-mode only
;; when needed (platformio.ini present in project root).
(add-hook 'c++-mode-hook
          (lambda ()
            (irony-mode)
            (irony-eldoc)
            (platformio-conditionally-enable)))

;; lsp
;; from https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;         (XXX-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))




;; from https://www.sandeepnambiar.com/setting-up-emacs-for-c++/


;;(use-package lsp-mode :commands lsp :ensure t)
;;(use-package lsp-ui :commands lsp-ui-mode :ensure t)
;; (use-package company-lsp
;;   :ensure t
;;   :commands company-lsp
;;   :config (push 'company-lsp company-backends)) ;; add company-lsp as a backend



;; Then we can setup ccls. It uses flymake for its syntax checking. We can disable that to let flycheck do its thing.


(use-package ccls
  :ensure t
  :config
  (setq ccls-executable "ccls")
  (setq lsp-prefer-flymake nil)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))

;; tramp

;; disable version control checks
(customize-set-variable 'vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; magit

(req-package magit
  :custom
  (magit-diff-arguments '("--ignore-all-space" "--stat" "--no-ext-diff"))) ; ignore whitespace

;; ibuffer

;; *Nice* buffer switching
(req-package ibuffer
  :after ibuf-ext
  :bind ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-saved-filter-groups
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
                   (name . "^\\*compilation\\*$")))
      ("shell" (or (name . "^\\*shell\\*$")
                   (name . "^\\*ansi-term\\*$")
                   (name . "^\\*terminal<\d+>\\*$")
                   (name . "^\\*eshell\\*$")))
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
  :hook
  (ibuffer . (lambda () (ibuffer-switch-to-saved-filter-groups "default")))
  :config
  (defadvice ibuffer-generate-filter-groups (after reverse-ibuffer-groups () activate)
    "Order ibuffer filter groups so the order is : [Default], [agenda], [Emacs]."
    (setq ad-return-value (nreverse ad-return-value))))

;; ace-window

(req-package ace-window
  :custom
  (aw-scope 'frame))

;; abbrev

(req-package abbrev
  :after key-chord
  :custom
  (abbrev-file-name "~/.abbrev_defs")
  (save-abbrevs 'silently)
  :config
  (abbrev-mode 1)
  (defun endless/ispell-word-then-abbrev (p)
    "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global."
    (interactive "P")
    (let (bef aft)
      (save-excursion
        (while (progn
                 (backward-word)
                 (and (setq bef (thing-at-point 'word))
                      (not (ispell-word nil 'quiet)))))
        (setq aft (thing-at-point 'word)))
      (when (and aft bef (not (equal aft bef)))
        (setq aft (downcase aft))
        (setq bef (downcase bef))
        (define-abbrev
          (if p local-abbrev-table global-abbrev-table)
          bef aft)
        (message "\"%s\" now expands to \"%s\" %sally"
                 bef aft (if p "loc" "glob")))))
  (key-chord-define-global "sx" 'endless/ispell-word-then-abbrev))

;; org

(req-package org
  ;;    :loader :elpa
  ;; NOTE: org must be manually installed from elpa / gnu since it's
  ;; require'd from init.el in order to tangle personal.org
  :bind  (("C-c l" . org-store-link)
          ("C-c b" . org-iswitchb)
          ("C-c C-." . jeff/org-timestamp-inactive))

  :custom
  (org-directory "~/Dropbox/workspace/org/")
  ;; (org-replace-disputed-keys t) ; org-CUA-compatible
  (org-log-into-drawer t)
  (org-support-shift-select 'always)
  (org-default-notes-file (concat org-directory "refile.org"))
  (org-refile-targets '(("tasks.org" :regexp . "RECURRING\\|SINGLETON")))
  (org-startup-indented t)
  (org-enforce-todo-dependencies t)
  (org-confirm-elisp-link-function nil)
  (org-src-window-setup 'current-window)
  (org-babel-python-command "python3")
  (org-edit-src-content-indentation 0)   ;; Let's have pretty source code blocks
  (org-src-tab-acts-natively t)
  (org-src-fontify-natively t)
  (org-confirm-babel-evaluate nil)
  (org-id-track-globally nil)
  (org-id-locations-file "~/Dropbox/workspace/org/.org-id-locations")

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages '((shell . t)
                               (python . t)
                               (ruby . t)
                               (dot . t)
                               (latex . t)
                               (gnuplot . t)
                               (emacs-lisp . t)))

  (add-hook 'org-mode-hook (lambda () (auto-revert-mode 1)))
  (defun jeff/org-add-ids-to-headlines-in-file ()
    "Add ID properties to all headlines in the current file which do not already have one."
    (interactive)
    (org-map-entries 'org-id-get-create))
  ;; (add-hook 'org-mode-hook
  ;;           (lambda ()
  ;;             (add-hook 'before-save-hook 'jeff/org-add-ids-to-headlines-in-file nil 'local)))

  (defun jeff/orgify-date ()
    "Convert the date in the current cell to an inactive timestamp."
    (interactive)
    (org-table-get-field nil (format " %s " (org-insert-time-stamp (org-time-string-to-time (org-read-date nil nil (org-table-get-field))) nil t))))

  (defun jeff/org-timestamp-inactive ()
    "Just like org-time-stamp, but inactive."
    (interactive)
    (org-time-stamp nil t))

  (defun jeff/yank-replace-src-block ()
    "Yank into src block, replacing contents"
    (interactive)
    (org-edit-src-code)
    (mark-whole-buffer)
    (delete-region (point) (mark))
    (yank)
    (org-edit-src-exit))

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
           (message "All subtrees checked."))))))
)

;; org superstar, indent

(req-package org-superstar
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))
(req-package org-indent)

;; ox

(req-package ox
  :after org
  :custom
  (org-html-validation-link nil)
)

;; org habit

(req-package org-habit
  :after org
  :custom
  (org-habit-following-days 1)
  (org-habit-graph-column 46))

;; htmlize

(req-package htmlize)

;; org agenda

(req-package org-agenda
  :after (org htmlize)
  :bind (("C-c a" . org-agenda))
  :custom
  (org-agenda-files (list (concat org-directory "tasks.org")
                          (concat org-directory "sauron.org")
                          (concat org-directory "jeff.org")
                          (concat org-directory "tempo.org")
                          (concat org-directory "michelle.org")))
  (org-agenda-tags-column -97)
  (org-agenda-block-separator
   (let ((retval ""))
     (dotimes (i (- org-agenda-tags-column)) (setq retval (concat retval "=")))
     retval))
  (org-agenda-search-headline-for-time nil)
  (org-agenda-window-setup 'current-window)
  (org-agenda-log-mode-items '(clock closed state))
  (org-agenda-dim-blocked-tasks nil) ; much faster!
  (org-agenda-use-tag-inheritance nil)
  (org-priority-faces '((?A . org-warning)))
  (org-agenda-exporter-settings
   '(
     ;;(org-agenda-add-entry-text-maxlines 50)
     ;;(org-agenda-with-colors nil)
     (org-agenda-write-buffer-name "Agenda")
     ;;(ps-number-of-columns 2)
     (ps-landscape-mode nil)
     (ps-print-color-p (quote black-white))
     (htmlize-output-type (quote css))))
  (org-agenda-timegrid-use-ampm t)
  (org-agenda-time-grid
   '((daily weekly today require-timed remove-match)
     (800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000)
     "........" "----------------"))

  :config
  (defun my-org-cmp-tag (a b)
    "Compare the tags of A and B, in reverse order."
    (let ((ta (mapconcat 'identity (reverse (get-text-property 1 'tags a)) ":"))
          (tb (mapconcat 'identity (reverse (get-text-property 1 'tags b)) ":")))
      (cond ((and (not ta) (not tb)) nil)
            ((not ta) -1)
            ((not tb) +1)
            ((string-lessp ta tb) -1)
            ((string-lessp tb ta) +1)
            (t nil))))

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
               (truncate
                (+ dur (org-time-to-minutes start))))))
       (start start)
       (t nil))))

  (defadvice org-agenda-add-time-grid-maybe (around mde-org-agenda-grid-tweakify
                                                    (list ndays todayp))
    (if (member 'remove-match (car org-agenda-time-grid))
        (let* ((windows (delq nil (mapcar 'org-extract-window list)))
               (org-agenda-time-grid
                (list
                 (car org-agenda-time-grid)
                 (cl-remove-if (lambda (time)
                                 (cl-find-if (lambda (w)
                                               (if (numberp w)
                                                   (equal w time)
                                                 (and (>= time (car w))
                                                      (< time (cdr w)))))
                                             windows))
                               (cadr org-agenda-time-grid))
                 (caddr org-agenda-time-grid)
                 (cadddr org-agenda-time-grid)
                 )))
          ad-do-it)
      ad-do-it))
  (ad-activate 'org-agenda-add-time-grid-maybe)
  )

;; org super agenda

(req-package org-super-agenda
  :after (org org-agenda)
  :config
  (org-super-agenda-mode 1)
  :custom
  (org-agenda-custom-commands
   '(
     ("z" "Zen View"
      ((agenda ""  (
                    (org-agenda-span 3)
                    (org-agenda-start-on-weekday 0)
                    (org-agenda-skip-scheduled-if-deadline-is-shown t)
                    (org-deadline-warning-days 0)
                    (org-agenda-hide-tags-regexp "^@")
                    (org-super-agenda-header-separator "")
                    (org-super-agenda-groups
                     '((:discard (:todo "DONE" :todo "CANCELED" :todo "SKIP"))
                       (:name "Calendar"
                              :time-grid t)
                       (:name "Habits"
                              :habit t)
                       (:name "michelle_bowen"
                              :tag "michelle_bowen")
                       (:name "@agendas"
                              :tag "@agendas")
                       (:name "@calls"
                              :tag "@calls")
                       (:name "@errands"
                              :tag "@errands")
                       (:name "@home"
                              :tag "@home")
                       (:name "@quicken"
                              :tag "@quicken")
                       (:name "@waiting"
                              :tag "@waiting")
                       (:name "@work"
                              :tag "@work")
                       (:name "other" ; "Tasks"
                              :anything t)
                       ))))
       (agenda "" (
                   (org-agenda-overriding-header "Unscheduled upcoming deadlines")
                   (org-agenda-span 1)
                   (org-agenda-time-grid nil)
                   (org-deadline-warning-days 365)
                   (org-agenda-entry-types '(:deadline))
                   (org-agenda-skip-deadline-prewarning-if-scheduled t)
                   ))
       (alltodo "" (
                    (org-agenda-overriding-header "")
                    (org-super-agenda-header-separator "")
                    (org-agenda-hide-tags-regexp "^@")
                    (org-agenda-prefix-format "  %-10T %t")
                    (org-agenda-cmp-user-defined 'my-org-cmp-tag)
                    (org-agenda-sorting-strategy '(priority-down tag-up user-defined-up alpha-up))
                    (org-super-agenda-groups
                     '((:discard (:deadline t :scheduled t))
                       (:name "Unscheduled no deadline"
                              :priority>= "C")
                       (:name "Someday"
                              :priority< "C")
                       )))))
      ) ; zen view
     ))
  )

;; origami

(req-package origami
  :after org-super-agenda
  :bind (:map org-super-agenda-header-map
              ("<tab>"  . origami-toggle-node))
  :hook
  (org-agenda-mode . origami-mode))

;; org clock

(req-package org-clock
  :after org
  :custom
  (org-clock-into-drawer t)
  :config
  (defun jeff/org-mode-ask-effort ()
    "Ask for an effort estimate when clocking in."
    (unless (org-entry-get (point) "Effort")
      (let ((effort
             (completing-read
              "Effort: "
              (org-entry-get-multivalued-property (point) "Effort"))))
        (unless (equal effort "")
          (org-set-property "Effort" effort)))))
  (add-hook 'org-clock-in-prepare-hook 'jeff/org-mode-ask-effort))

;; org capture

(req-package org-capture
  :after (org s)
  :bind (("C-c c" . org-capture))
  :config
  (defun adjust-captured-headline (hl)
    "Fixup headlines for amazon orders"
    (downcase (if (string-match "amazon\\.com order of \\(.+?\\)\\(\\.\\.\\.\\)?\\( has shipped!\\)? :" hl)
                  (let ((item (match-string 1 hl)))
                    (cond ((string-match ":@quicken:" hl) (concat "order of " item " :amazon_visa:@quicken:"))
                          ((string-match ":@waiting:" hl) (concat "delivery of " item " :amazon:@waiting:"))
                          (t hl))
                    )
                hl)))

  (add-hook 'org-capture-prepare-finalize-hook 'org-id-get-create)
  (add-hook 'org-capture-prepare-finalize-hook 'org-expiry-insert-created)

  ;; save all the agenda files after each capture
  (defun my/save-all-agenda-buffers ()
    "Function used to save all agenda buffers that are currently open, based on `org-agenda-files'."
    (interactive)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (member (buffer-file-name)
                      (mapcar 'expand-file-name (org-agenda-files t)))
          (save-buffer)))))
  (add-hook 'org-capture-after-finalize-hook 'my/save-all-agenda-buffers)

  :custom
  (org-capture-templates
   '(;; template for use by scripts, like entry.html or gmailtender
     ("b" "entry.html" entry
      (file+headline (lambda () (concat org-directory "tasks.org")) "SINGLETON")
      "* TODO %:description\n%:initial\n" :immediate-finish t)
     ;; template for habits, which include the special property
     ("h" "habit" entry
      (file+headline (lambda () (concat org-directory "tasks.org")) "SINGLETON")
      "* TODO [#C] %?\nSCHEDULED: %(s-replace \">\" \" .+1d/3d>\" \"%t\")\n:PROPERTIES:\n:STYLE: habit\n:END:\n")
     ;; a journal entry, stored in a datetree
     ("j" "journal" entry
      (file+olp+datetree (lambda () (concat org-directory "journal.org")))
      "** %U %?")
     ;; standard template, scheduled for today with average priority
     ("t" "todo" entry
      (file+headline (lambda () (concat org-directory "tasks.org")) "SINGLETON")
      "* TODO [#C] %?\nSCHEDULED: %t\n")
     ;; template for use by capture bookmarklet and emacsclient
     ;; javascript:capture('@agendas');function enc(s){return encodeURIComponent(typeof(s)=="string"?s.toLowerCase().replace(/"/g, "'"):s);};function capture(context){var re=new RegExp(/(.*) - \S+@gmail.com/);var m=re.exec(document.title);var t=m?m[1]:document.title;javascript:location.href='org-protocol://capture://w/'+encodeURIComponent(location.href)+'/'+enc(t)+' :'+context+':/'+enc(window.getSelection());}
     ("w" "org-protocol" entry
      (file+headline (lambda () (concat org-directory "tasks.org")) "SINGLETON")
      "* TODO [#C] %?%(adjust-captured-headline \"%:description\")\nSCHEDULED: %t\n:PROPERTIES:\n:END:\n%:link\n%:initial\n"))))

;; org protocol

(req-package org-protocol
  :after org-capture
  :config
  ;; We're overriding this function to get rid of the raise-window at the end,
  ;; which would switch desktops.
  (defun org-protocol-do-capture (info)
    "Perform the actual capture based on INFO."
    (let* ((temp-parts (org-protocol-parse-parameters info))
           (parts
            (cond
             ((and (listp info) (symbolp (car info))) info)
             ((= (length (car temp-parts)) 1) ;; First parameter is exactly one character long
              (org-protocol-assign-parameters temp-parts '(:template :url :title :body)))
             (t
              (org-protocol-assign-parameters temp-parts '(:url :title :body)))))
           (template (or (plist-get parts :template)
                         org-protocol-default-template-key))
           (url (and (plist-get parts :url) (org-protocol-sanitize-uri (plist-get parts :url))))
           (type (and url (if (string-match "^\\([a-z]+\\):" url)
                              (match-string 1 url))))
           (title (or (plist-get parts :title) ""))
           (region (or (plist-get parts :body) ""))
           (orglink (if url
                        (org-make-link-string
                         url (if (string-match "[^[:space:]]" title) title url))
                      title))
           (org-capture-link-is-already-stored t)) ;; avoid call to org-store-link
      (setq org-stored-links
            (cons (list url title) org-stored-links))
      (org-store-link-props :type type
                            :link url
                            :description title
                            :annotation orglink
                            :initial region
                            :query parts)
      ;; (raise-frame)
      (funcall 'org-capture nil template)))
  )

;; org capture pop frame

(req-package org-capture-pop-frame
  :custom
  (ocpf-frame-parameters
   '((name . "org-capture-pop-frame")
     (width . 132)
     (height . 14)
     (tool-bar-lines . 0)
     (menu-bar-lines . 0))))

;; org cua dwim

(req-package org-cua-dwim
  :el-get t
  :after (cua-base org)
  :config (org-cua-dwim-activate))

;; org expiry

(req-package org-expiry
  :el-get t
  :config
  (org-expiry-insinuate)
  :custom
  (org-expiry-inactive-timestamps t))          ; don't have everything in the agenda view

;; org plot

(req-package org-plot
  :after gnuplot-mode)

;; org-ehtml

(req-package web-server)

(req-package org-ehtml
  :el-get t
  :after (org web-server)
  :custom
  (org-ehtml-allow-agenda t)
  (org-ehtml-everything-editable t)
  (org-ehtml-docroot (expand-file-name "~/Dropbox/workspace/org"))

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
                  3333 nil :host (format-network-address (car (network-interface-info "ztklhxqed5")) t))
      (error (message "Failed to create web server"))))
  )

;; windmove

(req-package windmove
  :bind (("<M-wheel-up>"   . windmove-up)
         ("<M-wheel-down>" . windmove-down)
         ("<M-up>"         . windmove-up)
         ("<M-down>"       . windmove-down)
         ("<M-left>"       . windmove-left)
         ("<M-right>"      . windmove-right)))

;; shackle

(req-package shackle
  :custom
  (shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.4))))

;; atomic-chrome
;; Homepage: https://github.com/alpha22jp/atomic-chrome
;; Chrome extension: https://chrome.google.com/webstore/detail/atomic-chrome/lhaoghhllmiaaagaffababmkdllgfcmc

(req-package atomic-chrome
  :custom
  (atomic-chrome-buffer-open-style 'frame)
  (atomic-chrome-default-major-mode 'org-mode)
  :config
  (atomic-chrome-start-server)
  ;;(define-key atomic-chrome-edit-mode-map (kbd "C-c C-c") nil)
  :bind
  ;;(("s-c C-c" . atomic-chrome-close-current-buffer))
)

;; key bindings

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

(key-chord-define-global "xf" 'prelude-fullscreen)

(define-key isearch-mode-map (kbd "<f3>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-f")  'isearch-repeat-forward)

(global-set-key (kbd "<mouse-8>")       'switch-to-prev-buffer)
(global-set-key (kbd "<mouse-9>")       'switch-to-next-buffer)

;; hydra

(req-package hydra
  :after (windmove ace-window org-agenda)
  :config
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
    ("q" nil "cancel"))

  (define-key global-map
    (kbd "C-M-O") 'hydra-window/body)

  ;; from http://oremacs.com/2016/04/04/hydra-doc-syntax/

  (defun org-agenda-cts ()
    (if (bound-and-true-p org-mode)
        (let ((args (get-text-property
                     (min (1- (point-max)) (point))
                     'org-last-args)))
          (nth 2 args))
      nil))

  (defhydra hydra-org-agenda-view (:hint nil)
    "
  _d_: ?d? day        _g_: time grid=?g? _a_: arch-trees
  _w_: ?w? week       _[_: inactive      _A_: arch-files
  _t_: ?t? fortnight  _f_: follow=?f?    _r_: report=?r?
  _m_: ?m? month      _e_: entry =?e?    _D_: diary=?D?
  _y_: ?y? year       _q_: quit          _L__l__c_: ?l?"
    ("SPC" org-agenda-reset-view)
    ("d" org-agenda-day-view
     (if (eq 'day (org-agenda-cts))
         "[x]" "[ ]"))
    ("w" org-agenda-week-view
     (if (eq 'week (org-agenda-cts))
         "[x]" "[ ]"))
    ("t" org-agenda-fortnight-view
     (if (eq 'fortnight (org-agenda-cts))
         "[x]" "[ ]"))
    ("m" org-agenda-month-view
     (if (eq 'month (org-agenda-cts)) "[x]" "[ ]"))
    ("y" org-agenda-year-view
     (if (eq 'year (org-agenda-cts)) "[x]" "[ ]"))
    ("l" org-agenda-log-mode
     (format "% -3S" org-agenda-show-log))
    ("L" (org-agenda-log-mode '(4)))
    ("c" (org-agenda-log-mode 'clockcheck))
    ("f" org-agenda-follow-mode
     (format "% -3S" org-agenda-follow-mode))
    ("a" org-agenda-archives-mode)
    ("A" (org-agenda-archives-mode 'files))
    ("r" org-agenda-clockreport-mode
     (format "% -3S" org-agenda-clockreport-mode))
    ("e" org-agenda-entry-text-mode
     (format "% -3S" org-agenda-entry-text-mode))
    ("g" org-agenda-toggle-time-grid
     (format "% -3S" org-agenda-use-time-grid))
    ("D" org-agenda-toggle-diary
     (format "% -3S" org-agenda-include-diary))
    ("!" org-agenda-toggle-deadlines)
    ("["
     (let ((org-agenda-include-inactive-timestamps t))
       (org-agenda-check-type t 'timeline 'agenda)
       (org-agenda-redo)))
    ("q" (message "Abort") :exit t))

  (define-key org-agenda-mode-map
    "v" 'hydra-org-agenda-view/body)
  )

;; quicken

(defun jeff/number-lines-region (start end &optional beg)
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

(defun jeff/quicken-cleanup-uncategorized ()
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
    (jeff/number-lines-region (point) (point-max)))

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

  (let ((to (url-encode-url "Michelle Bowen <bowen.kowalski@gmail.com>"))
        (subject "quicken quiz")
        (body (url-encode-url (buffer-string))))
    (browse-url (concat "https://mail.google.com/mail/u/0/?view=cm&fs=1&tf=1"
                        "&to=" to
                        "&su=" subject
                        "&body=" body))))

;; theme

(req-package auto-dim-other-buffers
    :config
    (auto-dim-other-buffers-mode t)
    ;; adjust-dim-face added to emacs-starup-hook below
    (defun adjust-dim-face (&rest r)
      (unless (string= "unspecified-bg" (face-attribute 'default :background))
        (set-face-attribute 'auto-dim-other-buffers-face nil
                            :background (color-darken-name
                                         (face-attribute 'default :background) 3))))
    (defun adob--ignore-buffer (buffer)
      "Return whether to ignore BUFFER and do not affect its state.
  Currently only mini buffer, echo areas, and helm are ignored."
      (or (null buffer)
          (minibufferp buffer)
          (string-match "^ \\*Echo Area" (buffer-name buffer))
          (string-match "\\*helm" (buffer-name buffer))
          (string-match "\\*Minibuf" (buffer-name buffer))
          )))

  (req-package dimmer
    :custom
    (dimmer-fraction 0.50)
    :config
    (dimmer-mode 1))

  (req-package custom
    :custom
    (custom-safe-themes t))

  (req-package solarized-theme
    :after custom
    :chords (("xd" . (lambda () (interactive) (load-theme 'solarized-gruvbox-dark) (set-face-attribute 'org-agenda-date nil :box '(:line-width 1) :height 1.1)))
             ("xl" . (lambda () (interactive) (load-theme 'solarized-gruvbox-light) (set-face-attribute 'org-agenda-date nil :box '(:line-width 1) :height 1.1))))
    :custom
    (solarized-high-contrast-mode-line nil)
    (solarized-scale-org-headlines t)
    (x-underline-at-descent-line t)
    :config
    (defun solarized nil
      "Enable solarized theme"
      (interactive)
      (disable-theme 'zenburn)
      (load-theme 'solarized-gruvbox-dark t)
      (set-face-attribute 'org-agenda-date nil :box '(:line-width 1) :height 1.1)))

  (req-package zenburn-theme
    :after custom
    :config
    (defun zenburn nil
      "Enable zenburn theme"
      (interactive)
      (disable-theme 'solarized-gruvbox-dark)
      (load-theme 'zenburn t)
      (set-face-attribute 'org-agenda-date nil :box '(:line-width 1) :height 1.1)))


(add-hook 'emacs-startup-hook
          '(lambda ()
             (progn
               (advice-add 'load-theme :after #'adjust-dim-face)
               (if (tty-type (frame-terminal)) (zenburn) (solarized)))))

;; modeline
;; Use [[https://github.com/seagle0128/doom-modeline][doom-modeline]], a fancy and fast mode-line inspired by minimalism design.
;; This package requires the fonts included with ~all-the-icons~ to be installed. Run /M-x all-the-icons-install-fonts/ to do so. Please refer to the [[https://github.com/domtronn/all-the-icons.el#installation][installation guide]].

(req-package doom-modeline
  :custom
  (doom-modeline-minor-modes t)
  (doom-modeline-icon t) ;; required if daemonizing, per [[https://github.com/seagle0128/doom-modeline/issues/357][comments]]
  :config
  (doom-modeline-mode))

(req-package minions
  :config (minions-mode))
(display-time)
(column-number-mode)
(size-indication-mode)

;; exwm

(defun efs/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun efs/set-wallpaper ()
  (interactive)
  ;; NOTE: You will need to update this to a valid background path!
  (start-process-shell-command
   "feh" nil  "feh --bg-scale /home/jeff/Dropbox/sync-linux/drifts.jpg"))

(defun efs/exwm-init-hook ()
  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 1)

  ;; Open eshell by default
  ;;(eshell)

  ;; Show battery status in the mode line
  (display-battery-mode 1)

  ;; Show the time and date in modeline
  (setq display-time-day-and-date t)
  (display-time-mode 1)
  ;; Also take a look at display-time-format and format-time-string

  ;; Launch apps that will run in the background
  (efs/run-in-background "nm-applet")
  ;;  (efs/run-in-background "pasystray")
  ;;  (efs/run-in-background "blueman-applet")
  )

(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun jeff/exwm-workspace-switch-to-next ()
  (interactive)
  "Switch to the next active workspace."
  (let ((index (mod (+ exwm-workspace-current-index 1) exwm-workspace-number)))
    (exwm-workspace-switch index)))

(defun jeff/exwm-workspace-switch-to-prior ()
  (interactive)
  "Switch to the next active workspace."
  (let ((index (mod (- exwm-workspace-current-index 1) exwm-workspace-number)))
    (exwm-workspace-switch index)))

(defun jeff/exwm-workspace-move-window-to-next ()
  (interactive)
  "Switch to the next active workspace."
  (let ((index (mod (+ exwm-workspace-current-index 1) exwm-workspace-number)))
    (progn
      (exwm-workspace-move-window index)
      (exwm-workspace-switch index))))

(defun jeff/exwm-workspace-move-window-to-prior ()
  (interactive)
  "Switch to the next active workspace."
  (let ((index (mod (- exwm-workspace-current-index 1) exwm-workspace-number)))
    (progn
      (exwm-workspace-move-window index)
      (exwm-workspace-switch index))))

(req-package exwm
  :disabled t  ;; FIXME
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 5)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

  ;; When EXWM starts up, do some extra confifuration
  (add-hook 'exwm-init-hook #'efs/exwm-init-hook)

  ;; Rebind CapsLock to Ctrl
;;  (start-process-shell-command "xmodmap" nil "xmodmap ~/.emacs.d/exwm/Xmodmap")

  ;; Set the screen resolution (update this to be the correct resolution for your screen!)
  (require 'exwm-randr)
  (exwm-randr-enable)
  (start-process-shell-command "xrandr" nil "xrandr --output Virtual-1 --primary --mode 1366x768 --pos 0x0 --rotate normal")

  ;; Set the wallpaper after changing the resolution
  (efs/set-wallpaper)

  ;; Load the system tray before exwm-init
  (require 'exwm-systemtray)
  (setq exwm-systemtray-height 16)
  (exwm-systemtray-enable)

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\C-\M-j  ;; Buffer list
          ?\C-\ ))  ;; Ctrl+Space

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)

          ;; Move between workspaces
          ([M-s-left]    . jeff/exwm-workspace-switch-to-prior)
          ([M-s-right]   . jeff/exwm-workspace-switch-to-next)
          ([M-S-s-left]  . jeff/exwm-workspace-move-window-to-prior)
          ([M-S-s-right] . jeff/exwm-workspace-move-window-to-next)

          ;; Move between windows
          ([s-left]  . windmove-left)
          ([s-right] . windmove-right)
          ([s-up]    . windmove-up)
          ([s-down]  . windmove-down)

          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)
  (display-battery-mode)
  (exwm-enable)
)

;; organizer

(defun jeff/organizer ()
  "Show schedule in fullscreen on second desktop."
  (interactive)
  (run-with-idle-timer 1 nil
                       (lambda () (org-agenda nil "z")
                         ;; move our window to second desktop (-t 1)
                         (shell-command (format "wmctrl -t 1 -i -r %s"
                                                (shell-command-to-string
                                                 (format "wmctrl -l -x -p | fgrep %d | awk '{ printf \"%%s\", $1; }'" (emacs-pid)))))
                         (toggle-frame-fullscreen)
                         ))
  t)

;; finish

(req-package-finish)

(use-package-report)

(provide 'personal)
;;; personal.el ends here
