;;; personal.el -- emacs personalization file
;;; Commentary:
;;; Code:
;;;


;; ----------------------------------------------------------- [ shell / eshell ]

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (let ((default-directory (getenv "HOME")))
                (command-execute 'eshell)
                (bury-buffer))))

;; (add-hook 'eshell-mode-hook
;;           #'(lambda ()
;;               (define-key eshell-mode-map
;;                 [remap pcomplete]
;;                 'helm-esh-pcomplete)))
;; (add-hook 'eshell-mode-hook
;;           #'(lambda ()
;;               (define-key eshell-mode-map
;;                 (kbd "M-p")
;;                 'helm-eshell-history)))


;; ----------------------------------------------------------- [ emacs prelude ]

(add-hook 'prelude-prog-mode-hook
          (lambda ()
            (guru-mode -1)
            (whitespace-mode -1)) t)

;; re-tabbing during whitespace-cleanup would kill makefiles
(add-hook 'makefile-mode-hook
          (lambda ()  (remove-hook 'before-save-hook 'whitespace-cleanup t)))

;; FIXME - this is referenced from smartparens, and used to be in cua-base, but is no longer there
;; https://github.com/Fuco1/smartparens/issues/271
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


;; ----------------------------------------------------------- [ packages ]

;; (add-to-list 'package-archives '("technomancy" . "http://repo.technomancy.us/emacs/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; install the missing packages
(dolist (p '(;;ack-and-a-half
             cmake-mode
             cperl-mode
             dired+
             dired-single
             ;;expand-region       ; prelude
             ;;gh                  ; required by gist
             ;;gist                ; prelude
             ;;guru-mode           ; prelude
             http-post-simple
             ;;helm                ; prelude
             ;;helm-projectile     ; prelude
             helm-swoop
             htmlize               ; for org-ehtml
             image+
             ;;logito              ; required by gh
             ;;magit               ; prelude
             ;;magithub            ; prelude
             ;;melpa               ; prelude
             ;;nyan-mode
             org
             org-cua-dwim
             ;;paredit             ; required by prelude-lisp
             ;;pcache              ; required by gh
             ;;prelude-emacs-lisp  ; prelude auto
             ;;prelude-js          ; prelude auto
             ;;prelude-lisp        ; required by prelude-emacs-lisp
             ;;prelude-perl        ; prelude auto
             ;;prelude-programming
             ;;prelude-xml         ; prelude auto
             ;;projectile          ; required by helm-projectile
             ;;python              ; prelude auto
             ;;rainbow-delimiters  ; required by prelude-lisp
             ;;rainbow-mode        ; prelude
             smex
             sublimity
             ;;volatile-highlights ; prelude
             ;;workgroups
             ;;zenburn-theme       ; prelude
             ))
  (unless (package-installed-p p)
    (package-install p)))

(defalias 'perl-mode 'cperl-mode)
(define-key package-menu-mode-map "o" 'delete-other-windows)


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
        (:name web-server
               :description "web server running Emacs Lisp handlers"
               :type git
               :url "https://github.com/eschulte/emacs-web-server.git"
               :features web-server)
        (:name org-ehtml
               :description "Export Org-mode files as editable web pages"
               :type git
               :url "https://github.com/jeffkowalski/org-ehtml.git"
               :depends web-server
               :load-path "src")
        (:name nyan-mode
               :description "Nyan Cat for Emacs! Nyanyanyanyanyanyanyanyanyan!"
               :type git
               :url "https://github.com/jeffkowalski/nyan-mode.git"
               :features nyan-mode)
        ))

;; now set our own packages
(let ((el-get-packages
       '(el-get				; el-get is self-hosting
         arduino-mode
         evernote-mode
         web-server
         org-ehtml
         nyan-mode
         )))
  (el-get 'sync el-get-packages))

;; ----------------------------------------------------------- [ automodes ]

(add-to-list 'auto-mode-alist '("\\.ahk$" . ahk-mode))
(autoload 'ahk-mode "ahk-mode") ;; http://www.robf.de/Hacking/elisp/ahk-mode.el
(require 'compile)


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


;; ----------------------------------------------------------- [ kill line ]

(defun kill-line-or-region ()
  "Similar to 'kill-line', but will kill the region if one exists."
  (interactive)
  (kill-region (point)
               ;; It is better to move point to the other end of the kill
               ;; before killing.  That way, in a read-only buffer, point
               ;; moves across the text that is copied to the kill ring.
               ;; The choice has no effect on undo now that undo records
               ;; the value of point from before the command was run.
               (if (region-active-p)
                   (mark t)    ; if there's an active mark, kill region instead
                 (progn
                   (forward-visible-line 1)
                   (if (eobp)
                       (signal 'end-of-buffer nil))
                   (if (or (looking-at "[ \t]*$") (and kill-whole-line (bolp)))
                       (forward-visible-line 1)
                     (end-of-visible-line))
                   (point)))))

;;(global-set-key (kbd "C-k")             'kill-line-or-region)

(defun kill-to-end-of-buffer ()
  "Kill all text from point to end of buffer."
  (interactive)
  (kill-region (point) (point-max)))

;; ----------------------------------------------------------- [ adornments ]

(setq frame-title-format '(buffer-file-name "emacs - %f %*" ("%b %*")))
(setq icon-title-format '(buffer-file-name "emacs - %f %*" ("%b %*")))
(display-time)
(set-default 'cursor-type '(bar . 2))
(blink-cursor-mode)
(setq redisplay-dont-pause t)
(scroll-bar-mode -1)

;; hide trailing whitespaces in some programming modes:
(mapc (lambda (hook)
        (add-hook hook (lambda ()
                         (setq show-trailing-whitespace nil))))
      '(eshell-mode-hook term-mode-hook))


;; ----------------------------------------------------------- [ undo-tree ]

(require 'undo-tree)
(global-undo-tree-mode)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo-tree-undo)
;(global-unset-key (kbd "C-y"))
;(global-set-key (kbd "C-y") 'undo-tree-redo)


;; ----------------------------------------------------------- [ image+ ]

(require 'image+)
(imagex-global-sticky-mode)
(imagex-auto-adjust-mode)
(let ((map imagex-sticky-mode-map))
  (define-key map "+" 'imagex-sticky-zoom-in)
  (define-key map "-" 'imagex-sticky-zoom-out)
  (define-key map "l" 'imagex-sticky-rotate-left)
  (define-key map "r" 'imagex-sticky-rotate-right)
  (define-key map "m" 'imagex-sticky-maximize)
  (define-key map "o" 'imagex-sticky-restore-original)
  (define-key map "\C-x\C-s" 'imagex-sticky-save-image))


;; ----------------------------------------------------------- [ dired ]

(eval-when-compile
  (require 'dired)
  (require 'dired+))

(autoload 'dired-single-buffer "dired-single" "" t)
(autoload 'dired-single-buffer-mouse "dired-single" "" t)

(add-hook 'dired-mode-hook
          (lambda ()
            ;; Enable all commands
            (setq disabled-command-function nil)

            (define-key dired-mode-map [return] 'dired-single-buffer)
            (define-key dired-mode-map [down-mouse-1] 'dired-single-buffer-mouse)
            (define-key dired-mode-map [^]
              (lambda ()
                (interactive)
                (dired-single-buffer "..")))

            ;; Auto-refresh dired on file change
            (auto-revert-mode)
            (setq-default auto-revert-interval 1)

            ;; Hide dired current directory (.)
            (require 'dired+)
            ;; Fix color theme
            (setq-default dired-omit-files-p t)
            (setq font-lock-maximum-decoration (quote ((dired-mode) (t . t)))
                  dired-omit-files (concat dired-omit-files "\\."))))


;; ----------------------------------------------------------- [ company ]

(eval-when-compile
  (require 'company))

(add-to-list 'company-backends 'company-dabbrev t)
(add-to-list 'company-backends 'company-ispell t)
(add-to-list 'company-backends 'company-files t)
(add-to-list 'company-transformers 'company-sort-by-occurrence)

(defun my-pcomplete-capf ()
  "Org-mode completions."
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
(add-hook 'org-mode-hook #'my-pcomplete-capf)


;; ----------------------------------------------------------- [ tramp ]

;; disable version control checks
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(defun jeff/find-file-as-root ()
  "Like 'helm-find-file', but automatically edit the file with root-privileges (using tramp/sudo), if the file is not writable by user."
  (interactive)
  (let ((file (helm-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))
(global-set-key (kbd "C-x F") 'jeff/find-file-as-root)


;; ----------------------------------------------------------- [ ido ]

(eval-when-compile
  (require 'ido))

(defun ido-disable-line-trucation ()
  "Locally disable 'truncate-lines'."
  (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

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
            (define-key ido-completion-map (kbd "<down>") 'ido-next-match)))


;; ----------------------------------------------------------- [ ibuffer ]

;; *Nice* buffer switching
(eval-when-compile
  (require 'ibuffer)
  (require 'ibuf-ext))

(add-hook 'ibuffer-mode-hook
          (lambda ()
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
            (ibuffer-switch-to-saved-filter-groups "default")))

(defadvice ibuffer-generate-filter-groups (after reverse-ibuffer-groups () activate)
  "Order ibuffer filter groups so the order is : [Default], [agenda], [Emacs]."
  (setq ad-return-value (nreverse ad-return-value)))

;; ----------------------------------------------------------- [ org-mode ]

(eval-when-compile
  (require 'org)
  (require 'org-mobile)
  (require 'org-clock)
  (require 'org-capture))

(setq org-directory "~/Dropbox/workspace/org/")
(setq org-mobile-directory "~/Dropbox/mobileorg/")
(setq org-mobile-agendas '("a"))
(setq org-agenda-files (list (concat org-directory "toodledo.org")
                             (concat org-directory "sauron.org")
                             (concat org-directory "gcal.org")))
(setq org-mobile-inbox-for-pull (concat org-mobile-directory "flagged.org"))

(setq org-startup-indented t)
;;; (setq org-replace-disputed-keys t) ; org-CUA-compatible
(setq org-support-shift-select t)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-agenda-tags-column -97)
(setq org-agenda-block-separator (let ((retval ""))
                                   (dotimes (i (- org-agenda-tags-column)) (setq retval (concat retval "=")))
                                   retval))
(setq org-agenda-timegrid-use-ampm t)
(add-hook 'org-finalize-agenda-hook
          (lambda () (remove-text-properties
                      (point-min) (point-max) '(mouse-face t))))

(defun my-org-mode-ask-effort ()
  "Ask for an effort estimate when clocking in."
  (unless (org-entry-get (point) "Effort")
    (let ((effort
           (completing-read
            "Effort: "
            (org-entry-get-multivalued-property (point) "Effort"))))
      (unless (equal effort "")
        (org-set-property "Effort" effort)))))

(add-hook 'org-clock-in-prepare-hook
          'my-org-mode-ask-effort)

(setq org-log-into-drawer t)
(setq org-clock-into-drawer t)

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

(setq org-agenda-custom-commands
      '(("s" "Startup View"
         ((agenda ""     ((org-agenda-ndays 3)
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

(setq org-default-notes-file (concat org-directory "refile.org"))

(setq org-agenda-exporter-settings
      '(
        ;;(org-agenda-add-entry-text-maxlines 50)
        ;;(org-agenda-with-colors nil)
        (org-agenda-write-buffer-name "Agenda")
        ;;(ps-number-of-columns 2)
        (ps-landscape-mode nil)
        (ps-print-color-p (quote black-white))
        (htmlize-output-type (quote css))))

(require 'org-protocol)
(setq org-capture-templates
      (quote (("b" "entry.html" entry (file+headline (concat org-directory "toodledo.org") "TASKS")
               "* TODO [#C] %:description\nSCHEDULED: %t\n%:initial\n"
               :immediate-finish t)
              ("s" "sacha" entry (file+headline (concat org-directory "toodledo.org") "TASKS")
               "* TODO [#C] %^{Task}    %^g
SCHEDULED: %^t
%?
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:")
              ("t" "todo" entry (file+headline (concat org-directory "toodledo.org") "TASKS")
               "* TODO [#C] %?\n")
              ("w" "org-protocol" entry (file+headline (concat org-directory "toodledo.org") "TASKS")
               "* TODO [#C] %:description\nSCHEDULED: %t\n%:link\n%:initial\n")
              ("h" "Habit" entry (file+headline (concat org-directory "toodledo.org") "TASKS")
               "* TODO [#C] %?\nSCHEDULED: %t .+1d/3d\n:PROPERTIES:\n:STYLE: habit\n:END:\n"))))
(global-set-key (kbd "C-M-r") 'org-capture)
(global-set-key (kbd "C-c r") 'org-capture)

(defun kiwon/org-agenda-redo-in-other-window ()
  "Call org-agenda-redo function even in the non-agenda buffer."
  (interactive)
  (let ((agenda-window (get-buffer-window org-agenda-buffer-name t)))
    (when agenda-window
      (with-selected-window agenda-window (org-agenda-redo)))))
                                        ;(run-at-time nil 60 'kiwon/org-agenda-redo-in-other-window)

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

(defun jeff/organizer ()
  "Show schedule in fullscreen."
  (interactive)
  (toggle-frame-fullscreen)
  (org-agenda nil "s"))

(defun jeff/org-agenda-edit-headline ()
  "Go to the Org-mode file containing the item at point, then mark headline for overwriting."
  (interactive)
  (org-agenda-goto)
  (search-backward (org-get-heading t t))
  (push-mark)
  (goto-char (match-end 0))
  (activate-mark))

(require 'org-agenda)
(define-key org-agenda-mode-map (kbd "h") 'jeff/org-agenda-edit-headline)

(add-hook 'org-agenda-mode-hook
          (lambda ()
            (whitespace-mode -1)) t)

(require 'org-cua-dwim)
(org-cua-dwim-activate)


;; ----------------------------------------------------------- [ org-ehtml ]

(eval-when-compile
  (require 'org-ehtml))

(setq
 org-ehtml-everything-editable t
 org-ehtml-allow-agenda t
 org-ehtml-docroot (expand-file-name "~/Dropbox/workspace/org"))

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

(setq jeff/org-ehtml-handler
  '(((:GET  . "/capture") . jeff/capture-handler)
    ((:GET  . ".*") . org-ehtml-file-handler)
    ((:POST . ".*") . org-ehtml-edit-handler)))

(when t
  (mapc (lambda (server)
          (if (= 3333 (port server))
              (ws-stop server)))
        ws-servers)
  (condition-case-unless-debug nil
      (ws-start jeff/org-ehtml-handler 3333)
    (error (message "Failed to create web server"))))


;; ----------------------------------------------------------- [ evernote ]

(require 'evernote-mode)
(setq evernote-developer-token "S=s1:U=81f:E=1470997a804:C=13fb1e67c09:P=1cd:A=en-devtoken:V=2:H=0b3aafa546daa4a9b43c77a7574390d4")
(setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; optional
                                        ;(setq evernote-username "jeffkowalski") ; optional: you can use this username as default.
                                        ;(setq evernote-password-cache t)
                                        ;(setq enh-password-cache-file "~/.emacs-evernote-mode.gpg")
(setq enh-enclient-command "/home/jeff/Dropbox/workspace/evernote-mode/ruby/bin/enclient.rb")
                                        ;(setq evernote-developer-token "dd1e37ae18197b8e")
(global-set-key "\C-cEc" 'evernote-create-note)
(global-set-key "\C-cEo" 'evernote-open-note)
(global-set-key "\C-cEs" 'evernote-search-notes)
(global-set-key "\C-cES" 'evernote-do-saved-search)
(global-set-key "\C-cEw" 'evernote-write-note)
(global-set-key "\C-cEp" 'evernote-post-region)
(global-set-key "\C-cEb" 'evernote-browser)


;; ----------------------------------------------------------- [ modeline ]

(set-face-attribute 'mode-line nil :box nil)

;; Mode line setup, after http://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
(setq-default
 mode-line-format
 '(
   ;; Position, including warning for 80 columns
   (:propertize "%4l:" face mode-line-position-face)
   (:eval (propertize "%3c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   " "
   (-3 :eval (propertize (format "%2d%%%%"
                                 (if (= (point-max) (point-min))
                                     100
                                   (min 100 (/ (- (point) (point-min) 1) (/ (- (point-max) (point-min)) 100))))
                                 )
                         'face
                         (if (let* ((beg (point-min))
                                    (end (point-max))
                                    (total (buffer-size)))
                               (or (/= beg 1) (/= end (1+ total))))
                             'mode-line-narrow-face
                           'mode-line-position-face)))

   ;; emacsclient [default -- keep?]
   ;;mode-line-client
   " "
   ;; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " ** " 'face 'mode-line-modified-face))
          (t
           (propertize "    " 'face 'mode-line-unmodified-face))))
   " "
   ;; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 20))
                face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)
   ;; narrow [default -- keep?]
   ;; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   " "
   (:eval (when vc-mode (concat vc-mode " ")))
   " %[["
   (:propertize mode-name
                face mode-line-mode-face)
   "%]]"
   (:propertize mode-line-process
                face mode-line-process-face)
   " "
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   "  "
   (global-mode-string global-mode-string)
   "  "
   ;; nyan-mode uses nyan cat as an alternative to %p
   (:eval (when nyan-mode (list (nyan-create))))
   )
 )

;; Helper function
(defun shorten-directory (dir max-length)
  "Show directory name DIR up to MAX-LENGTH characters."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Extra mode line faces
(defvar mono-mode-line-family "Mono")
(set-face-attribute 'mode-line nil
                    :foreground "gray80" :background "gray15"
                    :inverse-video nil
                    :box '(:line-width 6 :color "gray15" :style nil))

(set-face-attribute 'mode-line-inactive nil
                    :foreground "gray60" :background "gray30"
                    :inverse-video nil
                    :box '(:line-width 6 :color "gray30" :style nil))

(make-face 'mode-line-position-face)
(set-face-attribute 'mode-line-position-face nil
                    :inherit 'mode-line-face
                    :family 'mono-mode-line-family)

(make-face 'mode-line-80col-face)
(set-face-attribute 'mode-line-80col-face nil
                    :inherit 'mode-line-position-face
                    :foreground "black" :background "#eab700")

(make-face 'mode-line-narrow-face)
(set-face-attribute 'mode-line-narrow-face nil
                    :inherit 'mode-line-position-face
                    :box '(:line-width 2 :color "gray40"))

(make-face 'mode-line-unmodified-face)
(set-face-attribute 'mode-line-unmodified-face nil
                    :inherit 'mode-line-face
                    :family 'mono-mode-line-family :height 110)

(make-face 'mode-line-read-only-face)
(set-face-attribute 'mode-line-read-only-face nil
                    :inherit 'mode-line-face
                    :foreground "#4271ae"
                    :family 'mono-mode-line-family :height 110
                    :box '(:line-width 2 :color "#4271ae"))

(make-face 'mode-line-modified-face)
(set-face-attribute 'mode-line-modified-face nil
                    :inherit 'mode-line-face
                    :foreground "#c82829"
                    ;; :background "#ffffff"
                    :family 'mono-mode-line-family :height 110
                    :box '(:line-width 2 :color "#c82829"))

(make-face 'mode-line-folder-face)
(set-face-attribute 'mode-line-folder-face nil
                    :inherit 'mode-line-face
                    :foreground "gray60")

(make-face 'mode-line-filename-face)
(set-face-attribute 'mode-line-filename-face nil
                    :inherit 'mode-line-face
                    :foreground "#eab700"
                    :weight 'bold)

(make-face 'mode-line-mode-face)
(set-face-attribute 'mode-line-mode-face nil
                    :inherit 'mode-line-face
                    :foreground "gray80")

(make-face 'mode-line-minor-mode-face)
(set-face-attribute 'mode-line-minor-mode-face nil
                    :inherit 'mode-line-mode-face
                    :foreground "gray40")

(make-face 'mode-line-process-face)
(set-face-attribute 'mode-line-process-face nil
                    :inherit 'mode-line-face
                    :foreground "#718c00")


;; ----------------------------------------------------------- [ key bindings ]

(define-key special-event-map [delete-frame] 'save-buffers-kill-terminal)
(global-set-key (kbd "<M-f4>")          'save-buffers-kill-terminal)
;; (global-set-key (kbd "C-o")             'find-file)
;; (global-set-key (kbd "C-f")             'isearch-forward)
;; (global-set-key (kbd "<f3>")            'isearch-repeat-forward)
(global-set-key (kbd "<f4>")            'next-error)
(global-set-key (kbd "<f5>")            'recompile)
(global-set-key (kbd "<f7>")            'goto-line)
(global-set-key (kbd "<f10>")           'eval-last-sexp)
;; (global-set-key (kbd "C-s")             'save-buffer)
(global-set-key (kbd "C-w")             'kill-buffer-and-window)
;; (global-set-key (kbd "C-l")             'goto-line)
(global-set-key (kbd "RET")             'newline-and-indent)
(global-set-key (kbd "C-S-a")           'mark-whole-buffer)
(global-set-key (kbd "<C-next>")        'scroll-other-window)
(global-set-key (kbd "<C-prior>")       'scroll-other-window-down)
(global-set-key (kbd "<C-tab>")         'next-buffer)
(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-buffer)
(global-set-key (kbd "<M-wheel-up>")    'windmove-up)
(global-set-key (kbd "<M-wheel-down>")  'windmove-down)
(global-set-key (kbd "<M-up>")          'windmove-up)
(global-set-key (kbd "<M-down>")        'windmove-down)
(global-set-key (kbd "<M-left>")        'windmove-left)
(global-set-key (kbd "<M-right>")       'windmove-right)
;; (global-set-key (kbd "M-x")             'helm-M-x)
(global-set-key (kbd "M-i")             'helm-swoop)

(define-key isearch-mode-map (kbd "<f3>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-f")  'isearch-repeat-forward)

(defvar ALT-F-map (make-sparse-keymap) "ALT-F keymap.")
(global-set-key [(alt f)]               ALT-F-map)
(global-set-key [(meta f)]              ALT-F-map)
(define-key ALT-F-map "s"               'save-buffer)
(define-key ALT-F-map "x"               'save-buffers-kill-emacs)
(define-key ALT-F-map "o"               'find-file)
(define-key ALT-F-map "c"               'kill-current-buffer)

(global-set-key (kbd "C-x C-f")         'helm-find-files)
(global-set-key (kbd "M-x")             'helm-M-x)
(global-set-key (kbd "C-x b")           'helm-buffers-list)

(global-set-key (kbd "<mouse-8>")       'switch-to-prev-buffer)
(global-set-key (kbd "<mouse-9>")       'switch-to-next-buffer)

;; fix prelude behavior on terminals that send ^[O{ABCD} for arrows
(defvar ALT-O-map (make-sparse-keymap) "ALT-O keymap.")
(define-key prelude-mode-map (kbd "M-O") ALT-O-map)


;; ----------------------------------------------------------- [ experimental ]

(setq enable-recursive-minibuffers t)

;; FIXME: workaround problem in CUA which doesn't seem to obey delete-selection
;;        behavior on paste
(defadvice cua-paste (before clobber-region
                                 (&optional arg))
  "Delete the region before pasting."
  (when (region-active-p) (delete-region (region-beginning) (region-end)))
  )
(ad-activate 'cua-paste)

(provide 'personal)
;;; personal.el ends here
