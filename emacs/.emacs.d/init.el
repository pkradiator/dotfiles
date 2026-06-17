;;; Packages I have Installed
;; speed-type (installed manually)


(require 'package)

(setq gc-cons-threshold (* 50 1000 1000))


;; Tell us how fast we're going, for benchmarking
(defun pkd/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'pkd/display-startup-time)
(load-theme 'modus-vivendi-tinted)

;; if emacs version is less than 27, load the autoloads of files in load-path
;; is done automatically after version 27
(when (< emacs-major-version 27) (package-initialize))

;; Install and load use-package, if not done already.
(unless (package-installed-p 'use-package)
  (package-install use-package))
(require 'use-package)
(setq use-package-always-defer 1)

;; Garbage produced by Custom in a separate file
(setq custom-file "~/.emacs.d/emacs-custom.el")
(if (file-readable-p custom-file)
    (load custom-file))

;; Add melpa
(add-to-list 'package-archives 
	     '("MELPA" .
	       "http://melpa.org/packages/"))

;; Full screen on startup
(toggle-frame-fullscreen)
(when (eq system-type 'windows-nt)
  (add-to-list 'after-make-frame-functions 'toggle-frame-fullscreen))

(use-package which-key
  :ensure t
  :demand t
  :config
  (which-key-mode))

;; On Mac keyboard Command -> Meta
(setq mac-command-modifier 'meta)
;;(setq mac-option-modifier 'control)

;; I prefer spaces over tabs
(setq-default indent-tabs-mode nil)
;;Add content of clipboard(things you copy outside emacs) to kill ring (emacs clipboard)
(setq save-interprogram-paste-before-kill t)

;; Menu Bar is actually useful on macos
(cond
 ((eq system-type 'windows-nt)
       (menu-bar-mode -1)
       (set-message-beep 'silent)
       ;setq package-gnupghome-dir "/c/Users/userName/.emacs.d/elpa/gnupg/")
       ;(add-to-list 'exec-path "c:/Program Files/Git/usr/bin")
       ;(setenv "PATH" "c:/Program Files/Git/usr/bin")
)
 ((eq system-type 'gnu/linux)
  (menu-bar-mode -1)
  (setq ring-bell-function 'ignore)))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(setq tab-bar-tab-hints t)

;; Show column number on the modeline.
(column-number-mode)

(use-package tab-bar
  :ensure nil
  :init
  (setq tab-bar-show 1)
  :config
  (tab-bar-mode 1)
  (tab-bar-history-mode 1)

  ;; Define the repeat map for tab history navigation
  (defvar tab-bar-history-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "p") 'tab-bar-history-back)
      (define-key map (kbd "n") 'tab-bar-history-forward)
      map))

  ;; Associate the commands with the repeat map
  (put 'tab-bar-history-back 'repeat-map 'tab-bar-history-repeat-map)
  (put 'tab-bar-history-forward 'repeat-map 'tab-bar-history-repeat-map)

  :bind
  (:map tab-bar-history-mode-map
        ("C-x w p" . tab-bar-history-back)
        ("C-x w n" . tab-bar-history-forward)))

(defun my/smart-next-line (&optional arg try-vscroll)
  "Move down. Uses logical lines if a prefix ARG is provided, else visual."
  (interactive "^p\np")
  (let ((line-move-visual (if current-prefix-arg nil line-move-visual)))
    (next-line arg try-vscroll)))

(defun my/smart-previous-line (&optional arg try-vscroll)
  "Move up. Uses logical lines if a prefix ARG is provided, else visual."
  (interactive "^p\np")
  (let ((line-move-visual (if current-prefix-arg nil line-move-visual)))
    (previous-line arg try-vscroll)))

(global-set-key (kbd "C-n") 'my/smart-next-line)
(global-set-key (kbd "C-p") 'my/smart-previous-line)

(use-package change-inner
  :ensure t
  :bind (("C-x c i" . change-inner)
         ("C-x c a" . change-outer)))

(use-package icomplete
  :ensure nil
  :demand t
  :config
  (icomplete-vertical-mode t)
  (setq icomplete-show-matches-on-no-input t)
  (setq completion-styles '(substring initials flex))
  (setq completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (define-key icomplete-minibuffer-map (kbd "<tab>") 'icomplete-force-complete)
  (define-key icomplete-minibuffer-map (kbd "C-j") 'minibuffer-complete)
  (define-key icomplete-minibuffer-map (kbd "<S-return>") 'exit-minibuffer)
  (define-key icomplete-minibuffer-map (kbd "RET") 'icomplete-force-complete-and-exit))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))


;; Display line numbers in the left margin, as a general rule,
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
;; but disable them where they just add clutter, e.g. shell.
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                eshell-mode-hook
                Info-mode-hook
                ement-room-mode-hook
                elfeed-show-mode-hook
                doc-view-mode-hook
                pdf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Modeline clock and CPU load level.
(display-time)

;;Theme:- Modus-vivendi or Modus-operandi
;;(setq modus-themes-italic-constructs t
;;           modus-themes-bold-constructs nil
;;           modus-themes-region '(bg-only no-extend))
;; Load the theme of your choice:
;;(load-theme 'modus-operandi)

(use-package auctex
  :ensure t)

;; Increase the quality of Docview
(setq doc-view-resolution 300)

;; Don't litter all over the place with #file# and file~
;; TODO : Improve this shit
(custom-set-variables
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))
(make-directory "~/.emacs.d/autosaves/" t)

;; Get the environment variables, especially path
(use-package exec-path-from-shell
  :ensure t
  :defer nil
  :config
  (when (eq system-type 'darwin)(exec-path-from-shell-initialize)))

;;Annoying windows solved
(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode
	  occur-mode
	  grep-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

;;; Org Mode:-
(defun pkd/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1)
  (modify-syntax-entry ?< ".")
  (modify-syntax-entry ?> "."))

(use-package cdlatex
  :ensure t
  :hook ((org-mode . turn-on-org-cdlatex)
	 (LaTeX-mode . turn-on-cdlatex)))

(use-package org
  :hook ((org-mode . pkd/org-mode-setup)
	 (org-mode . turn-on-org-cdlatex))
  
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :config
  (setq org-directory "~/kp7/org")
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-insert-heading-respect-content t)
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  ;; Org Capture templates
  (setq org-capture-templates
        '(;; Frictionless drop — when you're not sure where it belongs
          ("n" "Inbox" entry (file+headline "~/kp7/org/life.org" "Inbox")
           "* %?\n %U\n %i\n")

          ;; Tasks
          ("t" "Todo" entry (file+headline "~/kp7/org/life.org" "Tasks")
           "* TODO %?\n %U\n %i\n")

          ;; Events
          ("e" "Event" entry (file+headline "~/kp7/org/life.org" "Events")
           "* %?\n %T\n %i\n" :tag "event")

          ;; Reading / Links
          ("a" "Article/Link" entry (file+headline "~/kp7/org/life.org" "Reading")
           "* TODO Read: %?\n :PROPERTIES:\n :URL: %x\n :END:\n %U\n" :tag "read")

          ;; Ideas to explore
          ("i" "Idea" entry (file+headline "~/kp7/org/life.org" "Ideas")
           "* %?\n %U\n %i\n" :tag "idea")

          ;; Reflections / Essays
          ("d" "Diary" entry (file+headline "~/kp7/org/life.org" "Diary")
           "* On %?\n [%<%Y-%m-%d %a>]\n\n" :tag "note")

          ;; Resolutions — goals or habits
          ("r" "Resolution" entry (file+headline "~/kp7/org/life.org" "Resolutions")
           "* TODO %?\n %U\n %i\n" :tag "resolution")))
  (setq org-agenda-files '("~/kp7/org/life.org"))

  (setq org-agenda-custom-commands
  '(("w" "Weekend Review"
     ((tags "inbox"
            ((org-agenda-overriding-header "📥 Inbox — triage first")))

      (tags-todo "+TIMESTAMP<\"<-21d>\""
                 ((org-agenda-overriding-header "⚠️  Stale — decide or delete")))

      (tags-todo "read"
                 ((org-agenda-overriding-header "📚 Reading Queue")))

      (tags "idea"
            ((org-agenda-overriding-header "💡 Ideas")))

      (tags "resolution"
            ((org-agenda-overriding-header "🎯 Resolutions")))

      (todo "TODO"
            ((org-agenda-overriding-header "📋 All Tasks")
             (org-agenda-files '("~/kp7/org/life.org"))))))))
  
  ;; Increase scale from 1.0 to 1.4
  (setq org-format-latex-options
	'(:foreground default :background default :scale 1.4 :html-foreground "Black"
		      :html-background "Transparent" :html-scale 1.0
		      :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
  ;; Use dvisvgm instead of dvipng
  (setq org-latex-create-formula-image-program 'dvisvgm)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(( C . t))))

;; Stolen fsrom the package ov
;; Align latex previews in org mode (Currently not working
;;  (defun ov-at (&optional point)
;;    "Get an overlay at POINT.
;;  POINT defaults to the current `point'."
;;    (or point (setq point (point)))
;;    (car (overlays-at point)))
;; ;; https://www.reddit.com/r/emacs/comments/169keg7/comment/jzierha/?utm_source=share&utm_medium=web2x&context=3
;;  (defun org-justify-fragment-overlay (beg end image &optional imagetype)
;;    "Only equations at the beginning and also end of a line are justified."
;;    (if
;;     (and (= beg (line-beginning-position)) (= end (line-end-position)))
;;     (let* ((ov (ov-at))
;;    (disp (overlay-get ov 'display)))
;;       (overlay-put ov 'line-prefix `(space :align-to (- center (0.5 . ,disp)))))))
;;  (advice-add 'org--make-preview-overlay :after 'org-justify-fragment-overlay)

;; (use-package vterm
;;   :ensure t)

(use-package ghostel
  :ensure t
  :config
  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel" "s") t)
  :bind
  (:map project-prefix-map
        ("s" . ghostel-project)
        ("S" . ghostel-project-list-buffers)))

(use-package ace-window
  :ensure t
  :bind ("C-x o" . 'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-scope 'visible))

;;Start a competitive programming session
(defun start-comp-prog-session ()
  "Start a competitive programming session."
  (interactive)
  (if (> (window-width) 130)
      (progn
	(delete-other-windows)
	(find-file "~/kp7/Competitive_Programming/cpp_env/sol.cpp")
	(split-window-right)
	(other-window 1)
	(find-file "~/kp7/Competitive_Programming/cpp_env/input.txt")
	(split-window-below)
	(enlarge-window -9)
	(other-window 1)
	(split-window-below)
	(find-file "~/kp7/Competitive_Programming/cpp_env/output.txt")
	(other-window 1)
	(vterm))
    (progn
      (delete-other-windows)
      (find-file "~/kp7/Competitive_Programming/cpp_env/sol.cpp")
      (split-window-below)
      (enlarge-window 20)
      (other-window 1)
      (find-file "~/kp7/Competitive_Programming/cpp_env/input.txt")
      (vterm)
      (other-window 1))))
(keymap-global-set "C-x c p" 'start-comp-prog-session)

(use-package smartparens
  :ensure smartparens  ;; install the package
  :hook (prog-mode LaTeX-mode org-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

(use-package ediff
  :config
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package pdf-tools
  :ensure t
  :commands pdf-view-mode
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook ((pdf-view-mode . (lambda () (display-line-numbers-mode -1) (auto-revert-mode) (pdf-view-roll-minor-mode))))
  :config
  (pdf-loader-install))

(use-package yaml-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ;; ("\\.jsx\\'" . web-mode)
   ;; ("\\.html\\'" . web-mode)
   ;; ("\\.js\\'" . web-mode)
   ;; ("\\.css\\'" . web-mode)
   ;; ("\\.tsx\\'" . web-mode)
   )
  :config
  (setq web-mode-markup-indent-offset 2
	web-mode-code-indent-offset 2
	web-mode-css-indent-offset 2))

(use-package magit
  :ensure t
  :bind (("C-x g" . nil)
         ("C-x g g" . magit-status)))

(use-package dired
  :ensure nil
  :config
  (setq dired-kill-when-opening-new-dired-buffer 1))

(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.prisma\\'" . prisma-ts-mode)
         ;; More modes defined here...
         )
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (bash "https://github.com/tree-sitter/tree-sitter-bash")
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
               (markdown "https://github.com/ikatyang/tree-sitter-markdown")
               (make "https://github.com/alemuller/tree-sitter-make")
               (elisp "https://github.com/Wilfred/tree-sitter-elisp")
               (cmake "https://github.com/uyha/tree-sitter-cmake")
               (c "https://github.com/tree-sitter/tree-sitter-c")
               (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
               (toml "https://github.com/tree-sitter/tree-sitter-toml")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
               (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (c-or-c++-mode . c-or-c++-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (os/setup-install-grammars))


(use-package elfeed
  :ensure t
  :hook
  (elfeed-show-mode . visual-line-mode)
  :bind (("C-x w w" . elfeed))
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
  (setq elfeed-feeds
	'(;; Blogs
	  ("http://nullprogram.com/feed/" Programming CS)
	  "https://lexi-lambda.github.io/feeds/all.rss.xml"
	  "https://justanotherelectronicsblog.com/?feed=rss2"
	  "https://scripter.co/atom.xml"
	  "https://eugene-andrienko.com/en/feed.xml"
	  "https://karthinks.com/index.xml"
	  "https://pointersgonewild.com/feed.xml"
	  )))
(use-package elfeed-org
  :ensure t
  :init
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))
  :config
  (elfeed-org))

(use-package ibuffer
  :ensure nil
  :bind (("C-x C-b" . ibuffer-other-window)))

;; Live updating latex preview inside emacs
(use-package latex-preview-pane
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command "multimarkdown"
        markdown-fontify-code-blocks-natively t)
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

(use-package diff-hl
  :ensure t
  :init
  (setq diff-hl-draw-borders t)
  :hook ((dired-mode . diff-hl-dired-mode))
  :bind
  (:map diff-hl-command-map
	("n" . diff-hl-next-hunk)
	("p" . diff-hl-previous-hunk)
	("[" . nil)
	("]" . nil)
	("DEL"   . diff-hl-revert-hunk)
	("<delete>" . diff-hl-revert-hunk)
	("k" . diff-hl-revert-hunk)
	("SPC" . diff-hl-mark-hunk)
	("," . diff-hl-set-reference-rev)
	("." . diff-hl-reset-reference-rev)
	:map vc-prefix-map
	("n" . diff-hl-next-hunk)
	("p" . diff-hl-previous-hunk)
	("s" . diff-hl-stage-dwim)
	("DEL"   . diff-hl-revert-hunk)
	("<delete>" . diff-hl-revert-hunk)
	("k" . diff-hl-revert-hunk)
	("SPC" . diff-hl-mark-hunk)
	("," . diff-hl-set-reference-rev)
	("." . diff-hl-reset-reference-rev))
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)
  (global-diff-hl-mode)
  (put 'diff-hl-inline-popup-hide
       'repeat-map 'diff-hl-command-map)
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(use-package repeat
  :ensure nil
  :defer t
  :hook (after-init . repeat-mode))


;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-.")
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((tsx-ts-mode
           typescript-ts-mode
           js-ts-mode
	   java-ts-mode
           python-mode) . lsp-deferred))  
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
(use-package lsp-java
  :ensure t)

(use-package speed-type
  :ensure t
  :init
  (setq speed-type-wordlist-transform 'downcase)
  :bind (("C-x g s" . (lambda ()
                        (interactive)
                        (speed-type-top-x 8000)))))

(use-package verb
  :ensure t)

;; Even describe C functions
(setq source-directory (expand-file-name "~/kp7/emacs/"))
(setq find-function-C-source-directory (expand-file-name "~/kp7/emacs/src/"))

(set-face-attribute 'default nil :family "Courier New" :height 120 :weight 'normal :width 'normal)

(use-package erc
  :ensure nil
  :config
  (setopt erc-user-full-name "Priyanshu Kalal"))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-directory (file-truename "~/kp7/org/org-roam"))
  :config
  (org-roam-db-autosync-mode)
  :bind (("C-x n f" . org-roam-node-find)
	 ("C-x n i" . org-roam-node-insert)
	 ("C-x n y" . org-id-get-create)
	 ("C-x n c" . org-roam-capture)
         ("C-x n l" . org-roam-buffer-toggle)))

(use-package devdocs
  :ensure t
  :bind ("C-h D" . devdocs-lookup))

(use-package compile
  :ensure nil
  :bind (("C-x c c" . compile)
         ("C-x c r" . recompile)))

(use-package easy-hugo
  :ensure t
  :init
  (setq easy-hugo-basedir "~/kp7/mySite/"))

(use-package cuda-mode
  :ensure t)

(use-package gptel
  :ensure t
  :bind (("C-x a RET" . gptel-send))
  :config
  (setq auth-sources '("~/.authinfo.gpg"))
  (setq gptel-backend (gptel-make-gemini "Gemini"
                        :key (lambda ()
                               (auth-source-pick-first-password
                                :host "api.generativeai.google.com"
                                :user "apikey"))
                        :stream t)))

(with-eval-after-load 'project
  (defun project-find-regexp-with-unique-buffer (orig-fun &rest args)
    "An advice function that gives project-find-regexp a unique buffer name"
    (require 'xref)
    (let ((xref-buffer-name (format "%s %s" xref-buffer-name (car args))))
      (apply orig-fun args)))

  (advice-add 'project-find-regexp :around
              #'project-find-regexp-with-unique-buffer))


(provide 'init)

;;; init.el ends here
