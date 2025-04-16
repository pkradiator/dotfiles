;;; Packages I have Installed
;; auctex
;; cdlatex
;; eat
;; exec-path-from-shell
;; speed-type (installed manually)
;; yaml-mode
;; ace-window
;; pdf-tools
;; Increase garbage-collection threshold


(setq gc-cons-threshold (* 50 1000 1000))


;; Tell us how fast we're going, for benchmarking
(defun pkd/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'pkd/display-startup-time)


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
(load custom-file)

;; Add melpa
(add-to-list 'package-archives 
    '("MELPA" .
      "http://melpa.org/packages/"))

;; Full screen on startup
(toggle-frame-fullscreen)

;; On Mac keyboard Command -> Meta
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'control)

;; I prefer spaces over tabs
(setq indent-tabs-mode nil)
;;Add content of clipboard(things you copy outside emacs) to kill ring (emacs clipboard)
(setq save-interprogram-paste-before-kill t)

;; Menu Bar is actually useful on macos
;; (menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

;; Show column number on the modeline.
(column-number-mode)

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

;; Auctex
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
  (exec-path-from-shell-initialize))

;;; Org Mode:-
(defun pkd/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1)
  (modify-syntax-entry ?< ".")
  (modify-syntax-entry ?> "."))

(use-package org
  :hook (org-mode . pkd/org-mode-setup)
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :config
  (setq org-directory "~/kp7/org")
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-insert-heading-respect-content t)
  ;; Org Capture templates
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "~/kp7/org/gtd.org" "Tasks")
	   "* Todo %?\n  %i\n  %a")
	  ("j" "Journal" entry (file+datetree "~/kp7/org/journal.org")
	   "* %?\nEntered on %U\n  %i\n  %a")))
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

;;; For package eat
(use-package eat
  :hook (eshell-load . eat-eshell-mode))

;;; For package ace-window
(use-package ace-window
  :ensure t
  :bind ("C-x o" . 'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-scope 'frame))

;;Start a competitive programming session
(defun start-comp-prog-session ()
  "Starts a competitive programming session"
  (interactive)
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
  (eat))
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
      :hook ((pdf-view-mode . (lambda () (display-line-numbers-mode -1) (auto-revert-mode))))
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
   ("\\.jsx\\'" . web-mode)
   ("\\.html\\'" . web-mode)
   ("\\.js\\'" . web-mode)
   ("\\.css\\'" . web-mode)
   ("\\.tsx\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
	web-mode-code-indent-offset 2
	web-mode-css-indent-offset 2))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))
