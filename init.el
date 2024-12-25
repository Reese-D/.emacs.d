(defun efs/display-startup-time ()
  (message " Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;;M-x use-package-report -> shows how fast packages loaded up
(setq use-package-compute-statistics t)


(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;;; Set up the package manager

(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;;; Basic behaviour

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

;;; Tweak the looks of Emacs

;; Those three belong in the early-init.el, but I am putting them here
;; for convenience.  If the early-init.el exists in the same directory
;; as the init.el, then Emacs will read+evaluate it before moving to
;; the init.el.
(menu-bar-mode 1)
(scroll-bar-mode 1)
(tool-bar-mode -1)

(let ((mono-spaced-font "Monospace")
      (proportionately-spaced-font "Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 100)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))


;;use-package for modus-themes was roughly 0.7 seconds load up time on my mac mini m4
;;so instead try to just load the theme, if that works (we've run the use-package at least once before) just enable the theme to avoid that overhead
(if (load-theme 'modus-vivendi-tinted :no-confirm t)
    (enable-theme 'modus-vivendi-tinted)
  (use-package modus-themes
    :ensure t
    :config-theme 'modus-vivendi-tinted :no-confirm-loading))

(use-package hydra
  :ensure t)

(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

;; Remember to do M-x and run `nerd-icons-install-fonts' to get the
;; font files.  Then restart Emacs to see the effect.
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;;; Configure the minibuffer and completions

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrride nil))

(use-package savehist
  :ensure nil ; it is built-in
  :hook (after-init . savehist-mode))

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq tab-always-indent 'complete)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)

  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

;;; The file manager (Dired)

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

;---------------------------------------------------------------------

;;interactive lisp programming
(use-package slime
  :ensure t
  :config (setq inferior-lisp-program "sbcl")
  :commands slime)

;;hell yea
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

;;commands to show extra whitespace and line length
(use-package fill-column-indicator
  :ensure t
  :bind ( ("C-c 2" . display-fill-column-indicator-mode)
	  ("C-c 1" . whitespace-mode))
  :config
  (progn
    (setq fci-rule-width 1)
    (setq fci-rule-color "darkblue")
    (setq fci-rule-column 120))) 

;;colored parenthesis for balancing lisp
(use-package rainbow-delimiters
  :ensure t
  :config (progn
            (defface my-outermost-paren-face
              '((t (:weight bold)))
              "Face used for outermost parens.")
            (use-package cl-lib
            )
            (use-package color
            )
            (show-paren-mode)
            (cl-loop
             for index from 1 to rainbow-delimiters-max-face-count
             do
             (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
               (cl-callf color-saturate-name (face-foreground face) 30))))
  :hook (prog-mode . rainbow-delimiters-mode))

;;parenthesis balancing (may want to disable?)
(use-package paredit
  :ensure t
  :hook (lisp-mode . paredit-mode))

;;snippets
(use-package yasnippet
  :ensure t
  :config
  (progn
    (yas-global-mode 1)
    (use-package yasnippet-snippets
      )))

;;navigate quickly to text on screen by searching head character
(use-package ace-jump-mode
  :ensure t
  :diminish ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

;;git integration
(use-package magit
  :ensure t
  :defer t)

;;flash where the cursor is when the screen moves
(use-package beacon
    :ensure t
    :config (beacon-mode 1)
    )

;;qlot common lisp package manager and slime for repl
(setq slime-lisp-implementations
      '((sbcl ("sbcl") :coding-system utf-8-unix)
        (qlot ("qlot" "exec" "sbcl") :coding-system utf-8-unix)))

(use-package org-roam
  :ensure t
  :init (setq org-roam-v2-ack t)
  :custom (org-roam-directory "~/org-roam")
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n a" . org-roam-alias-add)))

(org-roam-db-autosync-mode)

(setq org-roam-node-display-template
      (concat "${title:*} "
	      (propertize "${tags:20}" 'face 'org-tag)))


;;roswell helper
(load (expand-file-name "~/.roswell/helper.el"))

(server-start)
;;things to add once comfortable
					;1) projectile/dired
					;2) tab bar mode
					;3) look into bookmarks, registers, and mark rings
					;4) company mode (maybe  helm?)
					;5) avy to replace ace-jump-mode
					;6) dumb-jump
					;7) swiper
					;8) look into using yasnippet
					;9) multiple-cursors
					;10) which-key (should come default in emacs 30)


;;some useful commands
					;display-line-numbers-mode and line-number-mode
