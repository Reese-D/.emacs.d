;;if this file doesn't load, check to make sure you don't have a .emacs file somewhere or a different init file running  "C-h v user-init-file"

;;to see compile options check the "system-configuration-options" value with "C-h v"

;;some common compile options I use
;;--without-sound --without-imagemagick --with-rsvg --with-threads --with-x-toolkit=no --with-native-compilation --with-tree-sitter --with-ns 'CFLAGS= -pipe -O3 -march=native -fomit-frame-pointer -fno-semantic-interposition -L/opt/homebrew/lib/gcc/14 -I/opt/homebrew/include -Wl,-rpath,/opt/homebrew/lib/gcc/14' LDFLAGS="-Wl,-O1" 

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Uncomment for systems which cannot create symlinks (I.E Windows)
;; (elpaca-no-symlink-mode)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

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
  :hook (elpaca-after-init . delete-selection-mode))

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

(menu-bar-mode 1)
(scroll-bar-mode 1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)

(let ((mono-spaced-font "Monospace")
      (proportionately-spaced-font "Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 110)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

;;Currently emacs 30.1 and 30.2 bake in an old version of transient, too old for magit/elpaca
;;this should force elpaca to download a newer version
(use-package transient
  :ensure t
  )

(use-package modus-themes
  :ensure t
  :config (load-theme 'modus-vivendi-tinted :no-confirm t)
  (enable-theme 'modus-vivendi-tinted))


(use-package hydra
  :ensure t
  :config (defhydra hydra-zoom (global-map "<f2>")
	    "zoom"
	    ("g" text-scale-increase "in")
	    ("l" text-scale-decrease "out")))

;;Allows you to rotate buffers to move windows around
;;Use with rotate-window command
(use-package rotate
  :ensure t)

;; Remember to do M-x and run `nerd-icons-install-fonts' to get the
;; font files.  Then restart Emacs to see the effect.
(use-package nerd-icons
  :ensure t
  )

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
  :hook (elpaca-after-init . vertico-mode))

(use-package marginalia
  :ensure t
  :hook (elpaca-after-init . marginalia-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrride nil))

(use-package savehist
  :ensure nil ; it is built-in
  :hook (elpaca-after-init . savehist-mode))

(use-package corfu
  :ensure t
  :hook (elpaca-after-init . global-corfu-mode)
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
(use-package sly
  :ensure t
  :config
  (setq inferior-lisp-program "/opt/homebrew/bin/sbcl"))


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
    (use-package yasnippet-snippets)))

;;navigate quickly to text on screen by searching head character
(use-package ace-jump-mode
  :ensure t
  :diminish ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

;;git integration
(use-package magit
  :ensure t
  )

;;flash where the cursor is when the screen moves
(use-package beacon
  :ensure t
  :config (beacon-mode 1)
  )

(use-package org-roam
  :ensure t
  :init (setq org-roam-v2-ack t)
  :custom (org-roam-directory "~/org-roam")
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n a" . org-roam-alias-add))
  :config (org-roam-db-autosync-mode)
  (setq org-roam-node-display-template
	(concat "${title:*} "
		(propertize "${tags:20}" 'face 'org-tag))))


;;roswell helper
					;(load (expand-file-name "~/.roswell/helper.el"))

;;(server-start)
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
					;display-line-numbers-mode and line-numb

(setq treesit-language-source-alist
      '(;;(bash "https://github.com/tree-sitter/tree-sitter-bash")
	;;(cmake "https://github.com/uyha/tree-sitter-cmake")
	;;(c "https://github.com/tree-sitter/tree-sitter-c")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	;;(elisp "https://github.com/Wilfred/tree-sitter-elisp") ;;no mode for this at the moment
	;;(go "https://github.com/tree-sitter/tree-sitter-go")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	;;(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	;;(json "https://github.com/tree-sitter/tree-sitter-json")
	;;(make "https://github.com/alemuller/tree-sitter-make")
	;;(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	;;(python "https://github.com/tree-sitter/tree-sitter-python")
	;;(toml "https://github.com/tree-sitter/tree-sitter-toml")
	;;(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	;;(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(haskell "https://github.com/tree-sitter/tree-sitter-haskell")))

(mapc (lambda (x) (unless (treesit-language-available-p (car x))
		    (treesit-install-language-grammar (car x))))
      treesit-language-source-alist)


(use-package haskell-ts-mode
  :ensure t
  :mode "\\.hs\\'")

(use-package css-ts-mode
  :ensure nil ;;should be built in, will blow up with t
  :mode "\\.css\\'")

(use-package html-ts-mode
  :ensure nil ;;should be built in, will blow up with t
  :mode "\\.html\\'")

(keymap-global-set "§" "`")
(keymap-global-set "±" "~")


;;Make sure to install Leiningen if using lein projects, then open with cider-jack-in
(use-package clojure-mode
  :ensure t
  )
;; (use-package cider
;;   :ensure t)

(use-package rust-mode
  :ensure t
  )
(use-package rustic
  :ensure t
  )

(use-package lsp-ui
  :ensure t
  :init
  (global-set-key (kbd "C-c k") 'lsp-ui-doc-glance)
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'at-point) ;;default is top or bottom i think?
  (setq lsp-ui-doc-show-with-cursor nil)) ;;show doc when hovering over symbol

(use-package company
  :ensure t
  :hook (lsp-mode . company-mode)
  :config
  (global-company-mode))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (which-key-mode)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'c++-mode-hook 'lsp))

(use-package markdown-mermaid
  :ensure (markdown-mermaid :type git :host github :repo "pasunboneleve/markdown-mermaid")
  :bind (:map markdown-mode-map
              ("C-c m" . markdown-mermaid-preview)))

(use-package projectile
  :ensure t
  )
(use-package dap-mode
  :ensure t
  :config  (add-hook 'rustic-mode-hook (lambda ()
					 (dap-register-debug-template "Rust LLDB Debug Configuration"
								      (list :type "cppdbg"
									    :request "launch"
									    : name "Rust::Run"
									    :MIMode "lldb"
									    :gdbpath "rust-lldb"
									    :program (concat (projectile-project-root) "target/debug/" (projectile-project-name)) ;; Requires that the rust project is a project in projectile
									    :environment []
									    :targetarchitecture "arm" ;;Only if you actually use ARM processor, such as on MacOS
									    :cwd (projectile-project-root))))))

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools


;;Requires a local copy of carp-emacs, it's not on melpa or anything like that.
(if (file-directory-p "~/git/carp-emacs")
    (progn
      (add-to-list 'load-path "~/git/carp-emacs")
      (require 'carp-mode)
      (require 'inf-carp-mode)
      (add-to-list 'auto-mode-alist '("\\.carp\\'" . carp-mode))
      ))

;; Use carp-mode for .carp files


;;Guix specific, load any emacs guix packages that might exist
(if (file-directory-p "~/.guix-profile/share/emacs/site-lisp")
  (progn
    (add-to-list 'load-path "~/.guix-profile/share/emacs/site-lisp")
    (guix-emacs-autoload-packages)))

;;local AI using llama-cpp
(use-package gptel
  :defer t
  :config (setq
	   gptel-model   'local
	   gptel-backend (gptel-make-openai "llama-cpp"
			   :stream t
			   :protocol "http"
			   :host "127.0.0.1:8000"
			   :models '(local))))

(setq-default c-basic-offset 4)

; Source - https://stackoverflow.com/a/22176971
; Posted by user2053036, modified by community. See post 'Timeline' for change history
; Retrieved 2026-05-25, License - CC BY-SA 3.0
(setq auto-save-file-name-transforms
          `((".*" ,(concat user-emacs-directory "auto-save/") t))) 

(setq create-lockfiles nil)

(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
