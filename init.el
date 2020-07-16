;;useful tips
;;C-h k --type this then any other command, and it will tell you the name of the command assigned to that hotkey as well as a description
;;C-h w --reverse of C-h k, type in the name of any command and it will tell you the keybinding for it
(require 'package)

;;add scripts directory to load path, so that .el files are automatically evaluated
(add-to-list 'load-path "~/.emacs.d/scripts")
(add-to-list 'load-path "~/.emacs.d/custom_elisp/")

(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(defun add-and-require-multiple (&rest list)
  (dolist (required list)
    (unless (package-installed-p required)
      (condition-case nil
          (progn
            (package-install required)
            (and required (require required)))
        (error (warn (format "package %s failed to load" required)))))))

(add-and-require-multiple
 'use-package ;can't include itself

 ;;use-package can't find these, so try other repos
 'undo-tree
 'smart-tabs-mode

 ;;Themes
 'ample-theme
 'monokai-theme)

;;---------------------------------use package------------------------------------
(use-package alchemist
  :ensure t)
(use-package diminish
  :ensure t)
(use-package haskell-mode
  :ensure t)
(use-package transpose-frame
  :ensure t)
(use-package inf-ruby   ;run ruby in emacs with M-x inf-ruby
  :ensure t)
(use-package magit-popup
  :ensure t)
(use-package magit
  :ensure t)
(use-package delight
  :ensure t)

(use-package lsp-mode
  :ensure t)

(use-package fill-column-indicator
  :init
  (define-globalized-minor-mode global-fci-mode fci-mode
    (lambda () (fci-mode 1)))
  :hook
  (python-mode . fci-mode)
  :bind ("C-c 2" . fci-mode)
  :config
  (progn
    (setq fci-rule-width 1)
    (setq fci-rule-color "darkblue")
    (setq fci-rule-column 120)))

(use-package redspace-mode
  :bind ("C-c 1" . redspace-mode))

(use-package smart-tabs-mode
  :ensure t
  :hook
  (common-lisp-mode . smart-tabs)
  :config
  (progn
    (setq-default indent-tabs-mode nil)
    (smart-tabs-insinuate 'python)
    (setq backward-delete-char-untabify-method nil)
    (smart-tabs-advice python-indent-line-1 python-indent)))

(use-package org
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)))
    (require 'ox-md)
    (use-package htmlize 
    :ensure t))
  :bind
  ("C-c o l" . org-store-link)
  ("C-c o a" . org-agenda))

(use-package elpy
  :ensure t
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

;; M-. find definitions
;; C-x 4 find definitions other window
;; M-, pop back ref stack
;; M-. show doc
(use-package anaconda-mode
  :ensure t
  :init
  (defun anaconda-autocomplete-hook ()
    (local-set-key (kbd "C-<tab>") 'anaconda-mode-complete))
  :hook (progn
          (python-mode . anaconda-mode)
          (python-mode . anaconda-eldoc-mode)
          (python-mode . anaconda-autocomplete-hook)))

(use-package rainbow-delimiters
  :ensure t
  :config (progn
            (defface my-outermost-paren-face
              '((t (:weight bold)))
              "Face used for outermost parens.")
            (use-package cl-lib
              :ensure t)
            (use-package color
              :ensure t)
            (show-paren-mode)
            (cl-loop
             for index from 1 to rainbow-delimiters-max-face-count
             do
             (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
               (cl-callf color-saturate-name (face-foreground face) 30))))
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package web-mode
  :ensure t
  :init
  (use-package glsl-mode
    :ensure t)
  (use-package web-mode
    :ensure t)
  :config (let ((glsl-stuff (mapcar (lambda (x) (cons x 'glsl-mode)) '("\\.glsl\\'" "\\.vert\\'" "\\.frag\\'" "\\.geom\\'")))
                (web-stuff (mapcar (lambda (x) (cons x 'web-mode)) '("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'"))))
            (mapc (lambda (x) (add-to-list 'auto-mode-alist x)) glsl-stuff)))

(use-package multiple-cursors
  :ensure t
  :bind
  ("C-c s" . mc/edit-lines)
  ("C-c n" . mc/mark-next-like-this)
  ("C-c p" . mc/mark-previous.like-this)
  ("C-c a" . mc/mark-more-like-this-extended))

(use-package yasnippet
  :ensure t
  :config
  (progn
    (yas-global-mode 1)
    (use-package yasnippet-snippets
      :ensure t)))

(use-package company
  :ensure t
  :diminish company-mode
  :hook (after-init . global-company-mode)
  :bind
  (:map company-active-map
        ("RET" . nil)
        ([return] . nil)
        ("TAB" . company-complete-selection)
        ([tab] . company-complete-selection)
        ("<right>" . company-complete-common))
  :custom
  (company-dabbrev-downcase nil)
  (company-idle-delay .2)
  (company-minimum-prefix-length 1)
  (company-require-match nil)
  (company-tooltip-align-annotations t))

(use-package dumb-jump
  :ensure t
  :bind
  ("M-i" . nil);; Remove the old keybinding tab-to-tab-stop
  ("M-i i" . dumb-jump-go)
  ("M-i b" . dumb-jump-back)
  ("M-i q" . dumb-jump-quick-look)
  ("M-i o" . dumb-jump-go-other-window)
  ("M-i e" . dumb-jump-go-prefer-external)
  ("M-i w" . dumbp-jump-g-prefer-external-other-window))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode 1))

(use-package ace-jump-mode
  :ensure t
  :diminish ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

(use-package neotree
  :ensure t
  :bind ("C-c 8" . neotree-toggle))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

;;-------------------------------custom functions---------------------------------

;;global minor modes
(defun my-electric-mode ()
  (electric-pair-mode 1))

;;; Define a default fullscreen and non full-screen mode, then add a function to toggle between the two
(defun my-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen 'fullboth)       ;this makes the frame go fullscreen
  (tool-bar-mode -1)                                    ;these 3 lines turn off GUI junk
  (menu-bar-mode -1))

(defun my-non-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'width 82)
  (set-frame-parameter nil 'fullscreen 'fullheight)
  (menu-bar-mode t))                                    ;I don't turn tool-bar and scroll-bar back on b/c I never want them


(defun toggle-fullscreen ()
  (interactive)
  (if (eq (frame-parameter nil 'fullscreen) 'fullboth)  ;tests if already fullscreened
      (my-non-fullscreen)
    (my-fullscreen)))

;;copies the buffers current file path
(defun filename ()
  "Copy the full path of the current buffer."
  (interactive)
  (kill-new (buffer-file-name (window-buffer (minibuffer-selected-window)))))

;;---------------------------------keybindings------------------------------------

(setq next-line-add-newlines t) ;C-n now adds newline if at end of buffer
(global-set-key (kbd "C-c f") 'toggle-fullscreen)
(global-set-key (kbd "<C-tab>") 'dabbrev-expand)
(global-set-key (kbd "C-c v") 'filename)

;; Different keybinds for macros
(global-set-key (kbd "C-c r s") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-c r e") 'kmacro-end-or-call-macro)

;;---------------------------------Initialization---------------------------------

(toggle-fullscreen)
(setq inhibit-startup-message t)
(put 'upcase-region 'disabled nil)

;;enable electric-pair-mode for lisps
(add-hook 'common-lisp-mode-hook #'my-electric-mode)
(add-hook 'emacs-lisp-mode-hook #'my-electric-mode)

;;this will indent switch statements in c
(c-set-offset 'case-label '+)

;;make c indent 4 by default instead of 2
(setq-default c-basic-offset 4)

;;(setq mac-option-modifier 'meta)  ;enable this if using a mac
(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))

(unless (file-exists-p custom-file)
  (write-region "" "" custom-file))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;---------------------------------Custom  Themes---------------------------------
(load-theme 'wombat t)          ;neutral dark color scheme
(load-theme 'tsdh-dark t)       ;another default emacs dark color scheme
(load-theme 'monokai t)
(load-theme 'ample t t)         ;these are all pretty nice, each gets a little lighter
(load-theme 'ample-flat t t)
(load-theme 'ample-light t t)   ;tan background

(enable-theme 'monokai) ;our chosen theme, pick whatever you like
