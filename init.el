;;useful tips
;;C-h k --type this then any other command, and it will tell you the name of the command assigned to that hotkey as well as a description
;;C-h w --reverse of C-h k, type in the name of any command and it will tell you the keybinding for it

;;set a register
;;C-x r SPC 
;;jump back to that register
;;C-x r j

(require 'package)

;;add scripts directory so that .el files are automatically evaluated
(mapc 'load (file-expand-wildcards "~/.emacs.d/scripts/*.el"))

;;add custom file to load path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/custom_elisp/") t)

                                        ;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
                                        ;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
                                        ;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

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

 ;; ;;use-package can't find these, so try other repos
 ;; 'undo-tree
 ;; 'smart-tabs-mode

 ;; ;;Themes
 ;; 'ample-theme
 ;; 'monokai-theme
 ;; 'gruvbox-theme
 ;; 'zenburn-theme
 ;; 'dracula-theme
 ;; 'nord-theme
 )

;;---------------------------------use package------------------------------------
(setq use-package-always-ensure t)
(use-package undo-tree)
(use-package ample-theme)
(use-package monokai-theme)
(use-package zenburn-theme)
;;(use-package dracula-theme)
(use-package nord-theme)
;;(use-package yaml-mode)
;;(use-package terraform-mode)  
;; (use-package clojure-mode
;;   :config (progn
;;             (use-package cider
;;             )
;;             (use-package clojure-mode-extra-font-locking
;;             )))
;; (use-package go-mode)
;; (use-package fennel-mode)
;; (use-package lua-mode)
;;(use-package alchemist)
(use-package diminish)
;;(use-package haskell-mode)
(use-package transpose-frame)
;;(use-package inf-ruby)   ;run ruby in emacs with M-x inf-ruby
(use-package magit-popup)
(use-package magit)
(use-package delight)
;;(use-package typescript-mode)
;; (use-package tide
;;   :after (typescript-mode company flycheck)
;;   :hook ((typescript-mode . tide-setup)
;;          (typescript-mode . tide-hl-identifier-mode)
;;          (before-save . tide-format-before-save)))


(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (
         (csharp-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config (progn
            (use-package dap-mode)
            (use-package which-key
              :config
              (which-key-mode))))

(use-package projectile
  :config
  (progn (projectile-global-mode 1)
         (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-read-string-input             'from-child-frame
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile)
)

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

;; (use-package smart-tabs-mode
;;   :hook
;;   (common-lisp-mode . smart-tabs)
;;   :config
;;   (progn
;;     (setq-default indent-tabs-mode nil)
;;     (smart-tabs-insinuate 'python)
;;     (setq backward-delete-char-untabify-method nil)
;;     (smart-tabs-advice python-indent-line-1 python-indent)))

(use-package org
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)))
    (require 'ox-md)
    (use-package htmlize 
    ))
  :bind
  ("C-c o l" . org-store-link)
  ("C-c o a" . org-agenda))

(use-package elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

;; M-. find definitions
;; C-x 4 find definitions other window
;; M-, pop back ref stack
;; M-. show doc
(use-package anaconda-mode
  :init
  (defun anaconda-autocomplete-hook ()
    (local-set-key (kbd "C-<tab>") 'anaconda-mode-complete))
  :hook (progn
          (python-mode . anaconda-mode)
          (python-mode . anaconda-eldoc-mode)
          (python-mode . anaconda-autocomplete-hook)))

;; (use-package rainbow-delimiters
;;   :config (progn
;;             (defface my-outermost-paren-face
;;               '((t (:weight bold)))
;;               "Face used for outermost parens.")
;;             (use-package cl-lib
;;             )
;;             (use-package color
;;             )
;;             (show-paren-mode)
;;             (cl-loop
;;              for index from 1 to rainbow-delimiters-max-face-count
;;              do
;;              (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
;;                (cl-callf color-saturate-name (face-foreground face) 30))))
;;   :hook (prog-mode . rainbow-delimiters-mode))

;; (use-package web-mode
;;   :init
;;   (use-package glsl-mode
;;   )
;;   :after (flycheck)
;;   :config (progn
;;             (let ((glsl-stuff (mapcar (lambda (x) (cons x 'glsl-mode)) '("\\.glsl\\'" "\\.vert\\'" "\\.frag\\'" "\\.geom\\'")))
;;                   (web-stuff (mapcar (lambda (x) (cons x 'web-mode)) '("\\.tsx\\'" "\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'"))))
;;               (mapc (lambda (x) (add-to-list 'auto-mode-alist x)) glsl-stuff)
;;               (mapc (lambda (x) (add-to-list 'auto-mode-alist x)) web-stuff))
;;               (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;                 (setup-tide-mode))))

(use-package flycheck
  :config (flycheck-add-mode 'typescript-tslint 'web-mode))
  
(use-package multiple-cursors
  :bind
  ("C-c s" . mc/edit-lines)
  ("C-c n" . mc/mark-next-like-this)
  ("C-c p" . mc/mark-previous.like-this)
  ("C-c a" . mc/mark-more-like-this-extended))

(use-package yasnippet
  :config
  (progn
    (yas-global-mode 1)
    (use-package yasnippet-snippets
    )))

(use-package company
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
  :bind
  ("M-i" . nil);; Remove the old keybinding tab-to-tab-stop
  ("M-i i" . dumb-jump-go)
  ("M-i b" . dumb-jump-back)
  ("M-i q" . dumb-jump-quick-look)
  ("M-i o" . dumb-jump-go-other-window)
  ("M-i e" . dumb-jump-go-prefer-external)
  ("M-i w" . dumbp-jump-g-prefer-external-other-window))


(use-package ace-jump-mode
  :diminish ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

(use-package ivy
  :config
  (progn (ivy-mode 1)
         (setq ivy-use-virtual-buffers t)
         (setq enable-recursive-minibuffers t)))
                 

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

(use-package slime
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

;;requires cmake
;;(use-package vterm)

;;-------------------------------custom functions---------------------------------


;;---------------------------------keybindings------------------------------------
(global-set-key (kbd "C-<delete>") 'switch-to-previous-buffer)

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
(load-theme 'gruvbox t)
(load-theme 'gruvbox-dark-hard t)
;;;(load-theme 'dracula t)
(load-theme 'nord t)


(enable-theme 'gruvbox-dark-hard) ;our chosen theme, pick whatever you like (my top picks are ample-flat, gruvbox-dark-hard, nord, and wombat)
