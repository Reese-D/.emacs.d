
# Table of Contents

1.  [Editing this file](#org40840f6)
2.  [Require](#org0b10954)
    1.  [Add and require multiple](#org56149f6)
3.  [Packages](#org5b3974f)
    1.  [Misc](#orgfe11ce3)
    2.  [Code formatting](#org57d6c47)
    3.  [Code editing](#org9e821cc)
    4.  [Auto complete](#orgf828764)
    5.  [Navigation](#org4c67133)
    6.  [Interactive environments](#org62b3c81)
4.  [Custom functions](#org0df8b6a)
5.  [Keybindings](#org05dc446)
6.  [Initialize settings](#org87568c9)
7.  [Themes](#org9f2105a)



<a id="org40840f6"></a>

# Editing this file

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">command</th>
<th scope="col" class="org-left">definitions</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-c C-v C-t</td>
<td class="org-left">Tangle to init.el</td>
</tr>


<tr>
<td class="org-left">C-c C-e</td>
<td class="org-left">Export</td>
</tr>
</tbody>
</table>


<a id="org0b10954"></a>

# Require

this requires all the different archives we might use sets them up

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


<a id="org56149f6"></a>

## Add and require multiple

This is a helper function that attempts to install a list of packages
it will catch and warn about any packages that can't be found

    (defun add-and-require-multiple (&rest list)
      (dolist (required list)
        (unless (package-installed-p required)
          (condition-case nil
              (progn
                (package-install required)
                (and required (require required)))
            (error (warn (format "package %s failed to load" required)))))))

Here we use that function to add any packages that we need that can't be installed by use-package
as well as the installation of use-package itself

    (add-and-require-multiple
     'use-package ;can't include itself
    
     ;;use-package can't find these, so try other repos
     'undo-tree
     'smart-tabs-mode
    
     ;;Themes
     'ample-theme
     'monokai-theme)


<a id="org5b3974f"></a>

# Packages


<a id="orgfe11ce3"></a>

## Misc

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


<a id="org57d6c47"></a>

## Code formatting

Fill column indicator draws a horizontal line at a specified column distance
This is used for projects to avoid line wrapping

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

Optionally highlight trailing whitespace in red

    (use-package redspace-mode
      :bind ("C-c 1" . redspace-mode))

Smart tabs, auto for python and just use spaces instead of tabs for everything else

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

Org mode, used for this file
[Youtube tutorial and features](https://www.youtube.com/watch?v=SzA2YODtgK4&t=1533s)

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

Some python magic for finding files, code info, etc

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

For all those lisp parenthesis

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

Syntax highlighting for editing web files

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


<a id="org9e821cc"></a>

## Code editing

Editing multiple lines at once

    (use-package multiple-cursors
      :ensure t
      :bind
      ("C-c s" . mc/edit-lines)
      ("C-c n" . mc/mark-next-like-this)
      ("C-c p" . mc/mark-previous.like-this)
      ("C-c a" . mc/mark-more-like-this-extended))

pre-defined code snippets

    (use-package yasnippet
      :ensure t
      :config
      (progn
        (yas-global-mode 1)
        (use-package yasnippet-snippets
          :ensure t)))


<a id="orgf828764"></a>

## Auto complete

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


<a id="org4c67133"></a>

## Navigation

Finding usage of code

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

Jump around in a project

    (use-package projectile
      :ensure t
      :config
      (projectile-global-mode 1))

Amazing

    (use-package ace-jump-mode
      :ensure t
      :diminish ace-jump-mode
      :bind ("C-c SPC" . ace-jump-mode))

File tree visualizer

    (use-package neotree
      :ensure t
      :bind ("C-c 8" . neotree-toggle))

Undo tree shows a history of all undo's and re-do's. Since emacs has a smart undo instead of a linear undo system, this really helps.
This will automatically show up when you undo (C-x u)

    (use-package undo-tree
      :diminish undo-tree-mode
      :config
      (global-undo-tree-mode)
      (setq undo-tree-visualizer-timestamps t)
      (setq undo-tree-visualizer-diff t))


<a id="org62b3c81"></a>

## Interactive environments

common lisp environment

    (use-package slime
      :ensure t
      :config
      (setq inferior-lisp-program "/usr/bin/sbcl")
      (setq slime-contribs '(slime-fancy)))


<a id="org0df8b6a"></a>

# Custom functions

    ;;-------------------------------custom functions---------------------------------
    
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


<a id="org05dc446"></a>

# Keybindings

This is for keybindings that aren't package-specific

    ;;---------------------------------keybindings------------------------------------
    
    (setq next-line-add-newlines t) ;C-n now adds newline if at end of buffer
    (global-set-key (kbd "C-c f") 'toggle-fullscreen)
    (global-set-key (kbd "<C-tab>") 'dabbrev-expand)
    (global-set-key (kbd "C-c v") 'filename)
    
    ;; Different keybinds for macros
    (global-set-key (kbd "C-c r s") 'kmacro-start-macro-or-insert-counter)
    (global-set-key (kbd "C-c r e") 'kmacro-end-or-call-macro)


<a id="org87568c9"></a>

# Initialize settings

Set some indentation for specific modes, and start emacs fullscreen

    ;;---------------------------------Initialization---------------------------------
    
    (toggle-fullscreen)
    (setq inhibit-startup-message t)
    (put 'upcase-region 'disabled nil)
    
    ;;this will indent switch statements in c
    (c-set-offset 'case-label '+)
    
    ;;make c indent 4 by default instead of 2
    (setq-default c-basic-offset 4)

I don't want custom stuff being auto-incuded in init.el to keep it clean, so I create a custom file here
create the file if it doesn't exist and set it as my custom file

    ;;(setq mac-option-modifier 'meta)  ;enable this if using a mac
    (defconst custom-file (expand-file-name "custom.el" user-emacs-directory))
    
    (unless (file-exists-p custom-file)
      (write-region "" "" custom-file))
    
    (setq custom-file "~/.emacs.d/custom.el")
    (load custom-file)


<a id="org9f2105a"></a>

# Themes

These are all pretty much dark themes, except for ample-light 

    ;;---------------------------------Custom  Themes---------------------------------
    (load-theme 'wombat t)          ;neutral dark color scheme
    (load-theme 'tsdh-dark t)       ;another default emacs dark color scheme
    (load-theme 'monokai t)
    (load-theme 'ample t t)         ;these are all pretty nice, each gets a little lighter
    (load-theme 'ample-flat t t)
    (load-theme 'ample-light t t)   ;tan background
    
    (enable-theme 'monokai) ;our chosen theme, pick whatever you like

