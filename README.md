
# Table of Contents

1.  [Emacs Configuration](#org33eef64)
    1.  [Initialization](#org95070d9)
    2.  [What's up with the zip file?](#orgb4ff693)
    3.  [.org files](#org3dd8118)
    4.  [Keybindings](#orge73b06b)
    5.  [init.el documentation](#org0d0f8c3)
2.  [Common emacs commands for those who are new](#org345ffba)
    1.  [Simple definitions by example](#org79d0044)
    2.  [common commands](#org4cb45fa)
        1.  [movement commands](#orgd03cbc2)
        2.  [file commands](#org9517ba5)
        3.  [copy/paste](#org040230b)
        4.  [window commands](#org2caf867)
        5.  [misc](#org83455df)



<a id="org33eef64"></a>

# Emacs Configuration

Tested with [emacs version 26.3](http://gnu.mirror.constant.com/emacs/emacs-26.3.tar.gz) in Debian 9 (windows subshell) & 10


<a id="org95070d9"></a>

## Initialization

Just clone this file in your home directory (~)

Emacs automatically looks for a file on the path ~/.emacs.d/init.el on startup

The init file is also setup to load in any files from the custom_elisp folder as well as the custom.el file

Don't edit the custom.el file, it's automatically generated


<a id="orgb4ff693"></a>

## What's up with the zip file?

It's a backup, you can unzip it to make sure you have the same elpa directory used in testing, or you can just leave it alone/delete it

If you don't unzip the file, the elpa folder should automatically regenerate the first time you run emacs


<a id="org3dd8118"></a>

## .org files

These are used to manage the README (this file) and the init.el files


<a id="orge73b06b"></a>

## Keybindings

All custom keybindings can be found in the init.el file. 


<a id="org0d0f8c3"></a>

## init.el documentation

Since the [init.el](init.el) file is managed with org mode, i've created a markdown file in the home directory [init.md](init.md) that includes some documentation


<a id="org345ffba"></a>

# Common emacs commands for those who are new


<a id="org79d0044"></a>

## Simple definitions by example

    M stands for "Meta" and by default is the alt key
    C stands for "Control" and is default the control key

    "Buffer" is the open text area, it's not called a window since you can split your screen into multiple buffers

    C-f          Means hold Ctrl and press f
    M-p          Means hold alt (meta) and press p
    C-x f        Means hold Ctrl and press x, release Ctrl and press f
    C-x C-c      Means hold Ctrl and press x, keep Ctrl down andpress c
    C-M-e        Region is the word for selection
    M-% or M-S-5 Means alt + shift + 5


<a id="org4cb45fa"></a>

## common commands


<a id="orgd03cbc2"></a>

### movement commands

    C-f move one character forwards
    M-f move one word foreward
    C-b move one character backwards
    M-b move one word backwards

    C-p move to previous line
    C-n move to next line
    C-a move to start of line
    C-e move to end of line

    M-< move to the beginning of the buffer
    M-> move to end of buffer
    
    C-v page down
    M-v page up
    C-l center page on cursor (multiple clicks move it to top/bottom instead of center)
    
    M-^ move current line and append it to the prior line, removing all whitespace and adding back in a single space
    
    C-F1 remember position in code (all function keys)
    F1   return to position in code


<a id="org9517ba5"></a>

### file commands

    C-x C-s save current buffer
    C-x C-w save to new file
    C-x C-f find file
    C-x C-c quit emacs


<a id="org040230b"></a>

### copy/paste

    C-k kill-line: delete the rest of the current line
    C-@ set-mark-command: mark is used to indicate the beginning of an area of text to be yanked
    C-w kill-region: delete the area of text between the mark and the current cursor position
    C-y yank: insert at current cursor location whatever was most recently deleted
    M-y yank-pop: cycle through your kill ring after yank
    M-w copy-region-as-kill: copy area between mark and cursor into kill-buffer so that it can be yanked into someplace else (doesn't delete)


<a id="org2caf867"></a>

### window commands

    C-x 3 split window vertically
    C-x 2 split window horizontally
    C-x 1 kill all windows except the current window
    C-x 0 kill only the current window
    C-x o move to next window


<a id="org83455df"></a>

### misc

    C-g keyboard-quit: if while typing a command you make a mistake and want to stop, this aborts a command in progress

