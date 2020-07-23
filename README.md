
# Table of Contents

1.  [Emacs Configuration](#org4fb142e)
    1.  [Initialization](#org35aa45c)
    2.  [What's up with the zip file?](#orgd51339e)
    3.  [.org files](#org0429128)
    4.  [Keybindings](#org9da14b9)
2.  [Common emacs commands for those who are new](#orgd80ee92)
    1.  [Simple definitions by example](#orga129eec)
    2.  [common commands](#org09821ee)
        1.  [movement commands](#orgeccad06)
        2.  [file commands](#org816991c)
        3.  [copy/paste](#org474300b)
        4.  [window commands](#org91807a0)
        5.  [misc](#orgd2a32db)



<a id="org4fb142e"></a>

# Emacs Configuration

Tested with [emacs version 26.3](http://gnu.mirror.constant.com/emacs/emacs-26.3.tar.gz) in Debian 9 (windows subshell) & 10


<a id="org35aa45c"></a>

## Initialization

Just clone this file in your home directory (~)

Emacs automatically looks for a file on the path ~/.emacs.d/init.el on startup

The init file is also setup to load in any files from the custom_elisp folder as well as the custom.el file

Don't edit the custom.el file, it's automatically generated


<a id="orgd51339e"></a>

## What's up with the zip file?

It's a backup, you can unzip it to make sure you have the same elpa directory used in testing, or you can just leave it alone/delete it

If you don't unzip the file, the elpa folder should automatically regenerate the first time you run emacs


<a id="org0429128"></a>

## .org files

These are used to manage the README (this file)

tangle: C-c C-v C-t
export: C-c C-e


<a id="org9da14b9"></a>

## Keybindings

All custom keybindings can be found in the init.el file. 


<a id="orgd80ee92"></a>

# Common emacs commands for those who are new


<a id="orga129eec"></a>

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


<a id="org09821ee"></a>

## common commands


<a id="orgeccad06"></a>

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


<a id="org816991c"></a>

### file commands

    C-x C-s save current buffer
    C-x C-w save to new file
    C-x C-f find file
    C-x C-c quit emacs


<a id="org474300b"></a>

### copy/paste

    C-k kill-line: delete the rest of the current line
    C-@ set-mark-command: mark is used to indicate the beginning of an area of text to be yanked
    C-w kill-region: delete the area of text between the mark and the current cursor position
    C-y yank: insert at current cursor location whatever was most recently deleted
    M-y yank-pop: cycle through your kill ring after yank
    M-w copy-region-as-kill: copy area between mark and cursor into kill-buffer so that it can be yanked into someplace else (doesn't delete)


<a id="org91807a0"></a>

### window commands

    C-x 3 split window vertically
    C-x 2 split window horizontally
    C-x 1 kill all windows except the current window
    C-x 0 kill only the current window
    C-x o move to next window


<a id="orgd2a32db"></a>

### misc

    C-g keyboard-quit: if while typing a command you make a mistake and want to stop, this aborts a command in progress

