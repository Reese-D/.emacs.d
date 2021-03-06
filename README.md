
# Table of Contents

1.  [Emacs Configuration](#orgadbaf3d)
    1.  [Initialization](#org5159f66)
    2.  [What's up with the zip file?](#orgca15b03)
    3.  [.org files](#org638b452)
    4.  [Keybindings](#org0143b02)
2.  [Common emacs commands for those who are new](#org95babc3)
    1.  [Simple definitions by example](#orgabedac2)
    2.  [common commands](#org90c7ab3)
        1.  [movement commands](#org963bc18)
        2.  [file commands](#org875dc1e)
        3.  [editing](#org7df1ec4)
        4.  [copy/paste](#orgc0b2f7e)
        5.  [macros](#org25dcd88)
        6.  [programming](#orgccf2dd4)
        7.  [window commands](#org80746ad)
        8.  [misc](#orga18fa0b)



<a id="orgadbaf3d"></a>

# Emacs Configuration

Tested with [emacs version 26.3](http://gnu.mirror.constant.com/emacs/emacs-26.3.tar.gz) in Debian 9 (windows subshell) & 10


<a id="org5159f66"></a>

## Initialization

Just clone this file in your home directory (~)

Emacs automatically looks for a file on the path ~/.emacs.d/init.el on startup

The init file is also setup to load in any files from the custom_elisp folder as well as the custom.el file

Don't edit the custom.el file, it's automatically generated


<a id="orgca15b03"></a>

## What's up with the zip file?

It's a backup, you can unzip it to make sure you have the same elpa directory used in testing, or you can just leave it alone/delete it

If you don't unzip the file, the elpa folder should automatically regenerate the first time you run emacs


<a id="org638b452"></a>

## .org files

These are used to manage the README (this file)

tangle: C-c C-v C-t
export: C-c C-e


<a id="org0143b02"></a>

## Keybindings

All custom keybindings can be found in the init.el file. 


<a id="org95babc3"></a>

# Common emacs commands for those who are new


<a id="orgabedac2"></a>

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


<a id="org90c7ab3"></a>

## common commands


<a id="org963bc18"></a>

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


<a id="org875dc1e"></a>

### file commands

    C-x C-s save current buffer
    C-x C-w save to new file
    C-x C-f find file
    C-x C-c quit emacs


<a id="org7df1ec4"></a>

### editing

    M-[n] repeat command [n] times
    C-u [n] repeat the next command [n] times
    C-t transpose two characters
    M-t transpose two words
    C-x C-t transpose two lines
    M-u uppercase letters from cursor to end of line
    M-l opposte of M-u
    M-c make first letter in word uppercase


<a id="orgc0b2f7e"></a>

### copy/paste

    C-k kill-line: delete the rest of the current line
    C-@ set-mark-command: mark is used to indicate the beginning of an area of text to be yanked
    C-w kill-region: delete the area of text between the mark and the current cursor position
    C-y yank: insert at current cursor location whatever was most recently deleted
    M-y yank-pop: cycle through your kill ring after yank
    M-w copy-region-as-kill: copy area between mark and cursor into kill-buffer so that it can be yanked into someplace else (doesn't delete)
    C-d delete character forward
    M-d delete word forward
    M-Del delete word backwards


<a id="org25dcd88"></a>

### macros

    C-x ( start macro definition
    C-x ) end of macro definition
    C-x e execute last definied macro
    C-x e execute last defined macro (can combine with C-u [n] or M-[n])
    M-x name-last-kbd-macro give name to macro (for saving)
    M-x insert-keyboard-macro save named macro into file
    M-x load-file load macro
    M-x macroname execute macroname


<a id="orgccf2dd4"></a>

### programming

    M C-\ indent region between cursor and mark
    M-m move to first (non-space) char in this line
    M-^ attach this line to previous
    M-; formatize and indent comment
    C-c C-c comment out marked area. 

    M-x outline-minor-mode collapses function definitions in a file to a mere {...}
    M-x show-subtree If you are in one of the collapsed functions, this un-collapses it In order to achive some of the feats coming up now you have to run etags *.c *.h *.cpp (or what ever ending you source files have) in the source directory
    M-. If you are in a function call, this will take you to it's definition
    M-x tags-search ENTER Searches through all you etaged
    M-, jumps to the next occurrence for tags-search
    M-x tags-query-replace yum. This lets you replace some text in all the tagged files


<a id="org80746ad"></a>

### window commands

    C-x 3 split window vertically
    C-x 2 split window horizontally
    C-x 1 kill all windows except the current window
    C-x 0 kill only the current window
    C-x o move to next window


<a id="orga18fa0b"></a>

### misc

    C-g keyboard-quit: if while typing a command you make a mistake and want to stop, this aborts a command in progress

