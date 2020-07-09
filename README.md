
# Table of Contents

1.  [Emacs Configuration](#orgbe7f2bb)
    1.  [Initialization](#orgf7ecbc0)
    2.  [What's up with the zip file?](#org82f7fa8)
    3.  [.org files](#org73a0636)
    4.  [Keybindings](#org14675d5)
    5.  [init.el documentation](#org2d3ae0a)



<a id="orgbe7f2bb"></a>

# Emacs Configuration

Tested with emacs version 26.3 in Debian 9 (windows subshell) & 10


<a id="orgf7ecbc0"></a>

## Initialization

Just clone this file in your home directory (~)

Emacs automatically looks for a file on the path ~/.emacs.d/init.el on startup

The init file is also setup to load in any files from the custom<sub>elisp</sub> folder as well as the custom.el file

Don't edit the custom.el file, it's automatically generated


<a id="org82f7fa8"></a>

## What's up with the zip file?

It's a backup, you can unzip it to make sure you have the same elpa directory used in testing, or you can just leave it alone/delete it

The elpa file should automatically regenerate


<a id="org73a0636"></a>

## .org files

These are used to manage the README (this file) and the init.el files


<a id="org14675d5"></a>

## Keybindings

All custom keybindings can be found in the init.el file. 


<a id="org2d3ae0a"></a>

## init.el documentation

Since the init.el file is managed with org mode, i've created a markdown file to give simple documentation of the file.

