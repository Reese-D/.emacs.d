
# Table of Contents

1.  [Emacs Configuration](#org38be69a)
    1.  [Initialization](#org26ac887)
    2.  [What's up with the zip file?](#org8b222ef)
    3.  [.org files](#org84e35c5)
    4.  [Keybindings](#orgb925c41)
    5.  [init.el documentation](#org0723d77)



<a id="org38be69a"></a>

# Emacs Configuration

Tested with [emacs version 26.3](http://gnu.mirror.constant.com/emacs/emacs-26.3.tar.gz) in Debian 9 (windows subshell) & 10


<a id="org26ac887"></a>

## Initialization

Just clone this file in your home directory (~)

Emacs automatically looks for a file on the path ~/.emacs.d/init.el on startup

The init file is also setup to load in any files from the custom<sub>elisp</sub> folder as well as the custom.el file

Don't edit the custom.el file, it's automatically generated


<a id="org8b222ef"></a>

## What's up with the zip file?

It's a backup, you can unzip it to make sure you have the same elpa directory used in testing, or you can just leave it alone/delete it

If you don't unzip the file, the elpa folder should automatically regenerate the first time you run emacs


<a id="org84e35c5"></a>

## .org files

These are used to manage the README (this file) and the init.el files


<a id="orgb925c41"></a>

## Keybindings

All custom keybindings can be found in the init.el file. 


<a id="org0723d77"></a>

## init.el documentation

Since the init.el file is managed with org mode, i've created a markdown file in the home directory (init.md) that includes some documentation

