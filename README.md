
# Table of Contents

1.  [Emacs Configuration](#org43d9361)
    1.  [Initialization](#orgc4d2094)
    2.  [What's up with the zip file?](#org6ffd5f3)
    3.  [.org files](#org54f7dc2)
    4.  [Keybindings](#org900192c)
    5.  [init.el documentation](#orgf453f74)



<a id="org43d9361"></a>

# Emacs Configuration

Tested with [emacs version 26.3](http://gnu.mirror.constant.com/emacs/emacs-26.3.tar.gz) in Debian 9 (windows subshell) & 10


<a id="orgc4d2094"></a>

## Initialization

Just clone this file in your home directory (~)

Emacs automatically looks for a file on the path ~/.emacs.d/init.el on startup

The init file is also setup to load in any files from the custom_elisp folder as well as the custom.el file

Don't edit the custom.el file, it's automatically generated


<a id="org6ffd5f3"></a>

## What's up with the zip file?

It's a backup, you can unzip it to make sure you have the same elpa directory used in testing, or you can just leave it alone/delete it

If you don't unzip the file, the elpa folder should automatically regenerate the first time you run emacs


<a id="org54f7dc2"></a>

## .org files

These are used to manage the README (this file) and the init.el files


<a id="org900192c"></a>

## Keybindings

All custom keybindings can be found in the init.el file. 


<a id="orgf453f74"></a>

## init.el documentation

Since the [init.el](init.el) file is managed with org mode, i've created a markdown file in the home directory [init.md](init.md) that includes some documentation

