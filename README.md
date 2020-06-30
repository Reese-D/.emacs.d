# my_emacs

Tested with emacs version 26.3 in debian

### Initialization

Simply clone this in your home directory (~). That's it! Emacs should automatically look for the file ~/.emacs.d/init.el on startup

You can create a directory called scripts in this folder, and any files should be automatically loaded in by the init.el file on startup

### What's with the zip file

Honestly it's just a backup, you can remove the elpa-zip.zip file, it's completely unused. I just keep it around incase melpa is ever re-building or something.

### Keybindings

Most keybindings are listed in the init.el file, look for the "use-package" calls, most of these will have a :bind section that will display any modified keybindings. There are a couple further down in the file that aren't related to any specific package


