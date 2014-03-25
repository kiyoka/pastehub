Install emacs client
=======================
> **NOTE**
> If your emacs support OS native clipboard, you don't have to install emacs-lisp. Please install pastehub gem only.

----

## Required platforms

- Linux or MacOS X
- Emacs 24.1 or later

----

## installation instruction

### Setup ruby gems

+ see [RubyGems](./ruby_gems.md)

### Setup melpa
add this code your .emacs

	(require 'package)
	(add-to-list 'package-archives
	            '("melpa" . "http://melpa.milkbox.net/packages/") t)
	(package-initialize)


### install the "pastehub" package from MELPA.

  ![pastehub_el](pastehub_el_on_melpa.png)

### add to your .emacs

	(setq pastehub-client-basepath "/usr/local/bin") ;; for Emacs.app on MacOS X
	(require 'pastehub)

### PasteHub's mode line appears 

  ![modeline](emacs_mode_line.png)

----
