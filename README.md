rbenv.el
========

use rbenv to manage your Ruby versions within Emacs

Installation
------------

```lisp
(add-to-list 'load-path (expand-file-name "/path/to/rbenv.el/"))
(require 'rbenv)
(global-rbenv-mode)
```

Usage
-----

* `global-rbenv-mode` activate / deactivate rbenv.el (The current Ruby version is shown in the modeline)
* `rbenv-use-global` will activate your global ruby
* `rbenv-use` allows you to choose what ruby version you want to use
* `rbenv-use-corresponding` searches for .ruby-version and activates
  the corresponding ruby

Configuration
-------------

**rbenv installation directory**
By default rbenv.el assumes that you installed rbenv into
`~/.rbenv`. If you use a different installation location you can
customize rbenv.el to search in the right place:

```lisp
(setq rbenv-installation-dir "/usr/local/rbenv")
```

*IMPORTANT:*: Currently you need to set this variable before you load rbenv.el

**the modeline**
rbenv.el will show you the active ruby in the modeline. If you don't
like this feature you can disable it:

```lisp
(setq rbenv-show-active-ruby-in-modeline nil)
```

The default modeline representation is the ruby version (colored red) in square
brackets. You can change the format by customizing the variable:

```lisp
;; this will remove the colors
(setq rbenv-modeline-function 'rbenv--modeline-plain)
```

You can also define your own function to format the ruby version as you like.

Press
-----

If you want to read more about rbenv.el check out the following links:

* [Use the right Ruby with emacs and rbenv](http://blog.senny.ch/blog/2013/02/11/use-the-right-ruby-with-emacs-and-rbenv/) by Yves Senn

[![githalytics.com alpha](https://cruel-carlota.pagodabox.com/f4c783738c250ce724df3c5b9753a786 "githalytics.com")](http://githalytics.com/senny/rbenv.el)
