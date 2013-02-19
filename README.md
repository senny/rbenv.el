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

* `global-rbenv-mode` activate / deactivate rbenv.el

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

Press
-----

If you want to read more about rbenv.el check out the following links:

* [Use the right Ruby with emacs and rbenv](http://blog.senny.ch/blog/2013/02/11/use-the-right-ruby-with-emacs-and-rbenv/) by Yves Senn
