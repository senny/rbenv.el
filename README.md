rbenv.el
========

use rbenv to manage your Ruby versions within Emacs

Installation
------------

```lisp
(add-to-list 'load-path (expand-file-name "/path/to/rbenv.el/"))
(require 'rbenv)
(rbenv-use-global)
```

Usage
-----

* `rbenv-use-global` will activate your global ruby
* `rbenv-use` allows you to choose what ruby version you want to use
* `rbenv-use-corresponding` searches for .ruby-version and activates
  the corresponding ruby

Press
-----

If you want to read more about rbenv.el check out the following links:

* [Use the right Ruby with emacs and rbenv](http://blog.senny.ch/blog/2013/02/11/use-the-right-ruby-with-emacs-and-rbenv/) by Yves Senn
