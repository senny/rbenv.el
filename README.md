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
