# rbenv.el

Use [rbenv](https://github.com/rbenv/rbenv) to manage your Ruby versions within GNU Emacs.

## Installation

* Native:

    1. Clone this repo.
    1. Add into `init.el`:

        ```emacs-lisp
        (add-to-list 'load-path (expand-file-name "/path/to/rbenv.el/"))
        (require 'rbenv)
        (global-rbenv-mode)
        ```

* `package.el`:

    ```emacs-lisp
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
    (package-initialize)

    (unless package-archive-contents
      (package-refresh-contents))

    (unless (package-installed-p 'rbenv)
      (package-install 'rbenv))

    (require 'rbenv)
    (global-rbenv-mode)
    ```

* `use-package`:

    ```emacs-lisp
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

    (package-initialize)

    (unless package-archive-contents
      (package-refresh-contents))

    (use-package rbenv
      :ensure t
      :config (global-rbenv-mode))
    ```

## Usage

* `global-rbenv-mode` — activate / deactivate `rbenv.el` (The current Ruby version is shown in the modeline).
* `rbenv-use-global` — activate your global Ruby interpreter.
* `rbenv-use` — allows you to choose what Ruby version you want to use.
* `rbenv-use-corresponding` — searches for `.ruby-version` and activates the corresponding Ruby interpreter.

## Configuration

### rbenv installation directory

By default `rbenv.el` assumes that you installed `rbenv` into `~/.rbenv`. If you use a different installation location you can
customize `rbenv-installation-dir` variable to search in the right place:

```emacs-lisp
(customize-set-variable 'rbenv-installation-dir "/usr/local/rbenv")
```

**IMPORTANT:** Currently you need to set this variable before you load `rbenv.el`:

```emacs-lisp
(use-package rbenv
  :ensure t
  :init
  (customize-set-variable 'rbenv-installation-dir "/usr/local/rbenv")
  :config (global-rbenv-mode))
```

### The modeline

`rbenv.el` will show you the active Ruby in the modeline. If you don't like this feature you can disable it:

```emacs-lisp
(customize-set-variable 'rbenv-show-active-ruby-in-modeline nil)
```

The default modeline representation is the Ruby version (colored red) in square brackets. You can change the format by customizing the variable `rbenv-modeline-function`:

```emacs-lisp
;; this will remove the colors
(customize-set-variable 'rbenv-modeline-function 'rbenv--modeline-plain)
```

You can also define your own function to format the Ruby version as you like.

## Press

If you want to read more about `rbenv.el` check out the following links:

* [Use the right Ruby with emacs and rbenv](http://blog.senny.ch/blog/2013/02/11/use-the-right-ruby-with-emacs-and-rbenv/) by Yves Senn

[![githalytics.com alpha](https://cruel-carlota.pagodabox.com/f4c783738c250ce724df3c5b9753a786 "githalytics.com")](http://githalytics.com/senny/rbenv.el)
