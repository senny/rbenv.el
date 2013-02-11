;;; rbenv.el --- Emacs integration for rbenv

;; Copyright (C) 2010-2011 Yves Senn

;; URL: https://github.com/senny/rbenv.el
;; Author: Yves Senn <yves.senn@gmail.com>
;; Version: 0.0.1
;; Created: 10 February 2013
;; Keywords: ruby rbenv

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; M-x rbenv-use-global prepares the current Emacs session to use
;; the global ruby configured with rbenv.

;; M-x rbenv-use allows you to switch the current session to the ruby
;; implementation of your choice.

;;; Compiler support:

;; helper function used in variable definitions
(defun rbenv--expand-path (&rest segments)
  (let ((path (mapconcat 'identity segments "/")))
    (expand-file-name (concat (getenv "HOME") "/.rbenv/" path))))

(defcustom rbenv-interactive-completion-function
  (if ido-mode 'ido-completing-read 'completing-read)
  "The function which is used by rbenv.el to interactivly complete user input"
  :group 'rbenv
  :type 'function)

(defvar rbenv-executable (expand-file-name "~/.rbenv/bin/rbenv")
  "path to the rbenv executable")

(defvar rbenv-global-version-file (expand-file-name "~/.rbenv/version")
  "path to the global version configuration file of rbenv")

(defvar rbenv-version-environment-variable "RBENV_VERSION"
  "name of the environment variable to configure the rbenv version")

(defvar rbenv-binary-paths (list (cons 'shims-path (rbenv--expand-path "shims"))
                                 (cons 'bin-path (rbenv--expand-path "bin")))
  "these are added to PATH and exec-path when rbenv is setup")

(defvar rbenv--initialized nil
  "indicates if the current Emacs session has been configured to use rbenv")

;;;###autoload
(defun rbenv-use-global ()
  "activate rbenv global ruby"
  (interactive)
  (rbenv-use (rbenv--global-ruby-version)))

;;;###autoload
(defun rbenv-use-corresponding ()
  "search for .ruby-version and activate the corresponding ruby"
  (interactive)
  (let ((version-file-path (or (rbenv--locate-file ".ruby-version")
                               (rbenv--locate-file ".rbenv-version"))))
    (if version-file-path (rbenv-use (rbenv--read-version-from-file version-file-path))
      (message "[rbenv] could not locate .ruby-version or .rbenv-version"))))

;;;###autoload
(defun rbenv-use (ruby-version)
  "choose what ruby you want to activate"
  (interactive
   (let ((picked-ruby (rbenv--completing-read "Ruby version: " (rbenv/list))))
     (list picked-ruby)))
  (rbenv--activate ruby-version)
  (message (concat "[rbenv] using " ruby-version)))

(defun rbenv/list ()
  (split-string (rbenv--call-process "versions" "--bare") "\n"))

(defun rbenv--setup ()
  (when (not rbenv--initialized)
    (dolist (path-config rbenv-binary-paths)
      (let ((bin-path (cdr path-config)))
        (setenv "PATH" (concat bin-path ":" (getenv "PATH")))
        (add-to-list 'exec-path bin-path)))
    (setq eshell-path-env (getenv "PATH"))
    (setq rbenv--initialized t)))

(defun rbenv--activate (ruby-version)
  (rbenv--setup)
  (setenv rbenv-version-environment-variable ruby-version))

(defun rbenv--completing-read (prompt options)
  (funcall rbenv-interactive-completion-function prompt options))

(defun rbenv--binary-path (ruby-version)
  (rbenv--expand-path "versions" ruby-version "bin"))

(defun rbenv--global-ruby-version ()
  (rbenv--read-version-from-file rbenv-global-version-file))

(defun rbenv--read-version-from-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (rbenv--replace-trailing-whitespace (buffer-substring-no-properties (point-min) (point-max)))))

(defun rbenv--locate-file (file-name)
  "searches the directory tree for an given file. Returns nil if the file was not found."
  (let ((directory (locate-dominating-file (expand-file-name buffer-file-name) file-name)))
    (when directory (concat directory file-name))))

(defun rbenv--call-process (&rest args)
  (with-temp-buffer
    (let* ((success (apply 'call-process rbenv-executable nil t nil
                           (delete nil args)))
           (raw-output (buffer-substring-no-properties
                        (point-min) (point-max)))
           (output (rbenv--replace-trailing-whitespace raw-output)))
      (if (= 0 success)
          output
        (message output)))))

(defun rbenv--replace-trailing-whitespace (text)
  (replace-regexp-in-string "[[:space:]]\\'" "" text))

(provide 'rbenv)

;;; rbenv.el ends here
