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

(defcustom rbenv-interactive-completion-function
  (if ido-mode 'ido-completing-read 'completing-read)
  "The function which is used by rbenv.el to interactivly complete user input"
  :group 'rbenv
  :type 'function)

(defvar rbenv-executable (expand-file-name "~/.rbenv/bin/rbenv")
  "path to the rbenv executable")

(defvar rbenv-global-version-file (expand-file-name "~/.rbenv/version")
  "path to the global version configuration file of rbenv")

(defvar rbenv--current-version nil
  "reflects the currently active rbenv ruby version")

(defun rbenv-use-global ()
  "activate rbenv global ruby"
  (interactive)
  (rbenv-use (rbenv--global-ruby-version)))

(defun rbenv-use (ruby-version)
  "choose what ruby you want to activate"
  (interactive
   (let ((picked-ruby (rbenv--completing-read "Ruby version: " (rbenv/list))))
     (list picked-ruby)))
  (rbenv--activate ruby-version)
  (message (concat "[rbenv] using " ruby-version)))

(defun rbenv/list ()
  (split-string (rbenv--call-process "versions" "--bare") "\n"))

(defun rbenv--activate (ruby-version)
  (let ((binary-path (rbenv--binary-path ruby-version)))
    (when rbenv--current-version
      (let ((old-binary-path (rbenv--binary-path rbenv--current-version)))
        (setenv "PATH" (replace-regexp-in-string
                        (regexp-quote (concat old-binary-path ":"))
                        ""
                        (getenv "PATH")))
        (setq exec-path (remove old-binary-path exec-path))))
    (setenv "PATH" (concat binary-path ":" (getenv "PATH")))
    (setq eshell-path-env (getenv "PATH"))
    (add-to-list 'exec-path binary-path))
  (setq rbenv--current-version ruby-version))

(defun rbenv--completing-read (prompt options)
  (funcall rbenv-interactive-completion-function prompt options))

(defun rbenv--binary-path (ruby-version)
  (rbenv--expand-path "versions" ruby-version "bin"))

(defun rbenv--expand-path (&rest segments)
  (let ((path (mapconcat 'identity segments "/")))
    (expand-file-name (concat (getenv "HOME") "/.rbenv/" path))))

(defun rbenv--global-ruby-version ()
  (with-temp-buffer
    (insert-file-contents rbenv-global-version-file)
    (rbenv--replace-trailing-whitespace (buffer-substring-no-properties (point-min) (point-max)))))

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
