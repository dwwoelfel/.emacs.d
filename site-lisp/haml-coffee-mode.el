;;; haml-coffee-mode.el --- Minor mode to compile HamlCoffee files in Emacs

;; Copyright (C) 2012 Daniel Woelfel

;; Version: 0.1.0
;; Keywords: HamlCoffee minor mode
;; Author: Daniel Woelfel <daniel@danielwoelfel.com>
;; URL: http://github.com/dwwoelfel/haml-coffee-mode

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary

;; For commentary please see the README.md or
;; http://github.com/dwwoelfel/haml-coffee-mode#readme

;;; Installation

;; In your shell:

;;     $ cd ~/.emacs.d/vendor
;;     $ git clone git://github.com/dwwoelfel/haml-coffee-mode.git

;; In your emacs config:

;;     (add-to-list 'load-path "~/.emacs.d/vendor/haml-coffee-mode")
;;     TODO: This should be a hook
;;     (require 'haml-coffee-mode)

;;; Thanks

;; Major thanks to http://github.com/defunkt/coffee-mode
;; This mode is a blantant rip-off of coffee-mode

;;; Code:

(require 'haml-mode)

(defconst haml-coffee-mode-version "0.1.0"
  "The version of this `haml-coffee-mode'.")

(defgroup haml-coffee nil
	"A HamlCoffee minor mode meant to supplement Haml-mode"
	:group 'languages)

(defcustom haml-coffee-js-mode 'js2-mode
  "The mode to use when viewing compiled JavaScript."
  :type 'string
  :group 'haml-coffee)

(defcustom haml-coffee-compiled-buffer-name "*haml-coffee-compiled*"
  "The name of the scratch buffer used for compiled HamlCoffee."
  :type 'string
  :group 'haml-coffee)

(defcustom haml-coffee-command "haml-coffee"
  "The Haml-Coffee command used for evaluating code. Must be in your
path."
  :type 'string
  :group 'haml-coffee)

(defcustom haml-coffee-args-compile '("--disable-html-escaping")
  "The command line args to pass to `haml-coffee-command' when compiling a file."
  :type 'list
  :group 'haml-coffee)

(defvar haml-coffee-mode-hook nil
  "A hook for you to run your own code when the mode is loaded.")

;; TODO: Custom namespace
;; (defcustom haml-coffee-namespace nil
;;   "If non-nil, will change template namespace"
;;   :type 'string
;;   :group 'haml-coffee)

;; TODO: Custom template name
;; (defcustom haml-coffee-root-path nil
;;   "If non-nil, template name will use remaining portion of path"
;;   :type 'string
;;   :group 'haml-coffee)

;;
;; Compat
;;

(unless (fboundp 'apply-partially)
  (defun apply-partially (fun &rest args)
    "Return a function that is a partial application of FUN to ARGS.
ARGS is a list of the first N arguments to pass to FUN.
The result is a new function which does the same as FUN, except that
the first N arguments are fixed at the values with which this function
was called."
    (lexical-let ((fun fun) (args1 args))
      (lambda (&rest args2) (apply fun (append args1 args2))))))

;;
;; Macros
;;

(defmacro haml-coffee-line-as-string ()
  "Returns the current line as a string."
  `(buffer-substring (point-at-bol) (point-at-eol)))

;;
;; Commands
;;

;; TODO: Custom template name
(defun haml-coffee-template-name ()
	"Determines the template name from the buffer name. Must be called when buffer to compile is selected!"
	(interactive)
	(replace-regexp-in-string
	 "[\s-]+" "_"
	 (replace-regexp-in-string
		"\\(\.html\\)?.haml[c]?$" "" (buffer-name))))

(defun haml-coffee-compile-buffer ()
	"Compiles the current buffer and displays the JS in another buffer."
	(interactive)
	(save-excursion
		(haml-coffee-compile-region (point-min) (point-max))))

(defun haml-coffee-compile-region (start end)
	"Compiles a region and displays the JS in another buffer."
	(interactive "r")

	(let ((buffer (get-buffer haml-coffee-compiled-buffer-name)))
		(when buffer
			(kill-buffer buffer)))

	(apply (apply-partially 'call-process-region start end haml-coffee-command
													nil
													(get-buffer-create haml-coffee-compiled-buffer-name)
													nil)
				 (append haml-coffee-args-compile (list "-t" (haml-coffee-template-name))))
	(switch-to-buffer (get-buffer haml-coffee-compiled-buffer-name))
	(funcall haml-coffee-js-mode)
	(goto-char (point-min)))

(provide 'haml-coffee-mode)
