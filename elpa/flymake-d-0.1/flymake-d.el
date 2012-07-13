;;; flymake-d.el --- A flymake handler for d-mode files
;;
;;; Author: Ben Grabham <bsg110@ic.ac.uk>
;;; Version: 0.1
;;;
;;; Keywords: flymake, d-mode, d
;;;
;;; Commentary:
;; Usage:
;;   (require 'flymake-d)
;;   (add-hook 'd-mode-hook 'flymake-d-load)
(require 'flymake)

;;; Code:

(defconst flymake-d-err-line-patterns '(("^\\(.*\\)(\\([0-9]+\\)): \\(.*\\)$" 1 2 nil 3)))

(defvar flymake-d-executable "dmd"
  "The D executable to use for syntax checking.")

(defun flymake-d--create-temp-in-system-tempdir (file-name prefix)
  "Return a temporary file name into which flymake can save.


This is tidier than `flymake-create-temp-inplace', and therefore
preferable when the checking doesn't depend on the file's exact
location."
  (make-temp-file (or prefix "flymake-d") nil ".d"))

;; Invoke dmd with -o- to not generate object files
(defun flymake-d-init ()
  "Construct a command that flymake can use to check D source."
  (list flymake-d-executable
	(list "-o-" (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))))
;;		     'flymake-d--create-temp-in-system-tempdir))))

;;;###autoload
(defun flymake-d-load ()
  "Configure flymake mode to check the current buffer's D syntax

This function is designed to be called in `d-mode-hook'; it
does not alter flymake's global configuration, so function
`flymake-mode' alone will not suffice."
  (interactive)
  (set (make-local-variable 'flymake-allowed-file-name-masks) '(("." flymake-d-init)))
  (set (make-local-variable 'flymake-err-line-patterns) flymake-d-err-line-patterns)
  (if (executable-find flymake-d-executable)
      (flymake-mode t)
    (message "Not enabling flymake: '%' command not found" flymake-d-executable)))


(provide 'flymake-d)
;;; flymake-d.el ends here
