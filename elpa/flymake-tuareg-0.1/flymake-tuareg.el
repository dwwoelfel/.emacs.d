;;; flymake-tuareg.el --- A flymake handler for tuareg-mode files
;;
;;; Author: Ben Grabham <bsg110@ic.ac.uk>
;;; Version: 0.1
;;;
;;; Keywords: flymake, tuareg-mode, ocaml, caml, tuareg
;;;
;;; Commentary:
;; Usage:
;;   (require 'flymake-tuareg)
;;   (add-hook 'tuareg-mode-hook 'flymake-tuareg-load)
(require 'flymake)

;;; Code:

(defconst flymake-tuareg-err-line-patterns
  '(("File \"\\(.*\\)\", line \\([0-9]+\\), characters \\([0-9]+\\)-[0-9]+: \\(.*\\)$"
     1 2 3 4)))

(defvar flymake-tuareg-executable (concat (file-name-directory load-file-name) "ocamlchecksyntax.sh")
  "The tuareg executable for syntax checking.")

(defun flymake-tuareg--create-temp-in-system-tempdir (file-name prefix)
  "Return a temporary file name into which flymake can save.


This is tidier than `flymake-create-temp-inplace', and therefore
preferable when the checking doesn't depend on the file's exact
location."
  (make-temp-file (or prefix "flymake-tuareg") nil ".d"))

(defun flymake-tuareg-init ()
  "Construct a command that flymake can use to check D source."
  (list flymake-tuareg-executable (list (flymake-init-create-temp-buffer-copy
					       'flymake-create-temp-inplace))))

;;;###autoload
(defun flymake-tuareg-load ()
  "Configure flymake mode to check the current buffer's D syntax

This function is designed to be called in `tuareg-mode-hook'; it
does not alter flymake's global configuration, so function
`flymake-mode' alone will not suffice."
  (interactive)
  (set (make-local-variable 'flymake-allowed-file-name-masks) '(("." flymake-tuareg-init)))
  (set (make-local-variable 'flymake-err-line-patterns) flymake-tuareg-err-line-patterns)
  (flymake-mode t))

(provide 'flymake-tuareg)
;;; flymake-tuareg.el ends here
