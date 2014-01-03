(defun set-path ()
  (if (not (getenv "TERM_PROGRAM"))
      (let ((path (shell-command-to-string
                   "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
        (setenv "PATH" path)
        (setq exec-path (split-string path ":")))))

(defun mac-copy ()
  (shell-command-to-string "pbpaste"))

(defun mac-paste (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'mac-paste)
(setq interprogram-paste-function 'mac-copy)
(set-language-environment "UTF-8")
(set-path)

(provide 'mac-bs)
