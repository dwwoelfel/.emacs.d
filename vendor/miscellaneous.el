(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; Still a good idea?
(setq ispell-program-name "/usr/local/bin/hunspell")
(setq flyspell-issue-message-flag nil)

(setq-default display-buffer-reuse-frames nil)

(setq cua-auto-tabify-rectangles nil)

(winner-mode 1)

(show-paren-mode t)

(require 'ido)
(ido-mode t)
(add-hook 'ido-setup-hook
 (lambda ()
   ;; Go straight home
   (define-key ido-file-completion-map
     (kbd "~")
     (lambda ()
       (interactive)
       (if (looking-back "/")
           (insert "~/")
         (call-interactively 'self-insert-command))))))


(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "/home/daniel/.places")

(setq completion-cycle-threshold 5)
(add-to-list 'completion-styles 'substring)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq initial-scratch-message "")
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(setq frame-title-format "%b")
(setq default-tab-width 2)
(setq initial-major-mode 'fundamental-mode)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq backup-by-copying t      ; don't clobber symlinks
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      '(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/tmp/\\2" t)
        ("\\`/?\\([^/]*/\\)*\\([^/]*\\)\\'" "/home/daniel/.saves/\\2" t)))

(setq tramp-default-method "ssh")

(defun whack-whitespace (arg)
  "Delete all white space from point to the next word.  With prefix ARG
    delete across newlines as well.  The only danger in this is that you
    don't have to actually be at the end of a word to make it work.  It
    skips over to the next whitespace and then whacks it all to the next
    word."
  (interactive "P")
  (let ((regexp (if arg "[ \t\n]+" "[ \t\n]+")))
    (re-search-forward regexp nil t)
    (replace-match "" nil nil)))

(defun quick-copy-line ()
  "Copy the whole line that point is on and move to the beginning of the next line.
    Consecutive calls to this command append each line to the
    kill-ring."
  (interactive)
  (let ((beg (line-beginning-position 1))
        (end (line-beginning-position 2)))
    (if (eq last-command 'quick-copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-new (buffer-substring beg end))))
  (beginning-of-line 2))

(defun djcb-find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
   White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" ""
    (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun shell-command-to-string-with-return-code (command)
  "Execute shell command COMMAND and return its output as a string."
  (let* ((return-code)
   (result (with-output-to-string
           (with-current-buffer standard-output
              (setq return-code
               (call-process shell-file-name nil
                 t nil shell-command-switch command))))))
    (list return-code result)))

(defun find-gem (gem)
  "Open a directory with the gem via Bundler."
  (interactive "sGem: ")
  (let* ((cmd (concat "bundle show " gem " --no-color"))
         (result (shell-command-to-string-with-return-code cmd)))
    (if (= (car result) 0)
  (find-file (trim-string (cadr result)))
      (message (trim-string (cadr result))))))

(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(defun fc-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-x F") 'djcb-find-file-as-root)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "<f10>") 'revert-buffer)
(global-set-key (kbd "<f9>") 'magit-status)
(global-set-key (kbd "<f8>") 'delete-trailing-whitespace)
(global-set-key (kbd "<f7>") 'whack-whitespace)
(global-set-key "\M- " 'hippie-expand)
(global-set-key (kbd "C-|") 'align)
(global-set-key (kbd "C-M-|") 'align-regexp)
(global-set-key (kbd "C-c C-k") 'camelscore-word-at-point)
(global-set-key (kbd "M-#") 'comment-or-uncomment-region)
(global-set-key (kbd "C->") 'end-of-buffer)
(global-set-key (kbd "C-<") 'beginning-of-buffer)
(global-set-key (kbd "C-%") 'goto-match-paren)
(global-set-key (kbd "C-c C-e") 'eval-buffer)
(global-set-key (kbd "M-S-J")
                (lambda ()
                  (interactive)
                  (join-line -1)))
(global-set-key (kbd "C-c I") 'find-user-init-file)
(global-set-key (kbd "C-c C-x C-e") 'fc-eval-and-replace)


(defun replace-hash-rockets ()
  (interactive)
  (query-replace-regexp ":\\([^[:space:]]+\\)[[:space:]]+=>" "\\1:"))

(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))

(setq split-height-threshold 1200)
(setq split-width-threshold 2000)

(blink-cursor-mode 0)

(provide 'miscellaneous)
