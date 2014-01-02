(setq debug-on-error t)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(defun vendor-url (filename)
  "Constructs a file url for filename in vendor dir"
  (concat "file://"
          (expand-file-name user-emacs-directory)
          "vendor/"
          filename))

(defmacro vendor-source (name)
  `(list :name ',name
         :type 'http
         :url ',(vendor-url (format "%s.el" name))
         :features ',name))

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(defun magit-after ()
  (eval-after-load 'magit-mode
    '(progn
       (set-face-foreground 'magit-diff-add "green3")
       (set-face-foreground 'magit-diff-del "red3")
       (when (not window-system)
         (set-face-background 'magit-item-highlight "black"))

      (defadvice magit-status (around magit-fullscreen activate)
        (window-configuration-to-register :magit-fullscreen)
        ad-do-it
        (delete-other-windows))
      (defun exit-magit ()
        (interactive)
        (kill-buffer)
        (jump-to-register :magit-fullscreen))
      (defun kill-magit-git-process ()
        (interactive)
        (kill-buffer "*magit-process*"))
      (define-key magit-status-mode-map (kbd "q") 'exit-magit)
      (define-key magit-status-mode-map (kbd "C-c k") 'kill-magit-git-process))))

(defun cider-after ()
  (setq cider-popup-stacktraces t)
  (setq cider-repl-popup-stacktraces t)

  ;; Do these both do the same thing?
  (setq cider-popup-on-error nil)
  (setq cider-auto-select-error-buffer nil)

  (setq nrepl-buffer-name-separator "-")
  (setq nrepl-buffer-name-show-port t)
  (setq cider-repl-print-length 1000)
  (setq cider-repl-history-size 500000)
  (setq cider-repl-history-file "~/.nrepl-history.eld")
  (add-hook 'cider-repl-mode-hook 'subword-mode)
  (eval-after-load 'cider-repl-mode
    ;; '(progn
    ;;   (defun fix-nrepl-indentation ()
    ;;     (setq-local lisp-indent-function 'clojure-indent-function))
    ;;   (add-hook 'nrepl-mode-hook 'fix-nrepl-indentation))
    '(define-key cider-repl-mode-map (kbd "M-R") 'cider-repl-previous-matching-input)))

(defun clojure-after ()
  (rename-modeline "clojure-mode" clojure-mode "Clj")
  (add-to-list 'auto-mode-alist '("\\.cljs" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljx" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.dieter" . clojure-mode))
  (define-key clojure-mode-map (kbd "TAB") 'cider-repl-indent-and-complete-symbol)
  (add-hook 'clojure-mode-hook
            '(progn
               (flyspell-prog-mode)
               (font-lock-add-keywords
                nil
                '(("(\\(defapi-\\w+\\)\\s-+\\(\\w+\\)"
                   (1 font-lock-keyword-face)
                   (2 font-lock-function-name-face)))))))

(defun paredit-after ()
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'scheme-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode))

(defun swank-js-after ()
  (require 'slime)
  (require 'slime-js)
  (global-set-key [f5] 'slime-js-reload)  
  (add-hook 'js2-mode-hook
            '(progn
               (slime-js-minor-mode 1))))

(defun haml-mode-after ()
  (add-to-list 'auto-mode-alist '("\\.hamlc$" . haml-mode))
  (add-hook 'haml-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-d") 'slime-js-haml-coffee-eval-current)
              (local-set-key (kbd "C-c C-k") 'slime-js-haml-coffee-eval-buffer)
              (slime-js-minor-mode 1))))

(defun coffee-mode-after ()
  (add-hook 'coffee-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-d") 'slime-js-coffee-eval-current)
              (local-set-key (kbd "C-c C-k") 'slime-js-coffee-eval-buffer)
              (slime-js-minor-mode 1))))

(defun keychord-after ()
  (key-chord-mode 1)
  (key-chord-define-global "xo" 'other-window)
  (key-chord-define-global "xb" 'ido-switch-buffer)
  (key-chord-define-global "fg" 'jump-char-forward))

(defun yaml-mode-after ()
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(defun markdown-mode-after ()
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(defun highlight-parentheses-after ()
  (add-hook 'find-file-hook
            'highlight-parentheses-mode))

(defun haskell-mode-after ()
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

(defun find-file-in-project-after ()
  (global-set-key (kbd "C-x f") 'find-file-in-project)
  (setq ffip-patterns '("html" "org" "txt" "md" "el" "clj" "hamlc" "less" "coffee"
                        "py" "rb" "js" "pl" "sh" "erl" "hs" "ml"))

  (setq ffip-project-root-function (lambda ()
                                     (replace-regexp-in-string "\n$" ""
                                                               (shell-command-to-string "git rev-parse --show-toplevel"))))
  (setq ffip-limit 100000))

(setq el-get-sources
      `(,(vendor-source mac-bs)
        ,(vendor-source miscellaneous)
        (:name magit
               :after (magit-after))
        (:name cider
               :after (cider-after))
        (:name clojure-mode
               :after (clojure-after))
        (:name paredit
               :after (paredit-after))
        (:name swank-js
               :after (swank-js-after))
        (:name haml-mode
               :after (haml-mode-after))
        (:name coffee-mode
               :after (coffee-mode-after))
        ,(vendor-source haml-coffee-mode)
        ,(vendor-source setup-slime-js)
        (:name ethan-wspace
               :after (global-ethan-wspace-mode 1))
        (:name key-chord
               :after (keychord-after))
        (:name yaml-mode
               :after (yaml-mode-after)
               :features yaml-mode)
        (:name markdown-mode
               :after (markdown-mode-after))
        (:name highlight-parentheses
               :after (highlight-parentheses-after))
        (:name haskell-mode
               :after (haskell-mode-after))
        (:name expand-region
               :after (global-set-key (kbd "C-=") 'er/expand-region))
        (:name find-file-in-project
               :after (find-file-in-project-after))
        ,(vendor-source sudo)
        ,(vendor-source setup-rcirc)
        ,(vendor-source setup-org-mode)
        ,(vendor-source diminish)))

(setq my-packages
      (append
       '(el-get haml-mode slime ethan-wspace sass-mode geiser nginx-mode)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)
