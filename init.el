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
  `(eval-after-load ',package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(defun git-toplevel ()
  (replace-regexp-in-string "\n$" ""
                            (shell-command-to-string "git rev-parse --show-toplevel")))

(defun magit-after ()
  (add-hook 'magit-mode-hook
    '(lambda ()
       (set-face-foreground 'magit-diff-added "black")
       (set-face-foreground 'magit-diff-added-highlight "black")
       (set-face-background 'magit-diff-added  "#ddffdd")
       (set-face-background 'magit-diff-added-highlight "#ddffdd")
       (set-face-background 'magit-diff-removed "#ffdddd")
       (set-face-background 'magit-diff-removed-highlight "#ffdddd")
       (set-face-foreground 'magit-diff-removed "black")
       (set-face-foreground 'magit-diff-removed-highlight "black")
       (set-face-foreground 'magit-diff-context-highlight "black")
       (set-face-background 'magit-diff-context-highlight "gray93")
       (set-face-foreground 'magit-diff-context "black")
       (set-face-foreground 'magit-section-highlight "black")
       (set-face-background 'magit-section-highlight "gray97")

       (defadvice magit-status (around magit-fullscreen activate)
         (window-configuration-to-register :magit-fullscreen)
         ad-do-it
         (delete-other-windows))

       (define-key magit-status-mode-map (kbd "q")
         '(lambda ()
            (interactive)
            (kill-buffer)
            (jump-to-register :magit-fullscreen)))

       (define-key magit-status-mode-map (kbd "C-c k")
         '(lambda ()
            (interactive)
            (kill-buffer "*magit-process*")))))
  (global-set-key (kbd "C-c m c") 'magit-checkout)
  (global-set-key (kbd "C-c m f") 'magit-fetch)
  (global-set-key (kbd "C-c m u") 'magit-submodule-update)
  (global-set-key (kbd "C-c m m") 'magit-merge)
  (global-set-key (kbd "C-c m B") 'magit-create-branch)
  (global-set-key (kbd "C-c m g") '(lambda ()
                                     (interactive)
                                     (require 'grep)
                                     (require 'vc-git)
                                     (vc-git-grep (grep-read-regexp)
                                                  ""
                                                  (git-toplevel)))))

(defun cider-after ()
  (setq cider-known-endpoints '(("pc-dev" "localhost" "6005")))
  (setq cider-popup-stacktraces t)
  (setq cider-repl-popup-stacktraces t)
  (setq cider-popup-on-error nil)
  (setq cider-show-error-buffer nil)
  (setq cider-auto-select-error-buffer nil)

  (setq nrepl-buffer-name-separator "-")
  (setq nrepl-buffer-name-show-port t)

  (setq cider-repl-print-length 10000)
  (setq cider-repl-history-size 500000)
  (setq cider-repl-history-file "~/.nrepl-history.eld")
  (add-hook 'cider-repl-mode-hook 'subword-mode)
  (add-hook 'cider-repl-mode-hook '(lambda ()
                                     (define-key cider-repl-mode-map (kbd "M-R") 'cider-repl-previous-matching-input)))
  (defadvice nrepl-send-request (before nrepl-send-request-whoami activate)
    (setq request (append request
                          (list "whoami" (cider-user-first-name))))))

(defun clojure-after ()
  (rename-modeline clojure-mode clojure-mode "Clj")
  (add-to-list 'auto-mode-alist '("\\.cljs" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljx" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.dieter" . clojure-mode))
  (define-key clojure-mode-map (kbd "<tab>") 'cider-repl-indent-and-complete-symbol)
  (add-hook 'clojure-mode-hook
            '(lambda ()
               (define-clojure-indent
                 (this-as 1)
                 (div 'defun)
                 (g 'defun)
                 (table 'defun)
                 (th 'defun)
                 (tr 'defun)
                 (td 'defun)
                 (thead 'defun)
                 (tbody 'defun)
                 (figure 'defun)
                 (foreignObject 'defun)
                 (tspan 'defun)
                 (set-state-nr! 2))
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
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (require 'hippie-exp)
  (defadvice he-substitute-string (after he-paredit-fix)
    "remove extra paren when expanding line in paredit"
    (if (and paredit-mode (equal (substring str -1) ")"))
        (progn (backward-delete-char 1) (forward-char)))))

(defun swank-js-after ()
  (require 'slime)
  (require 'slime-js)
  (global-set-key [f5] 'slime-js-reload)
  (add-hook 'js2-mode-hook
            '(lambda ()
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

(defun git-ls-file-in-project-after ()
  (global-set-key (kbd "C-x f") 'find-file-in-project)
  (setq ffip-patterns '("html" "org" "txt" "md" "el" "clj" "hamlc" "less" "coffee"
                        "py" "rb" "js" "pl" "sh" "erl" "hs" "ml" "ref"))

  (setq ffip-project-root-function 'git-toplevel)
  (setq ffip-limit 100000))

(defun clj-refactor-after ()
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)
                                 (cljr-add-keybindings-with-prefix "C-c C-j"))))

(defun ethan-wspace-after ()
  (setq mode-require-final-newline nil)
  (global-ethan-wspace-mode 1))

(defun ac-cider-after ()
  (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
  (add-hook 'cider-mode-hook 'ac-cider-setup)
  (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
  (eval-after-load "auto-complete"
    '(progn
       (add-to-list 'ac-modes 'cider-mode)
       (add-to-list 'ac-modes 'cider-repl-mode))))

(defun multiple-cursors-after ()
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(setq el-get-sources
      `(,(vendor-source mac-bs)
        ,(vendor-source miscellaneous)
        (:name magit
               :after (magit-after))
        (:name clojure-mode
               :after (clojure-after))
        (:name cider-stable
               :after (cider-after))
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
               :after (ethan-wspace-after))
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
        (:name git-ls-file-in-project
               :after (git-ls-file-in-project-after))
        (:name sass-mode
               :after (add-to-list 'auto-mode-alist '("\\.less" . sass-mode)))
        (:name clj-refactor
               :after (clj-refactor-after))
        (:name ac-cider
               :after (ac-cider-after))
        (:name multiple-cursors
               :after (multiple-cursors-after))
        ,(vendor-source sudo)
        ;; Not using this right now
        ;; ,(vendor-source setup-rcirc)
        ,(vendor-source setup-org-mode)
        ,(vendor-source diminish)))

(setq my-packages
      (append
       '(el-get less-css-mode slime ethan-wspace geiser nginx-mode js2-mode json-reformat
                ;; needed by clj-refactor
                s yasnippet)
       (mapcar 'el-get-source-name el-get-sources)
       '(cider-decompile)))

(el-get 'sync my-packages)

(put 'erase-buffer 'disabled nil)

;; These aren't executing for some reason :(
(require 'clojure-mode)
(clojure-after)
(require 'clj-refactor)
(clj-refactor-after)
