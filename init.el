(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(css-indent-offset 2)
 '(cua-mode t nil (cua-base))
 '(font-use-system-font t)
 '(icicle-download-dir "/home/daniel/.emacs.d/site-lisp/icicles")
 '(ido-enable-flex-matching t)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(less-css-lessc-options (quote ("-x")))
 '(org-agenda-files (quote ("/home/daniel/Dropbox/cakehealth/cake_health.org")))
 '(org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m org-drill)))
 '(safe-local-variable-values (quote ((less-css-output-directory . "../css") (less-css-compile-at-save . t) (ruby-compilation-executable . "ruby") (ruby-compilation-executable . "ruby1.8") (ruby-compilation-executable . "ruby1.9") (ruby-compilation-executable . "rbx") (ruby-compilation-executable . "jruby"))))
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(temporary-file-directory "/home/daniel/.saves")
 '(tool-bar-mode nil)
 '(use-file-dialog nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "microsoft" :family "Consolas"))))
 '(flymake-errline ((((class color)) (:background "pink"))))
 '(flymake-warnline ((((class color)) (:background "#EEEE00")))))

;; load path
(let ((default-directory "/home/daniel/.emacs.d/site-lisp"))
  (setq load-path
	(append
	 (let ((load-path (copy-sequence load-path))) ;; Shadow
	   (append
	    (copy-sequence (normal-top-level-add-to-load-path '(".")))
	    (normal-top-level-add-subdirs-to-load-path)))
	 load-path)))

;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)

(setq ispell-program-name "hunspell")

(setq-default display-buffer-reuse-frames nil)
;;(require 'org-drill) wants org-learn

(winner-mode 1)

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "xo" 'other-window)
(key-chord-define-global "xb" 'ido-switch-buffer)


(require 'ace-jump-mode)
(define-key global-map (kbd "C-0") 'ace-jump-mode)

;; Sets ace-jump-mode to use char-mode first
;; Get to word mode with C-u
(setq ace-jump-mode-submode-list
  '(ace-jump-char-mode
    ace-jump-word-mode
    ace-jump-line-mode))

;; Switch between Term-mode and Shell-mode
(require 'shell)
(require 'term)

;; note that this doesn't work
(defun term-switch-to-shell-mode ()
  (interactive)
  (if (equal major-mode 'term-mode)
      (progn
        (shell-mode)
        (set-process-filter  (get-buffer-process (current-buffer)) 'comint-output-filter )
        (local-set-key (kbd "C-j") 'term-switch-to-shell-mode)
        (compilation-shell-minor-mode 1)
        (comint-send-input)
				)
    (progn
			(compilation-shell-minor-mode -1)
			(font-lock-mode -1)
			(set-process-filter  (get-buffer-process (current-buffer)) 'term-emulate-terminal)
			(term-mode)
			(term-char-mode)
			(term-send-raw-string (kbd "C-l"))
			)))
(define-key term-raw-map (kbd "C-j") 'term-switch-to-shell-mode)

(require 'shell-completion)

(add-to-list 'hippie-expand-try-functions-list
						 'yas/hippie-try-expand)

;;whitespace
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

;; Emacs 24
(setq completion-cycle-threshold 5)
(add-to-list 'completion-styles 'substring)

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (package-initialize)

;; (require 'durendal)

;; (add-hook 'clojure-mode-hook 'durendal-enable-auto-compile)
;; (add-hook 'slime-repl-mode-hook 'durendal-slime-repl-paredit)
;; (add-hook 'sldb-mode-hook 'durendal-dim-sldb-font-lock)
;; (add-hook 'slime-compilation-finished-hook 'durendal-hide-successful-compile)

(add-to-list 'auto-mode-alist '("\\.cljs" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljx" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.dieter" . clojure-mode))

(setq nrepl-popup-stacktraces nil)

;; no more yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

(autoload 'linum-mode "linum" "toggle line numbers on/off" t)
;(autoload 'highlight-parentheses-mode "hpm" "highlight parentheses" t)
(add-hook 'find-file-hook
					'highlight-parentheses-mode)

(global-set-key (kbd "C-<right>") 'linum-mode)
(global-set-key (kbd "C-<left>") 'linum-mode)

(require 'smooth-scrolling)

(setq mouse-wheel-scroll-amount '(5 ((shift) . 5))) ;; five lines at a time

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-step 1) ;; keyboard scroll one line at a time

(setq smooth-scroll-margin 2)

;; open links in google chrome
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; flyspell

(dolist (hook '(text-mode-hook))
	(add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
	(add-hook hook (lambda () (flyspell-mode -1))))


;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

;(require 'icicles)


;(require 'expand-region)
;(require 'python-el-expansions)
;(require 'text-mode-expansions)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'magit)
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")
     (when (not window-system)
       (set-face-background 'magit-item-highlight "black"))))

(load "/usr/home/daniel/.emacs.d/site-lisp/haskell/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Rails stuff
;;(add-to-list 'grep-files-aliases (cons "rails" "*.rb *.haml *.erb *.sass *.js *.coffee")) ;; get symbol definition void on grep-files-aliases


;; Highlight colors

(require 'cl)
(defun hexcolour-luminance (color)
	"Calculate the luminance of a color string (e.g. \"#ffaa00\", \"blue\").
  This is 0.3 red + 0.59 green + 0.11 blue and always between 0 and 255."
	(let* ((values (x-color-values color))
				 (r (car values))
				 (g (cadr values))
				 (b (caddr values)))
		(floor (+ (* .3 r) (* .59 g) (* .11 b)) 256)))

(defun hexcolour-add-to-font-lock ()
	(interactive)
	(font-lock-add-keywords nil
		 `((,(concat "#[0-9a-fA-F]\\{3\\}[0-9a-fA-F]\\{3\\}?\\|"
								 (regexp-opt (x-defined-colors) 'words))
				(0 (let ((colour (match-string-no-properties 0)))
						 (put-text-property
							(match-beginning 0) (match-end 0)
							'face `((:foreground ,(if (> 128.0 
																					 (hexcolour-luminance colour))
																				"white" "black"))
											(:background ,colour)))))))))

(add-hook 'less-css-mode 'hexcoulour-add-to-font-lock)
(add-hook 'css-mode 'hexcoulour-add-to-font-lock)
(add-hook 'sass-mode 'hexcoulour-add-to-font-lock)

(setq scss-compile-at-save nil)

(require 'yari)
(setq rinari-tags-file-name "TAGS")

(require 'ido)
(ido-mode t)

;; kill flymake until I can get it to work with tramp
;(require 'flymake)

(setq flymake-run-in-place nil)

;(require 'rvm)
;(rvm-use-default)
;(require 'flymake-ruby)
;(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(add-to-list 'auto-mode-alist '("\\.rabl$" . ruby-mode))

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

(defun my-flymake-show-help ()
	(interactive)
	(when (get-char-property (point) 'flymake-overlay)
		(let ((help (get-char-property (point) 'help-echo)))
			(if help (message "%s" help)))))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-hook 'yaml-mode-hook
					'(lambda ()
						 (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; Lisp stuff

;(setq inferior-lisp-program "/usr/local/bin/sbcl") ; your Lisp system
;(add-to-list 'load-path "/home/daniel/.emacs.d/site-lisp/slime/")  ; your SLIME directory
;(require 'slime)
;(slime-setup)

;; Latex Stuff
;; commenting out until I can figure out why it's missing
;;(load "auctex.el" nil t t)
;;(load "preview-latex.el" nil t t)


(show-paren-mode 1)
(tool-bar-mode -1)
(setq frame-title-format "%b")
(setq default-tab-width 2)
;;(setq tab-stop-list '(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30))

;; better backup and autosave handling

;; (setq
;;    backup-by-copying t      ; don't clobber symlinks
;;    delete-old-versions t
;;    kept-new-versions 6
;;    kept-old-versions 2
;;    version-control t)       ; use versioned backups


(setq backup-directory-alist
			`((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
			'(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/tmp/\\2" t)
				("\\`/?\\([^/]*/\\)*\\([^/]*\\)\\'" "/home/daniel/.saves/\\2" t)))

;; org-mode
(setq org-cycle-include-plain-lists nil)

;;; fontification for code blocks
(setq org-src-fontify-natively t)

;;; custom keybindings and modes for org-mode
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key "\M-[" 'org-metaleft)
            (local-set-key "\M-]" 'org-metaright)
						(local-set-key (kbd "C-S-<return>") 'org-insert-heading-after-current)
						(org-indent-mode)
						(visual-line-mode)))

;; Shell stuff

(defun clear-shell ()
	(interactive)
	(let ((comint-buffer-maximum-size 0))
		(comint-truncate-buffer)))

;; viewing logs

(defun colorize-ansi ()
	(interactive)
	(ansi-color-apply-on-region (point-min) (point-max))
)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; tramp
(setq tramp-default-method "ssh")
;;; don't save local copies of backup files from remote server
;;(add-to-list 'backup-directory-alist
;;                  (cons tramp-file-name-regexp nil))
;;; multi-hop for cakehealth
;(add-to-list 'tramp-default-proxies-alist
;						 '("\\10\\.0\\.0\\.50" nil "/ssh:daniel@50.18.196.122:"))
;; TODO: previous lines throw an error "tramp-default-proxies-alist .... defn void
;; try getting the version from cvs

;; some keyboard shortcuts
(global-set-key (kbd "C-<f10>") 'revert-buffer)
(global-set-key (kbd "C-<f9>") 'magit-status)
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

(eval-after-load 'shell
	'(define-key shell-mode-map (kbd "C-c C-q") 'comint-delete-output))

(eval-after-load 'shell
	'(define-key shell-mode-map (kbd "C-c C-q") 'clear-shell))

;; this would be better if I could get the point whenever the buffer
;; reverted. Then I could pass that point into colorize-ansi for an automatic
;; colorizer. Would also save cpu cycles.
(global-set-key (kbd "C-c C-a") 'colorize-ansi)

(setq auto-mode-alist (cons '("\\.log" . auto-revert-tail-mode) auto-mode-alist))

(add-hook 'auto-revert-tail-mode-hook 'ansi-color-for-comint-mode-on)

(defun fc-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c C-c C-e") 'fc-eval-and-replace)

(defun sprite-top (start end)
	(interactive "r")
	(setq current_num (format "%s" (buffer-substring-no-properties start (line-end-position))))
	(setq current_num (string-to-number current_num))
	(setq new_num (- 2352 current_num))
	(delete-region start (line-end-position))
	(insert (format "-%spx" new_num)))

(defun outdent (limit)
  (while (search-forward "\n\t" limit t)
    (replace-match "\n" nil t)))

(defun unindent ()
  (interactive)
  (save-excursion (outdent nil)))

(defun unindent-region ()
  (interactive)
  (save-excursion
    (and (> (point) (mark)) (exchange-point-and-mark))
    (outdent (mark))))

;; Javascript
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-pretty-multiline-decl-indentation-p t)
(setq js2-bounce-indent-p nil)

(add-hook 'after-init-hook
					#'(lambda ()
							(when (locate-library "slime-js")
								(require 'setup-slime-js))))

(global-set-key [f5] 'slime-js-reload)
(add-hook 'js2-mode-hook
          (lambda ()
            (slime-js-minor-mode 1)))

(require 'coffee-mode)

(defun turn-on-tabs ()
	(setq indent-tabs-mode t))

(add-hook 'coffee-mode-hook
          (lambda ()
						(turn-on-tabs)
            (local-set-key (kbd "C-c C-d") 'slime-js-coffee-eval-current)
            (local-set-key (kbd "C-c C-b") 'slime-js-coffee-eval-buffer)
            (slime-js-minor-mode 1)))

;; ;; Close the compilation window if there was no error at all.
;; (setq compilation-exit-message-function
;; 			(lambda (status code msg)
;; 				;; If M-x compile exists with a 0
;; 				(when (and (eq status 'exit) (zerop code))
;; 					;; then bury the *compilation* buffer, so that C-x b doesn't go there
;; 					(bury-buffer "*compilation*")
;; 					;; and return to whatever were looking at before
;; 					(replace-buffer-in-windows "*compilation*"))
;; 				;; Always return the anticipated result of compilation-exit-message-function
;; 				(cons msg code)))

  ;; (setq compilation-finish-functions 'compile-autoclose)
  ;; (defun compile-autoclose (buffer string)
  ;;    (cond ((string-match "finished" string)
	;;   (bury-buffer "*compilation*")
  ;;         (winner-undo)
  ;;         (message "Build successful."))
  ;;        (t                                                                    
  ;;         (message "Compilation exited abnormally: %s" string))))

;; Need to find a better way to hide the compilation buffer, this
;; hides buffers I want to see, like grep

(setq cua-auto-tabify-rectangles nil)
(defadvice align (around smart-tabs activate)
	(let ((indent-tabs-mode nil)) ad-do-it))
(defadvice align-regexp (around smart-tabs activate)
	(let ((indent-tabs-mode nil)) ad-do-it))
(defadvice indent-relative (around smart-tabs activate)
	(let ((indent-tabs-mode nil)) ad-do-it))
(defadvice indent-according-to-mode (around smart-tabs activate)
	(let ((indent-tabs-mode indent-tabs-mode))
		(if (memq indent-line-function
							'(indent-relative
								indent-relative-maybe))
				(setq indent-tabs-mode nil))
		ad-do-it))
(defmacro smart-tabs-advice (function offset)
	`(progn
		 (defvaralias ',offset 'tab-width)
		 (defadvice ,function (around smart-tabs activate)
			 (cond
				(indent-tabs-mode
				 (save-excursion
					 (beginning-of-line)
					 (while (looking-at "\t*\\( +\\)\t+")
						 (replace-match "" nil nil nil 1)))
				 (setq tab-width tab-width)
				 (let ((tab-width fill-column)
							 (,offset fill-column)
							 (wstart (window-start)))
					 (unwind-protect
							 (progn ad-do-it)
						 (set-window-start (selected-window) wstart))))
				(t
				 ad-do-it)))))

(smart-tabs-advice js2-indent-line js2-basic-offset)
(setq ruby-indent-tabs-mode nil)
(setq ruby-indent-level 2)

(require 'highlight-parentheses)

(defun goto-match-paren (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "p")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc.
  (cond ((looking-at "[\[\(\{]") (forward-sexp))
        ((looking-back "[\]\)\}]" 1) (backward-sexp))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
        ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
        (t nil)))

;; converting between variable types

(defun split-name (s)
	(split-string
	 (let ((case-fold-search nil))
		 (downcase
			(replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
	 "[^A-Za-z0-9]+"))
(defun camelcase  (s) (mapconcat 'capitalize (split-name s) ""))
(defun underscore (s) (mapconcat 'downcase   (split-name s) "_"))
(defun colonize   (s) (mapconcat 'capitalize (split-name s) "::"))
(defun dasherize  (s) (mapconcat 'downcase   (split-name s) "-"))

(defun camelscore (s)
	(cond ((string-match-p "\:"  s)	(camelcase s))
				((string-match-p "-" s) (colonize s))
				((string-match-p "_" s)	(dasherize s))
				(t (underscore s)) ))

(defun camelscore-word-at-point ()
	(interactive)
	(let* ((case-fold-search nil)
				 (beg (and (skip-chars-backward "[:alnum:]:_-") (point)))
				 (end (and (skip-chars-forward  "[:alnum:]:_-") (point)))
				 (txt (buffer-substring beg end))
				 (cml (camelscore txt)) )
		(if cml (progn (delete-region beg end) (insert cml))) ))



; (Transient-mark-mode 1)  ; Now on by default: makes the region act quite like the text "highlight" in many apps.
   ; (setq shift-select-mode t) ; Now on by default: allows shifted cursor-keys to control the region.
   (setq mouse-drag-copy-region nil)  ; stops selection with a mouse being immediately injected to the kill ring
   (setq x-select-enable-primary nil)  ; stops killing/yanking interacting with primary X11 selection
   (setq x-select-enable-clipboard t)  ; makes killing/yanking interact with clipboard X11 selection
   ;; these will probably be already set to these values, leave them that way if so!
   ; (setf interprogram-cut-function 'x-select-text)
   ; (setf interprogram-paste-function 'x-cut-buffer-or-selection-value)
   ; You need an emacs with bug #902 fixed for this to work properly. It has now been fixed in CVS HEAD.
   ; it makes "highlight/middlebutton" style (X11 primary selection based) copy-paste work as expected
   ; if you're used to other modern apps (that is to say, the mere act of highlighting doesn't
   ; overwrite the clipboard or alter the kill ring, but you can paste in merely highlighted
   ; text with the mouse if you want to)
   (setq select-active-regions t) ;  active region sets primary X11 selection
   (global-set-key [mouse-2] 'mouse-yank-primary)  ; make mouse middle-click only paste from primary X11 selection, not clipboard and kill ring.
   ;; with this, doing an M-y will also affect the X11 clipboard, making emacs act as a sort of clipboard history, at
   ;; least of text you've pasted into it in the first place.
   ; (setq yank-pop-change-selection t)  ; makes rotating the kill ring change the X11 clipboard.


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
;(when
;		(load
;		 (expand-file-name "/home/daniel/.emacs.d/elpa/package.el"))
;	(package-initialize))

;; wrapping

(defun insert-quotes (&optional arg)
  (interactive "P")
  (insert-pair arg ?\" ?\"))

(global-set-key (kbd "M-\"") 'insert-quotes)

;; Python stuff

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")

;; Clojure repl stuff

(defun clojurescript-repl ()
 (interactive)
 (run-lisp "lein trampoline cljsbuild repl-listen"))

(setq inferior-lisp-program "browser-repl")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Allow input to be sent to somewhere other than inferior-lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is a total hack: we're hardcoding the name of the shell buffer
(defun shell-send-input (input)
  "Send INPUT into the *shell* buffer and leave it visible."
  (save-selected-window
    (switch-to-buffer-other-window "*shell*")
    (goto-char (point-max))
    (insert input)
    (comint-send-input)))

(defun defun-at-point ()
  "Return the text of the defun at point."
  (apply #'buffer-substring-no-properties
         (region-for-defun-at-point)))

(defun region-for-defun-at-point ()
  "Return the start and end position of defun at point."
  (save-excursion
    (save-match-data
      (end-of-defun)
      (let ((end (point)))
        (beginning-of-defun)
        (list (point) end)))))

(defun expression-preceding-point ()
  "Return the expression preceding point as a string."
  (buffer-substring-no-properties
   (save-excursion (backward-sexp) (point))
   (point)))

(defun shell-eval-last-expression ()
  "Send the expression preceding point to the *shell* buffer."
  (interactive)
  (shell-send-input (expression-preceding-point)))

(defun shell-eval-defun ()
  "Send the current toplevel expression to the *shell* buffer."
  (interactive)
  (shell-send-input (defun-at-point)))

(add-hook 'clojure-mode-hook
          '(lambda ()
             (define-key clojure-mode-map (kbd "C-c e") 'shell-eval-last-expression)
             (define-key clojure-mode-map (kbd "C-c x") 'shell-eval-defun)))


(add-hook 'slime-event-hooks 
					'(lambda (event)
						 (let ((command (cadr event)))
							 (if (stringp command) 
									 (message command))
							 (if (and (stringp command) (string-match "Remote attached*"
																												command))
									 (slime-js-eval "alert('swank-js is middleware!')")))))

(require 'sudo)

(defun sudo-before-save-hook ()
  (set (make-local-variable 'sudo:file) (buffer-file-name))
  (when sudo:file
    (unless(file-writable-p sudo:file)
      (set (make-local-variable 'sudo:old-owner-uid) (nth 2 (file-attributes sudo:file)))
      (when (numberp sudo:old-owner-uid)
	(unless (= (user-uid) sudo:old-owner-uid)
	  (when (y-or-n-p
		 (format "File %s is owned by %s, save it with sudo? "
			 (file-name-nondirectory sudo:file)
			 (user-login-name sudo:old-owner-uid)))
	    (sudo-chown-file (int-to-string (user-uid)) (sudo-quoting sudo:file))
	    (add-hook 'after-save-hook
		      (lambda ()
			(sudo-chown-file (int-to-string sudo:old-owner-uid)
					 (sudo-quoting sudo:file))
			(if sudo-clear-password-always
			    (sudo-kill-password-timeout)))
		      nil   ;; not append
		      t	    ;; buffer local hook
		      )))))))


(add-hook 'before-save-hook 'sudo-before-save-hook)
