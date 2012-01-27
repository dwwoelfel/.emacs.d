(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(ido-enable-flex-matching t)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(temporary-file-directory "~/.saves")
 '(tool-bar-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "microsoft" :family "Consolas")))))

;; load path
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (setq load-path
	(append
	 (let ((load-path (copy-sequence load-path))) ;; Shadow
	   (append 
	    (copy-sequence (normal-top-level-add-to-load-path '(".")))
	    (normal-top-level-add-subdirs-to-load-path)))
	 load-path)))

(autoload 'linum-mode "linum" "toggle line numbers on/off" t) 
(global-set-key (kbd "C-<right>") 'linum-mode)    
(global-set-key (kbd "C-<left>") 'linum-mode)    

(require 'smooth-scrolling)

(setq mouse-wheel-scroll-amount '(5 ((shift) . 5))) ;; five lines at a time

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
    
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
    
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; open links in google chrome
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(require 'magit)

;; Rails stuff

(require 'ido)
(ido-mode t)

(require 'haml-mode)
(add-hook 'haml-mode-hook
	  '(lambda ()
	     (setq indent-tabs-mode t)
	     (define-key haml-mode-map "\C-m" 'newline-and-indent)))

(require 'sass-mode)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-hook 'yaml-mode-hook
					'(lambda ()
						 (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; Lisp stuff

(setq inferior-lisp-program "/usr/local/bin/sbcl") ; your Lisp system
(add-to-list 'load-path "~/.emacs.d/site-lisp/slime/")  ; your SLIME directory
(require 'slime)
(slime-setup)

;; Latex Stuff

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)


(show-paren-mode 1)
(tool-bar-mode -1)
(setq frame-title-format "%b")
(setq default-tab-width 2)
;;(setq tab-stop-list '(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30))

;; better backup and autosave handling

(setq
   backup-by-copying t      ; don't clobber symlinks
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups


(setq backup-directory-alist
			`((".*" . ,temporary-file-directory)))
;;(setq auto-save-file-name-transforms
;;			`((".*" ,temporary-file-directory t)))

(setq auto-save-file-name-transforms
			'(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/tmp/\\2" t)
				("\\`/?\\([^/]*/\\)*\\([^/]*\\)\\'" "~/.saves/\\2" t)))

(setq org-cycle-include-plain-lists nil)



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
(global-set-key "\M- " 'hippie-expand)
(global-set-key (kbd "C-|") 'align)
(global-set-key (kbd "C-M-|") 'align-regexp)
(global-set-key (kbd "C-c C-k") 'camelscore-word-at-point)

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

(global-set-key (kbd "C-c C-e") 'fc-eval-and-replace)

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

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-pretty-multiline-decl-indentation-p t)
(setq js2-bounce-indent-p nil)

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
(when
		(load
		 (expand-file-name "~/.emacs.d/elpa/package.el"))
	(package-initialize))
