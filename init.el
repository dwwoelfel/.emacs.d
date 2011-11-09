(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(cua-mode t nil (cua-base))
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(menu-bar-mode t)
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(let ((default-directory "~/.emacs.d/site-lisp/"))
  (setq load-path
	(append
	 (let ((load-path (copy-sequence load-path))) ;; Shadow
	   (append 
	    (copy-sequence (normal-top-level-add-to-load-path '(".")))
	    (normal-top-level-add-subdirs-to-load-path)))
	 load-path)))

(require 'haml-mode)
(add-hook 'haml-mode-hook
	  '(lambda ()
	     (setq indent-tabs-mode t)
	     (define-key haml-mode-map "\C-m" 'newline-and-indent)))

(require 'sass-mode)

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(show-paren-mode 1)
(tool-bar-mode -1)
(setq frame-title-format "%b")
(setq default-tab-width 2)
;;(setq tab-stop-list '(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30))

;; better backup handling
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(setq org-cycle-include-plain-lists nil)

(defun clear-shell ()
	(interactive)
	(let ((comint-buffer-maximum-size 0))
		(comint-truncate-buffer)))

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

(global-set-key (kbd "<f6>") 'sprite-top)

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
(smart-tabs-advice ruby-indent-line ruby-indent-level)
(setq ruby-indent-tabs-mode t)

; (transient-mark-mode 1)  ; Now on by default: makes the region act quite like the text "highlight" in many apps.
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
