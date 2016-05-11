(setq org-cycle-include-plain-lists nil)

;;; fontification for code blocks
(setq org-src-fontify-natively t)

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key "\M-[" 'org-do-promote)
            (local-set-key "\M-]" 'org-do-demote)
            (local-set-key (kbd "M-S-[") 'org-promote-subtree)
            (local-set-key (kbd "M-S-]") 'org-demote-subtree)
            (local-set-key (kbd "C-S-<return>") 'org-insert-heading-after-current)
            (local-set-key (kbd "C-S-M-h") 'org-insert-heading-after-current)
            (local-set-key (kbd "C-<return>") 'org-insert-heading-respect-content)
            (local-set-key (kbd "C-c d") 'org-decrypt-entry)
            (define-key org-mode-map (kbd "C-<return>") 'org-insert-heading-respect-content)
            (org-indent-mode)
            (visual-line-mode)
            (require 'ethan-wspace)
            (setq ethan-wspace-errors '())))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-catch-invisible-edits 'smart)
(setq org-cycle-include-plain-lists t)
(setq org-log-done 'time)
(setq org-todo-keywords
      '((sequence "TODO(!)" "WAIT(!)" "|" "DONE(!)" "CANCELED(!)")))
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files (list org-directory))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-directory "/notes.org") "Tasks")
         "* TODO %?\n  %i\n  %a")

        ("j" "Journal Entry"
         entry (file (concat org-directory "/journal.org"))
         "* %T Heading :crypt: \n%?"
         :prepend
         :empty-lines 1)))

(setq org-return-follows-link t)

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key "D27912B7")
(setq org-crypt-disable-auto-save t)

(provide 'setup-org-mode)

(format-time-string "%a, %b %d '%y %I:%M%p")
