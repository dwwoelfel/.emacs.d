(setq org-cycle-include-plain-lists nil)

;;; fontification for code blocks
(setq org-src-fontify-natively t)

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key "\M-[" 'org-metaleft)
            (local-set-key "\M-]" 'org-metaright)
            (local-set-key (kbd "C-S-<return>") 'org-insert-heading-after-current)
            (org-indent-mode)
            (visual-line-mode)
            (require 'ethan-wspace)
            (setq ethan-wspace-errors '())))

(provide 'setup-org-mode)
