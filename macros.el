(fset 'unkeywordize-get
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([19 58 13 2 103 101 116 32 34 67108905 4 4 19 34 13 11 18 40 13 134217830 32 8388726 134217826 backspace] 0 "%d")) arg)))

(global-set-key (kbd "<f3>") 'unkeywordize-get)
