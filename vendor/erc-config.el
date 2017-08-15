;; ERC
;; Load ERC
(require 'erc)

;; Load authentication info from an external source.  Put sensitive
;; passwords and the like in here, like `(setq erc-password "my-password")`
(load "~/.emacs.d/.erc-auth")

;; This causes ERC to connect to the Freenode network upon hitting
;; C-c C-i.  Replace MYNICK with your IRC nick.
(defun join-erc! ()
  (interactive)
   (erc-tls :server "irc.tfbnw.net" :port 6443
            :nick "dww" :full-name "Daniel Woelfel" :password erc-password))

(global-set-key "\C-c\C-i" 'join-erc!)

;; (add-to-list 'erc-keywords '("\\bso\\b" erc-default-face))

(setq erc-log-channels-directory "~/erc/logs/")
(setq erc-save-buffer-on-part t)
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; Make erc more pleasurable
(defun open-last-link ()
  "Searches for the last link in a buffer and opens it in a browser"
  (interactive)
  (save-excursion
    (re-search-backward "http[s]?://")
    (browse-url-at-point)))

(global-set-key "\C-c\C-l" 'open-last-link)

(setq erc-autojoin-channels-alist '(("freenode.net" "#emacs"         "#chicken"
                                     "#reactjs-dev" "#reactjs"       "#webgl"
                                     "##opengl"     "#haskell"       "#haskell-beginners"
                                     "#ocaml"       "#mirage"        "#clojure"
                                     "#quilt"       "#immutant"      "#datomic"
                                     "#flowtype"    "#clojurescript" "#reasonml")
                                    ("tfbnw.net" "#e" "#ops")))

;; Universal desktop notification method, hardcoded for osx.  Requires
;; `terminal-notifier` to be installed in a way emacs can reach
;; it (https://github.com/julienXX/terminal-notifier):
;; brew install terminal-notifier
(defun notify (title subtitle message)
  (start-process "notification" nil "terminal-notifier"
                 "-title" title
                 "-message" message
                 "-subtitle" subtitle
                 "-activate" "org.gnu.Emacs"
                 ;;"-sender" "org.gnu.Emacs"
                 ))

;; (notify "title" "subtitle" "body?" "erc")

;; Prevent erc from clobbering work buffers when connecting and
;; joining a bunch of rooms
(setq erc-join-buffer 'bury)

;; Save to erc logs on activity
(setq erc-save-buffer-on-part nil
      erc-save-queries-on-quit nil
      erc-log-write-after-send t
      erc-log-write-after-insert t)

;; erc notifications
(defvar my-erc-page-message "erc: %s"
  "Format of message to display in dialog box")

(defvar my-erc-page-nick-alist nil
  "Alist of nicks and the last time they tried to trigger a notification")

(defvar my-erc-page-timeout 1
  "Number of seconds that must elapse between notifications from the same person.")

(defvar erc-keywords '("dww" "dwwoelfel"))

(defun my-erc-page-popup-notification (nick msg)
  "Function used to notify user of an erc page"
  ;; must set default directory, otherwise start-process is unhappy
  ;; when this is something remote or nonexistent
  (let ((default-directory "~/"))
    (notify (format my-erc-page-message nick) msg "IRC")))

(defun my-erc-page-allowed (nick &optional delay)
  "Return non-nil if a notification should be made for NICK.
If DELAY is specified, it will be the minimum time in seconds
that can occur between two notifications.  The default is
`my-erc-page-timeout'."
  (unless delay (setq delay my-erc-page-timeout))
  (let ((cur-time (time-to-seconds (current-time)))
        (cur-assoc (assoc nick my-erc-page-nick-alist))
        (last-time))
    (if cur-assoc
        (progn
          (setq last-time (cdr cur-assoc))
          (setcdr cur-assoc cur-time)
          (> (abs (- cur-time last-time)) delay))
      (push (cons nick cur-time) my-erc-page-nick-alist)
      t)))

(defun my-erc-page-me (match-type nick message)
  "Notify the current user when someone sends a message that
matches a regexp in `erc-keywords'."
  (interactive)
  (when (and (eq match-type 'keyword)
             ;; I don't want to see anything from the erc server
             (null (string-match "\\`\\([sS]erver\\|localhost\\)" nick))
             ;; or bots
             (null (string-match "\\(bot\\|serv\\)!" nick))
             ;; or from those who abuse the system
             (my-erc-page-allowed nick))
    (my-erc-page-popup-notification nick message)))
(add-hook 'erc-text-matched-hook 'my-erc-page-me)


(defun my-erc-page-me-PRIVMSG (proc parsed)
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
        (target (car (erc-response.command-args parsed)))
        (msg (erc-response.contents parsed)))
    (message "%s %s %s %s" (erc-current-nick) (erc-current-nick-p msg) (not (erc-is-message-ctcp-and-not-action-p msg)) (my-erc-page-allowed nick))
    (when (and ;;(erc-current-nick-p target)
               (not (erc-is-message-ctcp-and-not-action-p msg))
               (my-erc-page-allowed nick))
      (my-erc-page-popup-notification nick msg)
      nil)))

(add-hook 'erc-server-PRIVMSG-functions 'my-erc-page-me-PRIVMSG)

(provide 'erc-config)
