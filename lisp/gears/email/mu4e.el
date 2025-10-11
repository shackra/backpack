(require 'backpack-pouch)

(leaf mu4e-query
  :when (gearp! :email mu4e)
  :doc "use Emacs lisp to create valid mu queries"
  :ensure (mu4e-query :host github :repo "mickeynp/mu4e-query" :ref "dca407e1dd9c5d7a84a242b8bb8ceff27037a429")
  :require t
  :leaf-defer nil)

(leaf mu4e-contrib
  :unless (gearp! :email mu4e -fix-msg-background-color)
  :after mu4e
  :preface
  (defun shr-no-colourise-region (&rest ignore))
  :custom
  (mu4e-html2text-command . 'mu4e-shr2text)
  (shr-color-visible-luminance-min . 60)
  (shr-color-visible-distance-min . 5)
  :advice
  (:around shr-colorize-region shr-no-colourise-region))

(leaf mu4e
  :when (gearp! :email mu4e)
  :tag "email" "mu4e"
  :ensure (mu4e :host github :repo "djcb/mu" :ref "1a501281443eca6ccf7a7267a1c9c720bc6ccca1")
  :doctor
  ("msmtp" . "program that sends email")
  :bind
  ("C-c u"	.	mu4e)
  (:mu4e-main-mode-map
   ("x"		.	bury-buffer)
   ("I"		.	mu4e-update-index)
   ("<tab>"	.	shr-next-link)
   ("<backtab>" .	shr-previous-link))
  (:mu4e-headers-mode-map
   ("M"		.	mu4e-headers-mark-all)
   ("N"		.	mu4e-headers-mark-all-unread-read))
  :preface
  (defun backpack/mu4e-compose-goodies ()
    "Settings for mu4e compose mode."
    (set-fill-column 72)
    (turn-on-auto-fill)
    (electric-indent-local-mode -1)
    (when (gearp! :checkers spelling)
      (jinx-mode 1)))

  (defun backpack/mu4e~headers-human-date (msg)
    "Show date without year if message is from current year; show the year otherwise."
    (let* ((date (mu4e-msg-field msg :date))
           (tm   (decode-time date))
           (cur  (decode-time (current-time)))
           (day  (nth 3 tm))
           (mon  (nth 4 tm))
           (year (nth 5 tm))
           (cyr  (nth 5 cur)))
      (if (= year cyr)
          (format-time-string mu4e-headers-time-format date)
	(format-time-string mu4e-headers-date-format date))))
  :hook
  (mu4e-compose-mode-hook . backpack/mu4e-compose-goodies)
  :advice
  (:override mu4e~headers-human-date backpack/mu4e~headers-human-date)
  :custom
  (gnus-article-date-headers		.	'(combined-local-lapsed))
  (mu4e-maildir				.	"~/Maildir")
  (mu4e-update-interval			.	5)
  (mu4e-attachment-dir			.	`,(xdg-user-dir "DOWNLOAD"))
  (mu4e-change-filenames-when-moving	.	t)
  (mail-user-agent			.	'mu4e-user-agent)
  (message-citation-line-function	.	'message-insert-formatted-citation-line)
  (mu4e-compose-format-flowed		.	t)
  (mu4e-headers-auto-update		.	t)
  (mu4e-headers-date-format		.	"%d/%m/%Y %H:%M")
  (mu4e-index-cleanup			.	t)
  (mu4e-index-lazy-check		.	nil)
  (mu4e-main-hide-personal-addresses	.	t)
  (mu4e-main-buffer-name		.	"*mu4e-main*")
  (mu4e-use-fancy-chars			.	t)
  (sendmail-program			.	`,(executable-find "msmtp"))
  (message-sendmail-f-is-evil		.	t)
  (message-sendmail-extra-arguments	.	'("--read-envelope-from"))
  (message-send-mail-function		.	#'message-send-mail-with-sendmail)
  (send-mail-function			.	#'message-send-mail-with-sendmail)
  (mu4e-context-policy			.	'pick-first) ;; start with the first (default) context;
  (mu4e-compose-context-policy		.	'ask) ;; ask for context if no context matches;
  (message-kill-buffer-on-exit		.	t)
  ;; optional
  ;; store link to message if in header view, not to header query:
  (org-mu4e-link-query-in-headers-mode	.	nil)
  ;; don't have to confirm when quitting:
  (mu4e-confirm-quit			.	nil)
  ;; number of visible headers in horizontal split view:
  (mu4e-headers-visible-lines		.	5)
  ;; hide annoying "mu4e Retrieving mail..." msg in mini buffer:
  (mu4e-hide-index-messages		.	t)
  ;; M-x find-function RET message-citation-line-format for docs:
  (message-citation-line-function	.	'message-insert-formatted-citation-line)
  ;; by default do show related emails:
  (mu4e-search-include-related		.	nil)
  ;; ommit duplicates
  (mu4e-search-skip-duplicates		.	t)
  (mu4e-headers-attach-mark		.	`("a" . ,(propertize "@" 'face '(:foreground "gold" :weight bold))))
  (mu4e-headers-calendar-mark		.	`("c" . ,(propertize "%" 'face '(:foreground "deep sky blue"))))
  (mu4e-headers-draft-mark		.	`("D" . ,(propertize "*" 'face '(:foreground "orange" :slant italic))))
  (mu4e-headers-encrypted-mark		.	`("x" . ,(propertize "?" 'face '(:foreground "medium purple" :weight bold))))
  (mu4e-headers-flagged-mark		.	`("F" . ,(propertize "+" 'face '(:foreground "red" :weight bold))))
  (mu4e-headers-list-mark		.	`("l" . ,(propertize "L" 'face '(:foreground "forest green"))))
  (mu4e-headers-new-mark		.	`("N" . ,(propertize "!" 'face '(:foreground "orange red" :weight bold))))
  (mu4e-headers-passed-mark		.	`("P" . ,(propertize ">" 'face '(:foreground "gray"))))
  (mu4e-headers-personal-mark		.	`("p" . ,(propertize "y" 'face '(:foreground "light sea green" :slant italic))))
  (mu4e-headers-replied-mark		.	`("R" . ,(propertize "R" 'face '(:foreground "steel blue" :weight bold))))
  (mu4e-headers-seen-mark		.	`("S" . ,(propertize "]" 'face '(:foreground "gray55"))))
  (mu4e-headers-signed-mark		.	`("s" . ,(propertize "{" 'face '(:foreground "green4" :weight bold))))
  (mu4e-headers-trashed-mark		.	`("T" . ,(propertize "X" 'face '(:foreground "firebrick" :weight bold))))
  (mu4e-headers-unread-mark		.	`("u" . ,(propertize "[" 'face '(:foreground "dodger blue" :weight bold))))
  (mu4e-headers-date-format		.	"%b %-d, %Y") ; e.g., "mar 3 2024"
  (mu4e-headers-time-format		.	"%b %-d, %H:%M") ; e.g., "mar 3, 17:47"
  ;; Gmail style headers
  (mu4e-headers-fields			.	'((:human-date  . 12)
						  (:flags	. 7)
						  (:from	. 30)
						  (:subject	. 92)))
  :config
  (setf (alist-get 'trash mu4e-marks)
	'(:char ("d" . "▼")
		:prompt "dtrash"
		:dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
		;; Here's the main difference to the regular trash mark, no +T
		;; before -N so the message is not marked as IMAP-deleted, unless
		;; it's Gmail.
		:action (lambda (docid msg target)
                          (let ((maildir (mu4e-message-field msg :maildir)))
                            (if (string-match-p "Gmail\\|Google" maildir)
				(mu4e--server-move docid (mu4e--mark-check-target target) "+T+S-u-N")
                              (mu4e--server-move docid (mu4e--mark-check-target target) "+S-u-N"))))))

  (setf (alist-get 'refile mu4e-marks)
	'(:char ("r" . "▶")
		:prompt "refile"
		:dyn-target (lambda (target msg) (mu4e-get-refile-folder msg))
		;; Notice the special treatment for Gmail.
		:action (lambda (docid msg target)
                          (let ((maildir (mu4e-message-field msg :maildir)))
                            (if (string-match-p "Gmail\\|Google" maildir)
				(mu4e--server-remove docid)
                              (mu4e--server-move docid (mu4e--mark-check-target target) "+S-u-N")))))))
