(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
				:ref nil :depth 1 :inherit ignore
				:files (:defaults "elpaca-test.el" (:exclude "extensions"))
				:build (:not elpaca--activate-package)))

(progn
  (let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
	 (build (expand-file-name "elpaca/" elpaca-builds-directory))
	 (order (cdr elpaca-order))
	 (default-directory repo))
    (unless (file-exists-p repo)
      (make-directory repo t)
      (condition-case-unless-debug err
          (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                    ((zerop (apply #'call-process
				   `("git" nil ,buffer t "clone"
                                     ,@(when-let* ((depth (plist-get order :depth)))
					 (list (format "--depth=%d" depth) "--no-single-branch"))
                                     ,(plist-get order :repo) ,repo))))
                    ((zerop (call-process "git" nil buffer t "checkout"
                                          (or (plist-get order :ref) "--"))))
                    (emacs (concat invocation-directory invocation-name))
                    ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                          "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                    ((require 'elpaca))
                    ((elpaca-generate-autoloads "elpaca" repo)))
              (progn (message "%s" (buffer-string)) (kill-buffer buffer))
            (error "%s" (with-current-buffer buffer (buffer-string))))
	((error) (warn "%s" err) (delete-directory repo 'recursive))))
    (unless (require 'elpaca-autoloads nil t)
      (require 'elpaca)
      (elpaca-generate-autoloads "elpaca" repo)
      (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))

  (elpaca `(,@elpaca-order))

  (add-hook 'elpaca-post-queue-hook
	    (lambda ()
	      ;; (save-default-bg-fg-colors)
	      (unless (gearp! :ui -treesit)
		(when (fboundp 'treesit-auto-install-all)
		  (message "compiling tree-sitter grammars")
		  (let ((treesit-auto-install t))
		    (treesit-auto-install-all))))
	      (kill-emacs 0)))

  (backpack-load-gear-files)
  (elpaca-wait))
