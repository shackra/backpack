;; -*- lexical-binding: t; -*-
;;; backpack-treesit.el --- Tree-sitter grammar introspection

(require 'treesit)

(defun backpack-treesit--buffer-language ()
  "Return the tree-sitter language symbol for the current buffer, or nil."
  (or (when (and (fboundp 'treesit-parser-list)
                 (treesit-parser-list))
        (treesit-parser-language (car (treesit-parser-list))))
      (when (fboundp 'treesit-language-at)
        (treesit-language-at (point)))
      (when (string-suffix-p "-ts-mode" (symbol-name major-mode))
        (intern (substring (symbol-name major-mode) 0
                           (- (length (symbol-name major-mode))
                              (length "-ts-mode")))))))

(defun backpack-treesit--short-hash (hash)
  "Return the first 7 characters of HASH, or HASH if shorter."
  (if (and (stringp hash) (> (length hash) 7))
      (substring hash 0 7)
    hash))

(defun backpack-treesit--state-commit (lang)
  "Return the installed commit hash for LANG from the Backpack state file, or nil."
  (when (and (boundp 'backpack--treesit-state-file)
             (fboundp 'backpack--treesit-read-state))
    (alist-get lang (backpack--treesit-read-state))))

(defun backpack-treesit--recipe-info (lang)
  "Return a plist of (:url URL :revision REV :commit HASH) for LANG from `treesit-language-source-alist', or nil."
  (let ((entry (assoc lang treesit-language-source-alist)))
    (when entry
      (let ((val (cdr entry))
            result)
        (when val
          (if (stringp val)
              (setq result (plist-put result :url val))
            (when (and val (not (keywordp (car val))))
              (setq result (plist-put result :url (car val)))
              (setq val (cdr val)))
            (while val
              (let ((k (pop val))
                    (v (pop val)))
                (when (and (keywordp k) v)
                  (setq result
                        (plist-put result
                                   (cl-case k
                                     (:revision :revision)
                                     (:commit :commit)
                                     (:url :url)
                                     (otherwise k))
                                   v))))))
          result)))))

;;;###autoload
(defun backpack-treesit-grammar-info ()
  "Display tree-sitter grammar info for the current buffer's language."
  (interactive)
  (let* ((lang (backpack-treesit--buffer-language))
         (propertize (lambda (str) (propertize str 'face 'font-lock-type-face))))
    (if (not lang)
        (message "No tree-sitter language in current buffer")
      (let ((abi (when (fboundp 'treesit-language-abi-version)
                   (treesit-language-abi-version lang)))
            (lib-max (when (fboundp 'treesit-library-abi-version)
                       (treesit-library-abi-version)))
            (lib-min (when (fboundp 'treesit-library-abi-version)
                       (treesit-library-abi-version t)))
            (path (when (fboundp 'treesit-grammar-location)
                    (treesit-grammar-location lang)))
            (commit (backpack-treesit--state-commit lang))
            (short-lang (symbol-name lang)))
        (if (not abi)
            (message "%s: grammar not installed" (funcall propertize short-lang))
          (let* ((home-dir (expand-file-name "~/"))
                 (short-path (if (and path (string-prefix-p home-dir path))
                                 (concat "~/" (substring path (length home-dir)))
                               path))
                 (abi-str (format "ABI %s" abi))
                 (lib-range (if (and lib-min lib-max)
                                (format " (lib %s–%s)" lib-min lib-max)
                              ""))
                 (commit-str (if commit
                                (format ", commit %s"
                                        (backpack-treesit--short-hash commit))
                              "")))
            (message "%s: %s%s, %s%s"
                     (funcall propertize short-lang)
                     abi-str lib-range
                     (or short-path "path unknown")
                     commit-str)))))))

(provide 'backpack-treesit)
;;; backpack-treesit.el ends here
