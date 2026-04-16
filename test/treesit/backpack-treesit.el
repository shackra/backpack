;; -*- lexical-binding: t; -*-

;;; Tests for backpack--plist-remove, backpack--plist-merge,
;;; backpack--treesit-resolve-recipe, and backpack-treesit-recipe!
;;;
;;; These tests exercise the macro and its helpers without requiring a full
;;; Backpack / elpaca boot.  treesit-auto symbols are stubbed out so the tests
;;; run in plain batch Emacs.

(require 'ert)
(require 'cl-lib)

;;; ---------------------------------------------------------------------------
;;; Minimal stubs so the macro's runtime expansion works without treesit-auto
;;; ---------------------------------------------------------------------------

;; Provide treesit-auto as a feature so `with-eval-after-load' bodies run
;; immediately throughout these tests.
(unless (featurep 'treesit-auto)
  (cl-defstruct treesit-auto-recipe
    "Minimal stub of the treesit-auto-recipe struct."
    lang ts-mode remap url revision source-dir ext)
  (defvar treesit-auto-recipe-list nil
    "Stub for treesit-auto-recipe-list used in tests.")
  (provide 'treesit-auto))

;;; ---------------------------------------------------------------------------
;;; Helper macro: reset shared state between tests
;;; ---------------------------------------------------------------------------

(defmacro with-clean-treesit-state (&rest body)
  "Run BODY with fresh `treesit-auto-recipe-list' and `backpack--treesit-langs'."
  `(let ((treesit-auto-recipe-list nil)
         (backpack--treesit-langs nil))
     ,@body))

;;; ---------------------------------------------------------------------------
;;; backpack--plist-remove
;;; ---------------------------------------------------------------------------

(ert-deftest test-backpack--plist-remove-present ()
  "Removing an existing key returns plist without that key/value."
  :tags '(backpack treesit)
  (should (equal (backpack--plist-remove '(:a 1 :b 2 :c 3) :b)
                 '(:a 1 :c 3))))

(ert-deftest test-backpack--plist-remove-absent ()
  "Removing a missing key returns the original plist unchanged."
  :tags '(backpack treesit)
  (should (equal (backpack--plist-remove '(:a 1 :b 2) :z)
                 '(:a 1 :b 2))))

(ert-deftest test-backpack--plist-remove-only-first ()
  "When key appears multiple times only the first occurrence is removed."
  :tags '(backpack treesit)
  (should (equal (backpack--plist-remove '(:a 1 :b 2 :a 3) :a)
                 '(:b 2 :a 3))))

(ert-deftest test-backpack--plist-remove-empty ()
  "Removing from an empty plist returns nil."
  :tags '(backpack treesit)
  (should (null (backpack--plist-remove nil :a))))

;;; ---------------------------------------------------------------------------
;;; backpack--plist-merge
;;; ---------------------------------------------------------------------------

(ert-deftest test-backpack--plist-merge-override-wins ()
  "Keys in the override plist replace same keys in base."
  :tags '(backpack treesit)
  (should (equal (backpack--plist-merge '(:url "old" :revision "main")
                                        '(:revision "abc123"))
                 '(:url "old" :revision "abc123"))))

(ert-deftest test-backpack--plist-merge-new-key-added ()
  "Keys present only in the override are added to the result."
  :tags '(backpack treesit)
  (let ((result (backpack--plist-merge '(:url "u") '(:source-dir "src"))))
    (should (equal (plist-get result :url) "u"))
    (should (equal (plist-get result :source-dir) "src"))))

(ert-deftest test-backpack--plist-merge-base-only-key-preserved ()
  "Keys present only in base survive the merge."
  :tags '(backpack treesit)
  (let ((result (backpack--plist-merge '(:url "u" :revision "main")
                                       '(:revision "pin"))))
    (should (equal (plist-get result :url) "u"))))

(ert-deftest test-backpack--plist-merge-empty-override ()
  "Merging an empty override returns the base plist."
  :tags '(backpack treesit)
  (should (equal (backpack--plist-merge '(:a 1 :b 2) nil)
                 '(:a 1 :b 2))))

(ert-deftest test-backpack--plist-merge-empty-base ()
  "Merging into an empty base returns the override plist."
  :tags '(backpack treesit)
  (should (equal (backpack--plist-merge nil '(:a 1))
                 '(:a 1))))

;;; ---------------------------------------------------------------------------
;;; backpack--treesit-resolve-recipe  (pure function, no side effects)
;;; ---------------------------------------------------------------------------

(ert-deftest test-resolve-recipe-no-versions-returns-base ()
  "With no :versions list, the base plist is returned unchanged."
  :tags '(backpack treesit)
  (let ((base '(:url "u" :revision "main")))
    (should (equal (backpack--treesit-resolve-recipe base nil)
                   base))))

(ert-deftest test-resolve-recipe-matching-clause-overrides-revision ()
  "A clause whose :until-emacs >= emacs-version replaces :revision."
  :tags '(backpack treesit)
  (let* ((base '(:url "u" :revision "main"))
         ;; Use a far-future version string so this clause always matches.
         (versions '((:until-emacs "99.9" :revision "pinned")))
         (result (backpack--treesit-resolve-recipe base versions)))
    (should (equal (plist-get result :revision) "pinned"))
    (should (equal (plist-get result :url) "u"))))

(ert-deftest test-resolve-recipe-non-matching-clause-keeps-base ()
  "A clause whose :until-emacs < emacs-version is skipped."
  :tags '(backpack treesit)
  ;; emacs-version is at least "29.1"; "1.0" is guaranteed to be less.
  (let* ((base '(:url "u" :revision "main"))
         (versions '((:until-emacs "1.0" :revision "old")))
         (result (backpack--treesit-resolve-recipe base versions)))
    (should (equal (plist-get result :revision) "main"))))

(ert-deftest test-resolve-recipe-first-matching-clause-wins ()
  "When multiple clauses match, the first one is used."
  :tags '(backpack treesit)
  (let* ((base '(:revision "main"))
         (versions '((:until-emacs "99.8" :revision "first")
                     (:until-emacs "99.9" :revision "second")))
         (result (backpack--treesit-resolve-recipe base versions)))
    (should (equal (plist-get result :revision) "first"))))

(ert-deftest test-resolve-recipe-override-preserves-other-base-fields ()
  "An override clause only touches the keys it specifies."
  :tags '(backpack treesit)
  (let* ((base '(:url "u" :revision "main" :source-dir "src"))
         (versions '((:until-emacs "99.9" :revision "pinned")))
         (result (backpack--treesit-resolve-recipe base versions)))
    (should (equal (plist-get result :source-dir) "src"))
    (should (equal (plist-get result :url) "u"))
    (should (equal (plist-get result :revision) "pinned"))))

(ert-deftest test-resolve-recipe-override-can-add-new-key ()
  "An override clause can introduce keys not present in the base."
  :tags '(backpack treesit)
  (let* ((base '(:url "u" :revision "main"))
         (versions '((:until-emacs "99.9" :source-dir "sub/src")))
         (result (backpack--treesit-resolve-recipe base versions)))
    (should (equal (plist-get result :source-dir) "sub/src"))))

;;; ---------------------------------------------------------------------------
;;; backpack-treesit-recipe!  (integration: lang registration + recipe)
;;; ---------------------------------------------------------------------------

(ert-deftest test-backpack-treesit-recipe!-registers-lang ()
  "The macro implicitly registers the lang via backpack-treesit-langs!."
  :tags '(backpack treesit)
  (with-clean-treesit-state
   (backpack-treesit-recipe! go
     :ts-mode 'go-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-go")
   (should (memq 'go backpack--treesit-langs))))

(ert-deftest test-backpack-treesit-recipe!-registers-recipe ()
  "The macro adds a recipe to treesit-auto-recipe-list."
  :tags '(backpack treesit)
  (with-clean-treesit-state
   (backpack-treesit-recipe! go
     :ts-mode 'go-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-go")
   (should (cl-find 'go treesit-auto-recipe-list
                    :key #'treesit-auto-recipe-lang))))

(ert-deftest test-backpack-treesit-recipe!-base-fields ()
  "All base plist fields are forwarded to the recipe."
  :tags '(backpack treesit)
  (with-clean-treesit-state
   (backpack-treesit-recipe! markdown
     :ts-mode 'markdown-ts-mode
     :remap '(markdown-mode gfm-mode)
     :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
     :revision "split_parser"
     :source-dir "tree-sitter-markdown/src")
   (let ((recipe (cl-find 'markdown treesit-auto-recipe-list
                          :key #'treesit-auto-recipe-lang)))
     (should recipe)
     (should (eq  (treesit-auto-recipe-ts-mode recipe) 'markdown-ts-mode))
     (should (equal (treesit-auto-recipe-remap recipe) '(markdown-mode gfm-mode)))
     (should (equal (treesit-auto-recipe-url recipe)
                    "https://github.com/tree-sitter-grammars/tree-sitter-markdown"))
     (should (equal (treesit-auto-recipe-revision recipe) "split_parser"))
     (should (equal (treesit-auto-recipe-source-dir recipe)
                    "tree-sitter-markdown/src")))))

(ert-deftest test-backpack-treesit-recipe!-no-versions-uses-base-revision ()
  "Without :versions the base :revision is used as-is."
  :tags '(backpack treesit)
  (with-clean-treesit-state
   (backpack-treesit-recipe! go
     :ts-mode 'go-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-go"
     :revision "master")
   (let ((recipe (cl-find 'go treesit-auto-recipe-list
                          :key #'treesit-auto-recipe-lang)))
     (should (equal (treesit-auto-recipe-revision recipe) "master")))))

(ert-deftest test-backpack-treesit-recipe!-matching-version-overrides-revision ()
  "A matching :versions clause replaces :revision in the registered recipe."
  :tags '(backpack treesit)
  (with-clean-treesit-state
   ;; Use a far-future :until-emacs so the clause always matches.
   (backpack-treesit-recipe! go
     :ts-mode 'go-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-go"
     :revision "master"
     :versions ((:until-emacs "99.9" :revision "deadbeef")))
   (let ((recipe (cl-find 'go treesit-auto-recipe-list
                          :key #'treesit-auto-recipe-lang)))
     (should (equal (treesit-auto-recipe-revision recipe) "deadbeef")))))

(ert-deftest test-backpack-treesit-recipe!-non-matching-version-uses-base ()
  "A non-matching :versions clause is skipped; base :revision is used."
  :tags '(backpack treesit)
  (with-clean-treesit-state
   ;; Use a past :until-emacs string so the clause never matches.
   (backpack-treesit-recipe! go
     :ts-mode 'go-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-go"
     :revision "master"
     :versions ((:until-emacs "1.0" :revision "ancient")))
   (let ((recipe (cl-find 'go treesit-auto-recipe-list
                          :key #'treesit-auto-recipe-lang)))
     (should (equal (treesit-auto-recipe-revision recipe) "master")))))

(ert-deftest test-backpack-treesit-recipe!-version-override-preserves-other-fields ()
  "A :versions override only changes the specified keys; other base fields survive."
  :tags '(backpack treesit)
  (with-clean-treesit-state
   (backpack-treesit-recipe! markdown
     :ts-mode 'markdown-ts-mode
     :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
     :revision "split_parser"
     :source-dir "tree-sitter-markdown/src"
     :versions ((:until-emacs "99.9" :revision "oldcommit")))
   (let ((recipe (cl-find 'markdown treesit-auto-recipe-list
                          :key #'treesit-auto-recipe-lang)))
     (should (equal (treesit-auto-recipe-revision recipe) "oldcommit"))
     (should (equal (treesit-auto-recipe-source-dir recipe)
                    "tree-sitter-markdown/src"))
     (should (equal (treesit-auto-recipe-url recipe)
                    "https://github.com/tree-sitter-grammars/tree-sitter-markdown")))))

;;; ---------------------------------------------------------------------------
;;; backpack--treesit-commit-hash-p
;;; ---------------------------------------------------------------------------

(ert-deftest test-commit-hash-p-full-sha ()
  "A 40-character lowercase hex string is recognised as a commit hash."
  :tags '(backpack treesit)
  (should (backpack--treesit-commit-hash-p
           "7fe453beacecf02c86f7736439f238f5bb8b5c9b")))

(ert-deftest test-commit-hash-p-longer-sha ()
  "A SHA longer than 40 chars (e.g. SHA-256) is also accepted."
  :tags '(backpack treesit)
  (should (backpack--treesit-commit-hash-p
           (make-string 64 ?a))))

(ert-deftest test-commit-hash-p-branch-name ()
  "A branch name is not a commit hash."
  :tags '(backpack treesit)
  (should-not (backpack--treesit-commit-hash-p "main"))
  (should-not (backpack--treesit-commit-hash-p "split_parser"))
  (should-not (backpack--treesit-commit-hash-p "v1.0.0")))

(ert-deftest test-commit-hash-p-short-sha ()
  "An abbreviated SHA (fewer than 40 chars) is not treated as a full hash."
  :tags '(backpack treesit)
  (should-not (backpack--treesit-commit-hash-p "deadbeef")))

(ert-deftest test-commit-hash-p-non-hex-rejected ()
  "A 40-char string containing non-hex characters is not a commit hash."
  :tags '(backpack treesit)
  ;; 'g' through 'z' are not hex digits.
  (should-not (backpack--treesit-commit-hash-p
               "zfe453beacecf02c86f7736439f238f5bb8b5c9b")))

(ert-deftest test-commit-hash-p-nil ()
  "nil returns nil without error."
  :tags '(backpack treesit)
  (should-not (backpack--treesit-commit-hash-p nil)))

;;; ---------------------------------------------------------------------------
;;; backpack-treesit-recipe! -- deduplication
;;; ---------------------------------------------------------------------------

(ert-deftest test-backpack-treesit-recipe!-replaces-existing-recipe ()
  "Calling the macro twice for the same lang replaces the first recipe."
  :tags '(backpack treesit)
  (with-clean-treesit-state
   (backpack-treesit-recipe! go
     :ts-mode 'go-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-go"
     :revision "main")
   (backpack-treesit-recipe! go
     :ts-mode 'go-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-go"
     :revision "new-branch")
   ;; Only one recipe for 'go should remain.
   (let ((go-recipes (seq-filter (lambda (r) (eq (treesit-auto-recipe-lang r) 'go))
                                 treesit-auto-recipe-list)))
     (should (= 1 (length go-recipes)))
     (should (equal (treesit-auto-recipe-revision (car go-recipes)) "new-branch")))))

(ert-deftest test-backpack-treesit-recipe!-replaces-default-recipe ()
  "A backpack-treesit-recipe! call removes the treesit-auto default for that lang."
  :tags '(backpack treesit)
  (with-clean-treesit-state
   ;; Simulate a pre-existing default recipe (as treesit-auto would provide).
   (push (make-treesit-auto-recipe
          :lang 'go
          :ts-mode 'go-ts-mode
          :url "https://github.com/tree-sitter/tree-sitter-go"
          :revision "default-branch")
         treesit-auto-recipe-list)
   (backpack-treesit-recipe! go
     :ts-mode 'go-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-go"
     :revision "pinned-sha")
   ;; Still exactly one 'go recipe, and it's our pinned one.
   (let ((go-recipes (seq-filter (lambda (r) (eq (treesit-auto-recipe-lang r) 'go))
                                 treesit-auto-recipe-list)))
     (should (= 1 (length go-recipes)))
     (should (equal (treesit-auto-recipe-revision (car go-recipes)) "pinned-sha")))))

(ert-deftest test-backpack-treesit-recipe!-does-not-affect-other-langs ()
  "Replacing a recipe for one lang does not disturb recipes for other langs."
  :tags '(backpack treesit)
  (with-clean-treesit-state
   (backpack-treesit-recipe! go
     :ts-mode 'go-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-go"
     :revision "main")
   (backpack-treesit-recipe! rust
     :ts-mode 'rust-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-rust"
     :revision "master")
   ;; Replace only go.
   (backpack-treesit-recipe! go
     :ts-mode 'go-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-go"
     :revision "new-go")
   (should (cl-find 'rust treesit-auto-recipe-list :key #'treesit-auto-recipe-lang))
   (let ((go-recipe (cl-find 'go treesit-auto-recipe-list :key #'treesit-auto-recipe-lang)))
     (should (equal (treesit-auto-recipe-revision go-recipe) "new-go")))))

;;; ---------------------------------------------------------------------------
;;; backpack--remove-flag
;;; ---------------------------------------------------------------------------

(ert-deftest test-remove-flag-present ()
  "Removes a flag and its value from an argument list."
  :tags '(backpack treesit)
  (should (equal (backpack--remove-flag
                  '("clone" "https://x" "--depth" "1" "-b" "main" "/tmp/repo")
                  "-b")
                 '("clone" "https://x" "--depth" "1" "/tmp/repo"))))

(ert-deftest test-remove-flag-absent ()
  "Returns the list unchanged when the flag is not present."
  :tags '(backpack treesit)
  (should (equal (backpack--remove-flag
                  '("clone" "https://x" "--depth" "1" "/tmp/repo")
                  "-b")
                 '("clone" "https://x" "--depth" "1" "/tmp/repo"))))

(ert-deftest test-remove-flag-all-occurrences ()
  "Removes all occurrences of the flag (and each one's value)."
  :tags '(backpack treesit)
  ;; git clone args never have -b twice, but the function removes all of them.
  (should (null (backpack--remove-flag '("-b" "a" "-b" "b") "-b"))))

(ert-deftest test-remove-flag-empty ()
  "Removing from an empty list returns nil."
  :tags '(backpack treesit)
  (should (null (backpack--remove-flag nil "-b"))))

;;; ---------------------------------------------------------------------------
;;; backpack--treesit-clone-advice  (unit test via call interception)
;;; ---------------------------------------------------------------------------

(defconst test--sha "aabbccdd1122aabbccdd1122aabbccdd1122aabb"
  "A well-formed 40-char hex SHA used in clone-advice tests.")

(defmacro with-clone-advice-stubs (sha &rest body)
  "Run BODY with `backpack--treesit-pending-sha' bound to SHA.
`treesit--call-process-signal' is stubbed to record all calls into
the lexical variable CALLS (a list, most-recent-first)."
  (declare (indent 1))
  `(let ((calls nil)
         (backpack--treesit-pending-sha ,sha))
     (cl-letf (((symbol-function 'treesit--call-process-signal)
                (lambda (&rest a) (push a calls))))
       ,@body)
     calls))

(ert-deftest test-clone-advice-strips-b-and-depth-when-sha-pending ()
  "When SHA pending the advice removes -b AND --depth from clone args."
  :tags '(backpack treesit)
  (let ((calls (with-clone-advice-stubs test--sha
                 (backpack--treesit-clone-advice
                  (lambda (&rest a) (push a calls))
                  "git" nil t nil
                  "clone" "https://example.com/repo"
                  "--depth" "1" "--quiet"
                  "-b" test--sha "/tmp/workdir"))))
    ;; The clone call (second in the list, i.e. first chronologically)
    (let ((clone-call (car (last calls 2))))
      (should-not (member "-b"      clone-call))
      (should-not (member "--depth" clone-call))
      (should-not (member "1"       clone-call))) ; "1" was --depth's value
    ;; The checkout call (first in list = last chronologically)
    (let ((checkout-call (car calls)))
      (should (member "checkout" checkout-call))
      (should (member test--sha  checkout-call)))))

(ert-deftest test-clone-advice-passthrough-when-no-sha-pending ()
  "When no SHA is pending the advice passes args through unchanged."
  :tags '(backpack treesit)
  (let ((received nil)
        (backpack--treesit-pending-sha nil))
    (backpack--treesit-clone-advice
     (lambda (&rest a) (setq received a))
     "git" nil t nil
     "clone" "https://example.com/repo" "--depth" "1" "--quiet"
     "-b" "main" "/tmp/workdir")
    ;; -b must be preserved because we passed through untouched
    (should (member "-b" received))))

(ert-deftest test-clone-advice-passthrough-for-non-clone-git-call ()
  "Non-clone git calls pass through unchanged even when SHA is pending."
  :tags '(backpack treesit)
  (let ((received nil)
        (backpack--treesit-pending-sha "aabbccdd1122aabbccdd1122aabbccdd1122aabb"))
    (backpack--treesit-clone-advice
     (lambda (&rest a) (setq received a))
     "git" nil t nil "-C" "/tmp/repo" "checkout" "main")
    ;; Should have passed through exactly as-is
    (should (equal received
                   '("git" nil t nil "-C" "/tmp/repo" "checkout" "main")))))

(ert-deftest test-clone-advice-passthrough-for-cc-compile-call ()
  "C compiler calls pass through unchanged even when SHA is pending."
  :tags '(backpack treesit)
  (let ((received nil)
        (backpack--treesit-pending-sha "aabbccdd1122aabbccdd1122aabbccdd1122aabb"))
    (backpack--treesit-clone-advice
     (lambda (&rest a) (setq received a))
     "cc" nil t nil "-fPIC" "-c" "-I." "parser.c")
    (should (equal received
                   '("cc" nil t nil "-fPIC" "-c" "-I." "parser.c")))))

(provide 'backpack-treesit)

;;; backpack-treesit.el ends here
