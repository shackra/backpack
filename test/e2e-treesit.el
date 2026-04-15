;;; e2e-treesit.el --- End-to-end tests for tree-sitter editing gears -*- lexical-binding: t; -*-
;;
;; These tests verify that tree-sitter modes activate correctly when
;; visiting files with appropriate extensions.  They are meant to run
;; inside an Emacs session that already booted Backpack with packages
;; and tree-sitter grammars installed (via `backpack ensure').
;;
;; The orchestration script etc/scripts/run-e2e-treesit.sh takes care
;; of copying the repo, generating the init.el, running ensure, and
;; finally launching Emacs with these tests.
;;
;; Usage (manual, without Nix):
;;   etc/scripts/run-e2e-treesit.sh emacs .
;;
;; Usage (with Nix/devenv):
;;   run-e2e-treesit emacs-rolling .

(require 'ert)
(require 'treesit nil t)

;;; --- Accumulator for gear declarations -----------------------------------

(defvar backpack-e2e--test-specs nil
  "Alist of test specifications accumulated by `backpack-e2e-treesit-test'.
Each entry is (NAME . (:gear GEAR :file FILE :content CONTENT :treesit LANG)).")

(defvar backpack-e2e--generate-only nil
  "When non-nil, only accumulate specs without registering ERT tests.
Set this to t before loading the file when you only need the gear
declarations for init.el generation (step 2 of the orchestrator).")

;;; --- The macro -----------------------------------------------------------

(defmacro backpack-e2e-treesit-test (name &rest props)
  "Declare an end-to-end tree-sitter test called NAME.

PROPS is a plist with the following keys:
  :gear     -- List of gear symbols, e.g. (go) or (go lsp).
               Only the first element is used as the gear name for
               the `gear!' declaration under `:editing'.
  :file     -- Filename to create, e.g. \"main.go\".
  :content  -- String to write into the file (parsers need some
               content to detect the language).
  :treesit  -- Expected tree-sitter language symbol, e.g. `go'.

This macro does two things at load time:
1. Accumulates the gear declaration so `backpack-e2e--build-gear-form'
   can produce the combined `gear!' for init.el generation.
2. Generates an `ert-deftest' called `e2e-treesit-NAME' that creates
   a temporary file, visits it, and asserts that `treesit-language-at'
   returns the expected language."
  (declare (indent 1))
  (let ((test-name (intern (format "e2e-treesit-%s" name)))
        (gear     (plist-get props :gear))
        (file     (plist-get props :file))
        (content  (plist-get props :content))
        (ts-lang  (plist-get props :treesit)))
    `(progn
       ;; 1. Accumulate spec (always, even in generate-only mode)
       (push (cons ',name (list :gear ',gear
                                :file ,file
                                :content ,content
                                :treesit ',ts-lang))
             backpack-e2e--test-specs)

       ;; 2. Generate ERT test (skipped when only generating init.el)
       (unless backpack-e2e--generate-only
         (ert-deftest ,test-name ()
           ,(format "Visit a %s file and verify tree-sitter activates for `%s'." file ts-lang)
           :tags '(backpack e2e treesit)
           (unless (and (featurep 'treesit) (treesit-available-p))
             (ert-skip "tree-sitter not available in this Emacs build"))
           (let* ((tmp-dir (make-temp-file "backpack-e2e-" t))
                  (tmp-file (expand-file-name ,file tmp-dir))
                  (buf nil))
             (unwind-protect
                 (progn
                   ;; Write the test file
                   (with-temp-file tmp-file
                     (insert ,content))

                   ;; Visit it -- capture any error from mode hooks
                   (condition-case err
                       (setq buf (find-file-noselect tmp-file))
                     (error
                      (ert-fail
                       (format "Error visiting %s: %S" ,file err))))

                   ;; Verify tree-sitter is active
                   (with-current-buffer buf
                     (should (eq (treesit-language-at (point)) ',ts-lang))))

               ;; Cleanup: kill buffer and remove temp files
               (when (and buf (buffer-live-p buf))
                 (kill-buffer buf))
               (delete-directory tmp-dir t))))))))

;;; --- Test declarations ---------------------------------------------------

(backpack-e2e-treesit-test go
			   :gear (go)
			   :file "main.go"
			   :content "package main"
			   :treesit go)

(backpack-e2e-treesit-test nix
			   :gear (nix)
			   :file "default.nix"
			   :content "{ pkgs ? import <nixpkgs> {} }: pkgs"
			   :treesit nix)

(backpack-e2e-treesit-test lua
			   :gear (lua)
			   :file "init.lua"
			   :content "local M = {}\nreturn M"
			   :treesit lua)

(backpack-e2e-treesit-test cpp
			   :gear (cpp)
			   :file "main.cpp"
			   :content "#include <iostream>\nint main() { return 0; }"
			   :treesit cpp)

(backpack-e2e-treesit-test terraform
			   :gear (terraform)
			   :file "main.tf"
			   :content "resource \"time_static\" \"time_update\" {}"
			   :treesit terraform)

(backpack-e2e-treesit-test markdown
			   :gear (markdown)
			   :file "README.md"
			   :content "# Hello World"
			   :treesit markdown)

(backpack-e2e-treesit-test c
			   :gear (c)
			   :file "main.c"
			   :content "#include <std.io>"
			   :treesit c)

(backpack-e2e-treesit-test cmake
			   :gear (cmake)
			   :file "CMakeLists.txt"
			   :content " "
			   :treesit cmake)

(backpack-e2e-treesit-test haskell
			   :gear (haskell)
			   :file "main.hs"
			   :content " "
			   :treesit haskell)

(backpack-e2e-treesit-test json
			   :gear (json)
			   :file "main.json"
			   :content "{}"
			   :treesit json)

(backpack-e2e-treesit-test python
			   :gear (python)
			   :file "main.py"
			   :content "import this"
			   :treesit python)

(backpack-e2e-treesit-test rust
			   :gear (rust)
			   :file "main.rs"
			   :content "let a"
			   :treesit rust)

(backpack-e2e-treesit-test toml
			   :gear (toml)
			   :file "main.toml"
			   :content "hello: world"
			   :treesit toml)

(backpack-e2e-treesit-test yaml
			   :gear (yaml)
			   :file "main.yml"
			   :content "hello: world"
			   :treesit yaml)

;;; --- Interactive test runner (used by step 4 of the orchestrator) ---------

(defvar backpack-e2e--results-file nil
  "Path to write test results.  Set via --eval from the shell script.")

(defun backpack-e2e--diagnostics ()
  "Return a string with diagnostic information about the treesit/treesit-auto state.
Useful for understanding why a test might be failing."
  (let ((lines '()))
    ;; treesit availability
    (push (format "  treesit available: %s" (and (featurep 'treesit) (treesit-available-p))) lines)
    ;; treesit-auto state
    (push (format "  treesit-auto loaded: %s" (featurep 'treesit-auto)) lines)
    (when (featurep 'treesit-auto)
      (push (format "  global-treesit-auto-mode: %s"
                    (bound-and-true-p global-treesit-auto-mode))
            lines)
      (push (format "  treesit-auto-langs: %s"
                    (bound-and-true-p treesit-auto-langs))
            lines))
    ;; load paths
    (push (format "  treesit-extra-load-path: %s"
                  (bound-and-true-p treesit-extra-load-path))
          lines)
    ;; mode remap alist (global)
    (push (format "  major-mode-remap-alist (global): %s"
                  (default-value 'major-mode-remap-alist))
          lines)
    (mapconcat #'identity (nreverse lines) "\n")))

(defun backpack-e2e--buffer-diagnostics (buf file)
  "Return diagnostic info about BUF after visiting FILE."
  (with-current-buffer buf
    (let ((lines '())
          (ext (file-name-extension file)))
      (push (format "  major-mode: %s" major-mode) lines)
      (push (format "  local major-mode-remap-alist: %s"
                    (if (local-variable-p 'major-mode-remap-alist)
                        major-mode-remap-alist
                      "(not buffer-local)"))
            lines)
      (push (format "  treesit-parser-list: %s"
                    (and (featurep 'treesit) (treesit-parser-list)))
            lines)
      ;; Show auto-mode-alist entries relevant to this file's extension
      (when ext
        (push (format "  auto-mode-alist entries for .%s: %s" ext
                      (seq-filter (lambda (e)
                                    (string-match-p (concat "\\." (regexp-quote ext))
                                                    (car e)))
                                  auto-mode-alist))
              lines))
      (mapconcat #'identity (nreverse lines) "\n"))))

(defun backpack-e2e--run-tests ()
  "Run all accumulated E2E tree-sitter tests and write results to file.
This is meant to be called from `elpaca-after-init-hook' so that all
packages are fully activated and tree-sitter grammars are discoverable."
  (let ((results nil)
        (failures 0)
        (total 0))

    ;; Emit global diagnostics once before running any tests
    (push (format "--- Diagnostics at test start ---\n%s\n  backpack--treesit-langs: %s\n---"
                  (backpack-e2e--diagnostics)
                  (bound-and-true-p backpack--treesit-langs))
          results)

    (dolist (spec (reverse backpack-e2e--test-specs))
      (let* ((name    (car spec))
             (props   (cdr spec))
             (file    (plist-get props :file))
             (content (plist-get props :content))
             (ts-lang (plist-get props :treesit))
             (tmp-dir (make-temp-file "backpack-e2e-" t))
             (tmp-file (expand-file-name file tmp-dir))
             (buf nil))
        (cl-incf total)
        (unwind-protect
            (condition-case err
                (progn
                  ;; Write test file
                  (with-temp-file tmp-file
                    (insert content))
                  ;; Visit it
                  (setq buf (find-file-noselect tmp-file))
                  (with-current-buffer buf
                    (let ((got (and (featurep 'treesit)
                                    (treesit-available-p)
                                    (treesit-language-at (point)))))
                      (if (eq got ts-lang)
                          (push (format "PASS  %-12s %-20s treesit-language-at => %s"
                                        name file got)
                                results)
                        (cl-incf failures)
                        (push (format "FAIL  %-12s %-20s expected %s, got %s\n%s"
                                      name file ts-lang got
                                      (backpack-e2e--buffer-diagnostics buf file))
                              results)))))
              (error
               (cl-incf failures)
               (push (format "FAIL  %-12s %-20s error: %S" name file err)
                     results)))
          ;; Cleanup
          (when (and buf (buffer-live-p buf))
            (kill-buffer buf))
          (delete-directory tmp-dir t))))

    ;; Write results file
    (setq results (nreverse results))
    (let ((report (concat "E2E Tree-sitter Test Results\n"
                          "============================\n"
                          (mapconcat #'identity results "\n")
                          "\n\n"
                          (if (zerop failures)
                              (format "All %d tests passed.\n" total)
                            (format "%d of %d tests FAILED.\n" failures total)))))
      (when backpack-e2e--results-file
        (with-temp-file backpack-e2e--results-file
          (insert report)))
      (princ report #'external-debugging-output))

    (kill-emacs failures)))

(defun backpack-e2e--run-and-exit ()
  "Hook into `elpaca-after-init-hook' to run tests after full initialization.
Called via -f from the orchestrator script."
  (add-hook 'elpaca-after-init-hook #'backpack-e2e--run-tests 90))

;;; --- init.el generation (used by the orchestrator script) ----------------

(defun backpack-e2e--build-gear-form ()
  "Build a `gear!' s-expression from accumulated test specs.
Returns a string containing the Elisp form."
  (let ((gears (mapcar (lambda (spec)
                         (car (plist-get (cdr spec) :gear)))
                       backpack-e2e--test-specs)))
    ;; Remove duplicates (a gear might appear in multiple tests)
    (setq gears (delete-dups gears))
    (format "(gear!\n  :config\n  default\n  :editing\n  %s)\n"
            (mapconcat #'symbol-name gears "\n  "))))

(defun backpack-e2e--print-init-el ()
  "Print the generated init.el content to stdout and exit.
Called by the orchestrator script to produce the user init.el."
  (princ (backpack-e2e--build-gear-form))
  (kill-emacs 0))

(provide 'e2e-treesit)

;;; e2e-treesit.el ends here
