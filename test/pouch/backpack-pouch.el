(require 'ert)
(require 'backpack-pouch)

(ert-deftest test-gear! ()
  :tags '(backpack)
  (gear!
   :ui
   (theme doom-one))
  (should (equal backpack--gear '(:ui (theme doom-one)))))

(ert-deftest test-gearp!-throw-error ()
  :tags '(backpack)
  (gear!
   theme)

  (should-error (gearp! :ui theme)))

(ert-deftest test-gearp!-throw-error-two-pouches ()
  :tags '(backpack)
  (gear!
   :ui
   :defaults)

  (should-error (gearp! :ui theme)))

(ert-deftest test-gearp!-find-ui-theme ()
    :tags '(backpack)
    (gear!
     :ui
     theme)

    (should (gearp! :ui theme)))

(ert-deftest test-gearp!-find-ui-theme-with-flag ()
  :tags '(backpack)
  (gear!
   :ui
   (theme doom-one))

  (should (gearp! :ui theme doom-one)))

(ert-deftest test-gearp!-cant-find-module ()
  :tags '(backpack)
  (gear!
   :ui
   theme)

  (should-not (gearp! :ui not-included)))

(ert-deftest test-gearp!-query-absent-pouch-multi-pouch ()
  "gearp! should return nil (not error) when querying a pouch
that does not exist in gear!, even with multiple pouches present."
  :tags '(backpack)
  (gear!
   :editing
   go
   nix
   :config
   (default hide-menu-bar))
  (should-not (gearp! :ui theme)))

(ert-deftest test-gearp!-query-absent-gear-across-pouches ()
  "gearp! should return nil when the gear is not in any pouch,
without erroring on pouch transitions."
  :tags '(backpack)
  (gear!
   :editing
   go
   :config
   (default hide-menu-bar))
  (should-not (gearp! :editing python)))

(ert-deftest test-gearp!-flag-query-across-pouches ()
  "gearp! with a flag should work correctly across multiple pouches."
  :tags '(backpack)
  (gear!
   :config
   (default hide-menu-bar)
   :editing
   (go lsp))
  (should (gearp! :editing go lsp))
  (should-not (gearp! :ui -treesit)))

(ert-deftest test-gearp!-reset-across-pouches ()
  "gearp! should not carry over module matches from a previous pouch."
  :tags '(backpack)
  (gear!
   :ui
   theme
   :editing
   go)
  ;; `theme' is under :ui, not :editing
  (should (gearp! :ui theme))
  (should-not (gearp! :editing theme)))

(ert-deftest benchmark-gearp! ()
  (gear!
   :ui
   (theme doom-one)

   (should (< (benchmark-run 10000 (gearp! :ui theme)) 0.1))))

(ert-deftest test-backpack--extract-gear-form-with-gear ()
  "Should extract gear! form and rest forms from a file containing both."
  :tags '(backpack)
  (let ((tmpfile (make-temp-file "backpack-test-" nil ".el"
                                  ";; test config\n(gear!\n :ui\n (theme doom-one)\n :editing go)\n\n(set-face-attribute 'default nil :height 160)\n(setq some-var t)")))
    (unwind-protect
        (pcase-let ((`(,gear-form . ,rest-forms) (backpack--extract-gear-form tmpfile)))
          (should (equal gear-form '(gear! :ui (theme doom-one) :editing go)))
          (should (= 2 (length rest-forms)))
          (should (equal (car rest-forms) '(set-face-attribute (quote default) nil :height 160)))
          (should (equal (cadr rest-forms) '(setq some-var t))))
      (delete-file tmpfile))))

(ert-deftest test-backpack--extract-gear-form-no-gear ()
  "Should return nil gear form when file has no gear! declaration."
  :tags '(backpack)
  (let ((tmpfile (make-temp-file "backpack-test-" nil ".el"
                                  "(setq some-var t)\n(message \"hello\")")))
    (unwind-protect
        (pcase-let ((`(,gear-form . ,rest-forms) (backpack--extract-gear-form tmpfile)))
          (should (null gear-form))
          (should (= 2 (length rest-forms))))
      (delete-file tmpfile))))

(ert-deftest test-backpack--extract-gear-form-empty ()
  "Should return nil gear form and empty rest for an empty file."
  :tags '(backpack)
  (let ((tmpfile (make-temp-file "backpack-test-" nil ".el" "")))
    (unwind-protect
        (pcase-let ((`(,gear-form . ,rest-forms) (backpack--extract-gear-form tmpfile)))
          (should (null gear-form))
          (should (null rest-forms)))
      (delete-file tmpfile))))

(ert-deftest test-backpack--extract-gear-form-only-gear ()
  "Should return gear form and empty rest when file only has gear!."
  :tags '(backpack)
  (let ((tmpfile (make-temp-file "backpack-test-" nil ".el"
                                  "(gear! :ui theme)")))
    (unwind-protect
        (pcase-let ((`(,gear-form . ,rest-forms) (backpack--extract-gear-form tmpfile)))
          (should (equal gear-form '(gear! :ui theme)))
          (should (null rest-forms)))
      (delete-file tmpfile))))
