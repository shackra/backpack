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
