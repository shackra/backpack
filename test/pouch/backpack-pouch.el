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

(ert-deftest benchmark-gearp! ()
  (gear!
   :ui
   (theme doom-one)

   (should (< (benchmark-run 10000 (gearp! :ui theme)) 0.1))))
