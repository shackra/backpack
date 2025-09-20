(require 'ert)
(require 'pouch)

(ert-deftest test-gear! ()
  :tags '(backpack)
  (gear!
   :ui
   (theme +doom-one))
  (should (equal backpack--gear '(:ui (theme +doom-one)))))
