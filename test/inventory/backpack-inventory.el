(require 'ert)
(require 'backpack-inventory)

(ert-deftest test-inventory-no-negative-prefix-gears-from-guard-checks ()
  "Guard checks like (not (gearp! :completion -consult)) in :when gates
should not produce spurious gear entries with negative-prefixed names."
  :tags '(backpack)
  (let* ((form '(leaf consult-eglot
		  :doc "workspace symbol search via eglot"
		  :when (and (not (gearp! :completion -consult))
			     (gearp! :completion consult eglot)
			     (not (gearp! :completion -eglot)))))
	 (results (backpack-inventory--parse-leaf-form form :completion))
	 (gear-names (mapcar (lambda (r) (plist-get r :name))
			     (cl-remove-if-not
			      (lambda (r) (eq (plist-get r :type) :gear))
			      results))))
    ;; Should not contain -consult or -eglot as gear names
    (should-not (member '-consult gear-names))
    (should-not (member '-eglot gear-names))))

(ert-deftest test-inventory-extracts-flag-from-when-gate ()
  "A :when gate with 3-arg gearp! should correctly extract the flag."
  :tags '(backpack)
  (let* ((form '(leaf consult-eglot
		  :doc "workspace symbol search via eglot"
		  :when (and (not (gearp! :completion -consult))
			     (gearp! :completion consult eglot)
			     (not (gearp! :completion -eglot)))))
	 (results (backpack-inventory--parse-leaf-form form :completion))
	 (flags (cl-remove-if-not
		 (lambda (r) (eq (plist-get r :type) :flag))
		 results)))
    ;; Should extract the eglot flag on the consult gear
    (should (= 1 (length flags)))
    (should (eq 'consult (plist-get (car flags) :gear)))
    (should (eq 'eglot (plist-get (car flags) :name)))))

(ert-deftest test-inventory-default-on-gear-from-unless ()
  "An :unless gate with (gearp! :completion -consult) should produce
a default-on gear named consult (without the dash prefix)."
  :tags '(backpack)
  (let* ((form '(leaf consult
		  :doc "enhanced minibuffer commands"
		  :unless (gearp! :completion -consult)))
	 (results (backpack-inventory--parse-leaf-form form :completion))
	 (gears (cl-remove-if-not
		 (lambda (r) (eq (plist-get r :type) :gear))
		 results)))
    (should (= 1 (length gears)))
    (should (eq 'consult (plist-get (car gears) :name)))
    (should (plist-get (car gears) :default-on))))
