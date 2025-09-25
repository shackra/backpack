(defvar backpack--gear '()
  "List of gear in use by the user.")

(defvar backpack-log-loading nil
  "Tell Backpack to log every file load.")

(defmacro gear! (&rest gear)
  "Declare the GEAR to use."
  `(setq backpack--gear ',gear))

(defmacro gearp! (category &optional gear flag)
  "Check if GEAR is in use.

For example, if `(gear! :ui (theme +doom-one))' then:
(gearp! :ui) => t
(gearp! :ui theme) => t
(gearp! :ui theme +doom-one) => t
(gearp! :ui emacs) => nil
(gearp! :other) => nil"
  `(backpack--gearp!-impl ',category ',gear ',flag))

(defun backpack--gearp!-impl (category &optional gear flag)
  nil)

(provide 'backpack-pouch)
