(require 'cl-lib)

(defvar backpack--gear '()
  "List of gear in use by the user.")

(defvar backpack-log-loading nil
  "Tell Backpack to log every file load.")

(defmacro gear! (&rest gear)
  "Declare the GEAR to use."
  `(setq backpack--gear ',gear))

(defmacro gearp! (pouch gear &optional flag)
  "Check if GEAR in POUCH was enabled

For example, if `(gear! :ui (theme doom-one))' then:
(gearp! :ui) => nil
(gearp! :ui theme) => t
(gearp! :ui theme doom-one) => t
(gearp! :ui emacs) => nil"
  `(backpack--gearp!-impl ',pouch ',gear ',flag))

(defmacro gear-keep-disabledp! (pouch gear &optional flag)
  "Check if GEAR in POUCH should remain disabled."
  `(not (backpack--gearp!-impl ',pouch ',gear ',flag)))

(defun backpack--gearp!-impl (pouch gear &optional flag)
  "Internal helper for `gearp!`.

Return non-nil if GEAR in POUCH is active, optionally with FLAG."
  (let ((category nil)
        (module nil)
        (ourflagnil))
    (cl-loop for thing in backpack--gear
	     unless (or
		     (and (eq pouch category) (eq gear module) (null flag))
		     (and (eq pouch category) (eq gear module) (eq ourflag flag)))
             do
             (cond
              ;; the first element is not a pouch/category
              ((and (null category)
                    (not (keywordp thing)))
               (error "gear '%s' is not part of a pouch" thing))

              ;; two pouches in a row without defining gears
              ((and (null module)
                    category
                    (keywordp thing))
               (error "last pouch '%s' did not use any gear" category))

              ;; found a pouch/category
              ((keywordp thing)
               (setq category thing))

              ;; found a gear/module symbol
              ((and (symbolp thing)
                    (eq thing gear))
               (setq module thing))

              ;; found a list! => gear + flags
              ((listp thing)
               (setq module (pop thing))
               (when (memq flag thing)
                 (setq ourflag flag)))))

    ;; return result
    (or
     (and (eq pouch category) (eq gear module) (null flag))
     (and (eq pouch category) (eq gear module) (eq ourflag flag)))))

(provide 'backpack-pouch)
