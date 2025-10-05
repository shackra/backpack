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
  "Internal helper for `gearp!`."
  (let ((gears (copy-sequence backpack--gear))
	thing
	category
	module
	f)
    (while (setq thing (pop gears))
      (cond
       ;; the first element is not a pouch/category
       ((and (null category)
	     (not (keywordp thing)))
	(throw "gear '%s' is not part of a pouch" thing))
       ;; two pouches are defined one after the other without defining gears
       ((and (null module)
	     category
	     (keywordp thing))
	(throw "last pouch '%s' did not use any gear" category))
       ;; found pouch/category
       ((and (keywordp thing)
	     (eq thing pouch))
	(setq category thing))
       ;; found gear/module
       ((and (symbolp thing)
	     (eq thing gear))
	(setq module thing))
       ;; found a list!
       ((listp thing)
	(setq module (pop thing))
	(when (memq flag thing)
	  (setq f flag)))))

    ;; return whatever was found
    (let ((found-principals (and category module)))
      (or 
       (and flag
	    found-principals
	    f)
       (and found-principals
	    (null f))))))

(provide 'backpack-pouch)
