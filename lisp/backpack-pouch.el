;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(defvar backpack--gear '()
  "List of gear in use by the user.")

(defvar backpack-log-loading nil
  "Tell Backpack to log every file load.")

(defmacro gear! (&rest gear)
  "Declare the GEAR to use."
  `(setq backpack--gear ',gear))

(defmacro gearp! (pouch gear &optional flag)
  "Check if GEAR in POUCH was enabled.

For example, if `(gear! :ui (theme doom-one))' then:
(gearp! :ui) => nil
(gearp! :ui theme) => t
(gearp! :ui theme doom-one) => t
(gearp! :ui emacs) => nil"
  `(backpack--gearp!-impl ',pouch ',gear ',flag))

(defmacro gear-with-any-flagp! (pouch gear &rest flags)
  "Check if GEAR in POUCH was enabled with any FLAGS."
  (let ((flags-sym (gensym "flags"))
        (flag-sym (gensym "flag"))
        (found-sym (gensym "found")))
    `(let ((,flags-sym (list ,@(mapcar (lambda (f) `',f) flags)))
           ,flag-sym
           ,found-sym)
       (when (null ,flags-sym)
         (error "No flags passed"))
       (while (and ,flags-sym (not ,found-sym))
         (setq ,flag-sym (pop ,flags-sym))
         (when (backpack--gearp!-impl ',pouch ',gear ,flag-sym)
           (setq ,found-sym t)))
       ,found-sym)))

(require 'cl-lib)

(defun backpack--gearp!-impl (pouch gear &optional flag)
  "Internal helper for `gearp!`.

Return non-nil if GEAR in POUCH is active, optionally with FLAG."
  (let ((category nil)
        (module nil)
        (ourflag nil)
        (has-gears nil))
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
              ((and (not has-gears)
                    category
                    (keywordp thing))
               (error "last pouch '%s' did not use any gear" category))

              ;; found a pouch/category -- reset per-pouch state
              ((keywordp thing)
               (setq category thing
                     module nil
                     ourflag nil
                     has-gears nil))

              ;; found a gear/module symbol
              ((symbolp thing)
               (setq has-gears t)
               (when (eq thing gear)
                 (setq module thing)))

              ;; found a list! => gear + flags
              ((listp thing)
               (setq has-gears t)
               (let ((name (car thing))
                     (flags (cdr thing)))
                 (when (eq name gear)
                   (setq module name)
                   (when (memq flag flags)
                     (setq ourflag flag)))))))

    ;; return result
    (or
     (and (eq pouch category) (eq gear module) (null flag))
     (and (eq pouch category) (eq gear module) (eq ourflag flag)))))

(defun backpack--extract-gear-form (file)
  "Read FILE and split it into two parts: the gear! form and everything else.
Return a cons cell (GEAR-FORM . REST-FORMS) where GEAR-FORM is the
first top-level (gear! ...) form found (or nil), and REST-FORMS is
a list of all other top-level forms in evaluation order."
  (let ((gear-form nil)
        (rest-forms nil)
        (found-gear nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (if (and (not found-gear)
                       (listp form)
                       (eq (car form) 'gear!))
                  (setq gear-form form
                        found-gear t)
                (push form rest-forms))))
        (end-of-file nil)))
    (cons gear-form (nreverse rest-forms))))

(provide 'backpack-pouch)
