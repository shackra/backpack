;; -*- lexical-binding: t; -*-
;;
;; backpack-inventory.el -- Self-documenting inventory browser for Backpack
;;
;; Browse available pouches, gears and flags interactively.
;; Run M-x backpack-inventory to open the browser.

(require 'cl-lib)
(require 'backpack-pouch)

;;; Pouch descriptions

(defvar backpack-inventory--pouch-descriptions
  '((:config     . "Core settings and sane defaults")
    (:ui         . "User interface, themes and visual features")
    (:completion . "Completion framework and related tools")
    (:tools      . "Developer tools and utilities")
    (:checkers   . "Syntax and spell checking")
    (:email      . "Email clients and utilities")
    (:editing    . "Programming language support and file type modes"))
  "Brief descriptions for each pouch category.")

;; Preferred display order for pouches (pouches not listed here
;; appear at the end in alphabetical order).
(defvar backpack-inventory--pouch-order
  '(:config :ui :completion :tools :checkers :email :editing)
  "Preferred display order for pouches.")

;;; Buffer-local state

(defvar-local backpack-inventory--history nil
  "Navigation history stack.  Each entry is (VIEW-TYPE . VIEW-DATA).")

(defvar-local backpack-inventory--current-view nil
  "Current view being displayed.  (VIEW-TYPE . VIEW-DATA).")

(defvar-local backpack-inventory--registry nil
  "Parsed registry cache.  Alist of (POUCH-KEYWORD . GEAR-LIST).")

(defconst backpack-inventory--buffer-name "*backpack-inventory*"
  "Name of the inventory buffer.")

;;; Mode definition

(defvar backpack-inventory-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'backpack-inventory-enter)
    (define-key map (kbd "l")   #'backpack-inventory-back)
    (define-key map (kbd "DEL") #'backpack-inventory-back)
    (define-key map (kbd "g")   #'backpack-inventory-refresh)
    (define-key map (kbd "n")   #'next-line)
    (define-key map (kbd "p")   #'previous-line)
    map)
  "Keymap for `backpack-inventory-mode'.")

(define-derived-mode backpack-inventory-mode special-mode "Backpack-Inventory"
  "Major mode for browsing Backpack's inventory of pouches, gears and flags.

\\{backpack-inventory-mode-map}"
  (setq truncate-lines t)
  (setq buffer-read-only t))

;;; Filesystem discovery

(defun backpack-inventory--gears-dir ()
  "Return the absolute path to the gears directory."
  (expand-file-name "gears" backpack-core-dir))

(defun backpack-inventory--discover-pouches ()
  "Scan the gears directory for pouch subdirectories.
Returns an alist of (POUCH-KEYWORD . ABSOLUTE-DIR-PATH), sorted
according to `backpack-inventory--pouch-order'."
  (let* ((gears-dir (backpack-inventory--gears-dir))
         (dirs (cl-remove-if-not
                #'file-directory-p
                (directory-files gears-dir t "\\`[^.]")))
         (pouches (mapcar (lambda (dir)
                            (cons (intern (concat ":" (file-name-nondirectory dir)))
                                  dir))
                          dirs)))
    ;; Sort according to preferred order
    (sort pouches
          (lambda (a b)
            (let ((pos-a (cl-position (car a) backpack-inventory--pouch-order))
                  (pos-b (cl-position (car b) backpack-inventory--pouch-order)))
              (cond
               ((and pos-a pos-b) (< pos-a pos-b))
               (pos-a t)
               (pos-b nil)
               (t (string< (symbol-name (car a)) (symbol-name (car b))))))))))

(defun backpack-inventory--discover-gear-files (pouch-dir)
  "List .el files in POUCH-DIR.
Returns a list of absolute file paths."
  (directory-files pouch-dir t "\\.el\\'"))

;;; S-expression parsing engine

(defun backpack-inventory--read-file-forms (filepath)
  "Read all top-level S-expressions from FILEPATH.
Returns a list of forms."
  (with-temp-buffer
    (insert-file-contents filepath)
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t
            (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(defun backpack-inventory--gearp-call-p (form)
  "Return non-nil if FORM is a (gearp! ...) call."
  (and (listp form)
       (eq (car form) 'gearp!)))

(defun backpack-inventory--gear-with-any-flagp-call-p (form)
  "Return non-nil if FORM is a (gear-with-any-flagp! ...) call."
  (and (listp form)
       (eq (car form) 'gear-with-any-flagp!)))

(defun backpack-inventory--extract-gearp-info (form)
  "Extract (POUCH GEAR FLAG) from a gearp! FORM.
FLAG may be nil if not present."
  (when (backpack-inventory--gearp-call-p form)
    (let ((pouch (nth 1 form))
          (gear  (nth 2 form))
          (flag  (nth 3 form)))
      (list pouch gear flag))))

(defun backpack-inventory--plist-get-values (plist key)
  "Get all consecutive values for KEY from PLIST.
Leaf keyword args can have multiple values before the next keyword.
Returns a list of values."
  (let (values collecting)
    (dolist (item plist)
      (cond
       (collecting
        (if (keywordp item)
            (setq collecting nil)
          (push item values)))
       ((eq item key)
        (setq collecting t))))
    (nreverse values)))

(defun backpack-inventory--extract-leaf-keyword (props keyword)
  "Extract the value(s) following KEYWORD in leaf PROPS.
Returns a list of values found after the keyword, stopping at the
next keyword."
  (backpack-inventory--plist-get-values props keyword))

(defun backpack-inventory--extract-doctor-entries (props)
  "Extract :doctor entries from leaf PROPS.
Returns a list of (BINARY . DESCRIPTION) pairs."
  (let ((values (backpack-inventory--extract-leaf-keyword props :doctor)))
    ;; :doctor entries are cons cells like (\"binary\" . \"description\")
    (cl-remove-if-not #'consp values)))

(defun backpack-inventory--extract-font-entries (props)
  "Extract :fonts entries from leaf PROPS.
Returns a list of (FONT-NAME . DESCRIPTION) pairs."
  (let ((values (backpack-inventory--extract-leaf-keyword props :fonts)))
    (cl-remove-if-not #'consp values)))

(defun backpack-inventory--analyze-gate (form)
  "Analyze a :when or :unless gate expression FORM.
Returns a list of gearp-info entries: ((POUCH GEAR FLAG) ...)."
  (cond
   ;; Direct gearp! call
   ((backpack-inventory--gearp-call-p form)
    (list (backpack-inventory--extract-gearp-info form)))

   ;; (not (gearp! ...))
   ((and (listp form) (eq (car form) 'not)
         (backpack-inventory--gearp-call-p (cadr form)))
    (list (backpack-inventory--extract-gearp-info (cadr form))))

   ;; (and ...) or (or ...)
   ((and (listp form) (memq (car form) '(and or)))
    (cl-mapcan #'backpack-inventory--analyze-gate (cdr form)))

   ;; gear-with-any-flagp!
   ((backpack-inventory--gear-with-any-flagp-call-p form)
    (let ((pouch (nth 1 form))
          (gear  (nth 2 form))
          (flags (cddr (cdr form))))
      (mapcar (lambda (f) (list pouch gear f)) flags)))

   (t nil)))

(defun backpack-inventory--flag-default-on-p (flag-symbol)
  "Return non-nil if FLAG-SYMBOL represents a default-on opt-out flag.
Flags prefixed with `-' represent features that are on by default
and must be explicitly disabled."
  (and flag-symbol
       (string-prefix-p "-" (symbol-name flag-symbol))))

(defun backpack-inventory--detect-default-on (form context)
  "Detect if FORM in CONTEXT (:when or :unless) indicates a default-on gear.
Returns non-nil for default-on gears."
  (cond
   ;; :unless (gearp! :pouch -gear) => default-on
   ((and (eq context :unless)
         (backpack-inventory--gearp-call-p form))
    (let ((gear (nth 2 form)))
      (backpack-inventory--flag-default-on-p gear)))

   ;; :unless (or (gearp! :pouch -gear) ...) => default-on
   ((and (eq context :unless)
         (listp form) (eq (car form) 'or))
    ;; Check if any branch has a -prefixed gear
    (cl-some (lambda (branch)
               (and (backpack-inventory--gearp-call-p branch)
                    (backpack-inventory--flag-default-on-p (nth 2 branch))))
             (cdr form)))

   (t nil)))

(defun backpack-inventory--proper-list-p (form)
  "Return non-nil if FORM is a proper (non-dotted) list."
  (and (listp form)
       (listp (cdr form))
       (condition-case nil
           (progn (ignore (length form)) t)
         (wrong-type-argument nil))))

(defun backpack-inventory--collect-gearp-calls (form)
  "Recursively collect all gearp! calls from FORM.
Returns a list of (CONTEXT GEARP-INFO) where CONTEXT is :when or :unless
based on the surrounding when/unless/not context."
  (cond
   ((not (consp form)) nil)
   ((not (backpack-inventory--proper-list-p form)) nil)
   ((null form) nil)

   ;; (gearp! ...) -- standalone, no clear context
   ((backpack-inventory--gearp-call-p form)
    (list (list :bare (backpack-inventory--extract-gearp-info form))))

   ;; (when (gearp! ...) ...) or (when (and/or ...) ...)
   ((and (memq (car form) '(when if)) (cdr form))
    (let ((condition (cadr form))
          (body (cddr form)))
      (append
       (backpack-inventory--collect-gearp-from-condition condition :when)
       (cl-mapcan #'backpack-inventory--collect-gearp-calls body))))

   ;; (unless (gearp! ...) ...)
   ((and (eq (car form) 'unless) (cdr form))
    (let ((condition (cadr form))
          (body (cddr form)))
      (append
       (backpack-inventory--collect-gearp-from-condition condition :unless)
       (cl-mapcan #'backpack-inventory--collect-gearp-calls body))))

   ;; Any other proper list -- recurse into elements
   (t
    (cl-mapcan #'backpack-inventory--collect-gearp-calls form))))

(defun backpack-inventory--collect-gearp-from-condition (condition context)
  "Collect gearp! calls from CONDITION with CONTEXT (:when or :unless).
Handles (not ...) by flipping context, and (and/or ...) by recursing."
  (cond
   ((backpack-inventory--gearp-call-p condition)
    (list (list context (backpack-inventory--extract-gearp-info condition))))

   ((and (listp condition) (eq (car condition) 'not) (cadr condition))
    (let ((flipped (if (eq context :when) :unless :when)))
      (backpack-inventory--collect-gearp-from-condition (cadr condition) flipped)))

   ((and (listp condition) (memq (car condition) '(and or)))
    (cl-mapcan (lambda (sub) (backpack-inventory--collect-gearp-from-condition sub context))
               (cdr condition)))

   ((backpack-inventory--gear-with-any-flagp-call-p condition)
    (let ((pouch (nth 1 condition))
          (gear  (nth 2 condition))
          (flags (cddr (cdr condition))))
      (mapcar (lambda (f) (list context (list pouch gear f))) flags)))

   (t nil)))

(defun backpack-inventory--parse-leaf-form (form pouch-keyword)
  "Parse a leaf FORM and extract gear metadata for POUCH-KEYWORD.
Returns a list of gear-info plists, or nil if the form isn't relevant."
  (when (and (listp form) (eq (car form) 'leaf))
    (let* ((leaf-name (nth 1 form))
           (props (cddr form))
           (doc (car (backpack-inventory--extract-leaf-keyword props :doc)))
           (doctors (backpack-inventory--extract-doctor-entries props))
           (fonts (backpack-inventory--extract-font-entries props))
           (when-val (car (backpack-inventory--extract-leaf-keyword props :when)))
           (unless-val (car (backpack-inventory--extract-leaf-keyword props :unless)))
           (gate-form (or when-val unless-val))
           (gate-context (cond (when-val :when) (unless-val :unless)))
           (gate-infos (when gate-form
                         (backpack-inventory--analyze-gate gate-form)))
           results)

      ;; Process the :when/:unless gate to find gear/flag definitions
      (dolist (info gate-infos)
        (let ((g-pouch (nth 0 info))
              (g-gear  (nth 1 info))
              (g-flag  (nth 2 info)))
          (when (eq g-pouch pouch-keyword)
            (cond
             ;; 2-arg gearp! with a -prefixed gear name in :unless => default-on gear
             ((and (null g-flag)
                   (eq gate-context :unless)
                   (backpack-inventory--flag-default-on-p g-gear))
              (let ((real-name (intern (substring (symbol-name g-gear) 1))))
                (push (list :type :gear
                            :name real-name
                            :leaf-name leaf-name
                            :doc doc
                            :default-on t
                            :doctors doctors
                            :fonts fonts)
                      results)))

             ;; 2-arg gearp! in :when => opt-in gear
             ((and (null g-flag) (eq gate-context :when))
              (push (list :type :gear
                          :name g-gear
                          :leaf-name leaf-name
                          :doc doc
                          :default-on nil
                          :doctors doctors
                          :fonts fonts)
                    results))

             ;; 3-arg gearp! => this is a flag on a gear
             (g-flag
              (let ((default-on (and (eq gate-context :unless)
                                     (backpack-inventory--flag-default-on-p g-flag))))
                (push (list :type :flag
                            :gear g-gear
                            :name g-flag
                            :leaf-name leaf-name
                            :doc doc
                            :default-on default-on
                            :doctors doctors
                            :fonts fonts)
                      results)))))))

      ;; Recursively scan the entire leaf body for gearp! calls that
      ;; represent flags.  This catches patterns inside :config, :init,
      ;; :hook lambdas, etc.
      (let ((all-calls (backpack-inventory--collect-gearp-calls props))
            seen-flags)
        (dolist (call-entry all-calls)
          (let* ((ctx  (nth 0 call-entry))
                 (info (nth 1 call-entry))
                 (g-pouch (nth 0 info))
                 (g-gear  (nth 1 info))
                 (g-flag  (nth 2 info))
                 (flag-key (when g-flag (cons g-gear g-flag))))
            ;; Only 3-arg calls (flags) in our pouch, skip duplicates
            (when (and (eq g-pouch pouch-keyword)
                       g-flag
                       (not (member flag-key seen-flags))
                       ;; Don't duplicate flags already found in :when/:unless gate
                       (not (cl-find g-flag results
                                     :key (lambda (r) (plist-get r :name)))))
              (push flag-key seen-flags)
              (let ((default-on
                     (cond
                      ;; (unless (gearp! :p g -flag) ...) => default-on
                      ((and (eq ctx :unless)
                            (backpack-inventory--flag-default-on-p g-flag))
                       t)
                      ;; (when (not (gearp! :p g -flag)) ...) => same
                      ;; This is handled by context flipping in collect-gearp-from-condition
                      ;; so :unless context with -flag means default-on
                      (t nil))))
                (push (list :type :flag
                            :gear g-gear
                            :name g-flag
                            :leaf-name nil
                            :doc nil
                            :default-on default-on
                            :doctors nil
                            :fonts nil)
                      results))))))

      ;; Recursively process nested leaf blocks in body sections
      (dolist (section '(:config :init :preface))
        (let ((body (backpack-inventory--extract-leaf-keyword props section)))
          (dolist (sub-form body)
            (when (and (listp sub-form) (eq (car sub-form) 'leaf))
              (let ((sub-results (backpack-inventory--parse-leaf-form sub-form pouch-keyword)))
                (setq results (append results sub-results)))))))

      results)))

(defun backpack-inventory--parse-bare-form (form pouch-keyword)
  "Parse a bare (when ...) or (unless ...) FORM for flag information.
Returns a list of flag-info plists for POUCH-KEYWORD.
Uses context-aware collection that handles (not ...) context flipping."
  (when (and (listp form) (memq (car form) '(when unless)))
    (let* ((context (if (eq (car form) 'when) :when :unless))
           (condition (cadr form))
           (call-entries (backpack-inventory--collect-gearp-from-condition condition context))
           results)
      (dolist (call-entry call-entries)
        (let* ((ctx  (nth 0 call-entry))
               (info (nth 1 call-entry))
               (g-pouch (nth 0 info))
               (g-gear  (nth 1 info))
               (g-flag  (nth 2 info)))
          ;; Only interested in 3-arg gearp! calls (flags) in our pouch
          (when (and (eq g-pouch pouch-keyword) g-flag)
            (let ((default-on (and (eq ctx :unless)
                                   (backpack-inventory--flag-default-on-p g-flag))))
              (push (list :type :flag
                          :gear g-gear
                          :name g-flag
                          :leaf-name nil
                          :doc nil
                          :default-on default-on
                          :doctors nil
                          :fonts nil)
                    results)))))
      results)))

(defun backpack-inventory--infer-gear-from-file (filepath forms _pouch-keyword)
  "Infer a gear entry from FILEPATH when no gearp! call defines one.
FORMS are the parsed S-expressions, POUCH-KEYWORD is the pouch.
This handles files like nerd-icons-completion.el where the :unless
gate references other gears rather than the gear being defined.
Returns a gear-info plist or nil."
  (let ((file-stem (file-name-sans-extension (file-name-nondirectory filepath))))
    ;; Find the first leaf block in the file
    (cl-loop for form in forms
             when (and (listp form) (eq (car form) 'leaf))
             return (let* ((props (cddr form))
                           (doc (car (backpack-inventory--extract-leaf-keyword props :doc)))
                           (doctors (backpack-inventory--extract-doctor-entries props))
                           (fonts (backpack-inventory--extract-font-entries props))
                           (unless-val (car (backpack-inventory--extract-leaf-keyword props :unless)))
                           ;; Check if this is a default-on gear (has :unless)
                           (default-on (and unless-val t)))
                      (list :type :gear
                            :name (intern file-stem)
                            :leaf-name (nth 1 form)
                            :doc doc
                            :default-on default-on
                            :doctors doctors
                            :fonts fonts)))))

(defun backpack-inventory--parse-file (filepath pouch-keyword)
  "Parse a gear FILEPATH belonging to POUCH-KEYWORD.
Returns a list of parsed entries (gear-info and flag-info plists)."
  (let ((forms (backpack-inventory--read-file-forms filepath))
        results
        found-gear-p)
    (dolist (form forms)
      (cond
       ;; leaf block
       ((and (listp form) (eq (car form) 'leaf))
        (let ((leaf-results (backpack-inventory--parse-leaf-form form pouch-keyword)))
          (when (cl-some (lambda (r) (eq (plist-get r :type) :gear)) leaf-results)
            (setq found-gear-p t))
          (setq results (append results leaf-results))))

       ;; Bare when/unless at top level
       ((and (listp form) (memq (car form) '(when unless)))
        (let ((bare-results (backpack-inventory--parse-bare-form form pouch-keyword)))
          (setq results (append results bare-results)))
        ;; Also check for leaf blocks inside when/unless body
        (dolist (body-form (cddr form))
          (when (and (listp body-form) (eq (car body-form) 'leaf))
            (let ((leaf-results (backpack-inventory--parse-leaf-form body-form pouch-keyword)))
              (when (cl-some (lambda (r) (eq (plist-get r :type) :gear)) leaf-results)
                (setq found-gear-p t))
              (setq results (append results leaf-results))))))))

    ;; If no gear entry was created from gearp! calls, infer one from the file
    (unless found-gear-p
      (let ((inferred (backpack-inventory--infer-gear-from-file filepath forms pouch-keyword)))
        (when inferred
          (push inferred results))))

    results))

(defun backpack-inventory--better-doc-p (new-entry existing-gear)
  "Return non-nil if NEW-ENTRY has a better doc than EXISTING-GEAR.
Prefer doc from a leaf whose leaf-name matches the gear name."
  (let ((new-doc (plist-get new-entry :doc))
        (existing-doc (plist-get existing-gear :doc))
        (gear-name (plist-get existing-gear :name))
        (new-leaf-name (plist-get new-entry :leaf-name)))
    (and new-doc
         (or (null existing-doc)
             ;; Prefer doc from leaf whose name matches the gear
             (eq new-leaf-name gear-name)))))

(defun backpack-inventory--merge-gear-entries (entries source-file)
  "Merge a list of parsed ENTRIES from SOURCE-FILE into gear structures.
Returns a list of gear plists."
  (let ((gears (make-hash-table :test 'eq)))
    ;; First pass: collect gear definitions
    (dolist (entry entries)
      (when (eq (plist-get entry :type) :gear)
        (let* ((name (plist-get entry :name))
               (existing (gethash name gears)))
          (if existing
              ;; Merge: prefer better doc, accumulate doctors/fonts
              (progn
                (when (backpack-inventory--better-doc-p entry existing)
                  (plist-put existing :doc (plist-get entry :doc)))
                ;; If existing has no default-on but new does, upgrade
                (when (and (plist-get entry :default-on)
                           (not (plist-get existing :default-on)))
                  (plist-put existing :default-on t))
                (plist-put existing :doctors
                           (cl-remove-duplicates
                            (append (plist-get existing :doctors)
                                    (plist-get entry :doctors))
                            :key #'car :test #'string=))
                (plist-put existing :fonts
                           (cl-remove-duplicates
                            (append (plist-get existing :fonts)
                                    (plist-get entry :fonts))
                            :key #'car :test #'string=)))
            ;; New gear
            (puthash name
                     (list :name name
                           :doc (plist-get entry :doc)
                           :default-on (plist-get entry :default-on)
                           :flags nil
                           :doctors (copy-sequence (plist-get entry :doctors))
                           :fonts (copy-sequence (plist-get entry :fonts))
                           :source-file source-file)
                     gears)))))

    ;; Second pass: attach flags to their gears
    (dolist (entry entries)
      (when (eq (plist-get entry :type) :flag)
        (let* ((gear-name (plist-get entry :gear))
               (gear (gethash gear-name gears))
               (flag-name (plist-get entry :name)))
          ;; If the gear doesn't exist yet (e.g., flag found before gear definition),
          ;; create a placeholder gear entry
          (unless gear
            (setq gear (list :name gear-name
                             :doc nil
                             :default-on nil
                             :flags nil
                             :doctors nil
                             :fonts nil
                             :source-file source-file))
            (puthash gear-name gear gears))

          ;; Add flag if not already present
          (let ((existing-flags (plist-get gear :flags)))
            (unless (cl-find flag-name existing-flags
                             :key (lambda (f) (plist-get f :name)))
              (plist-put gear :flags
                         (append existing-flags
                                 (list (list :name flag-name
                                             :doc (plist-get entry :doc)
                                             :default-on (plist-get entry :default-on)
                                             :doctors (plist-get entry :doctors)
                                             :fonts (plist-get entry :fonts))))))
            ;; Merge flag's doctors/fonts up to the gear level
            (plist-put gear :doctors
                       (cl-remove-duplicates
                        (append (plist-get gear :doctors)
                                (plist-get entry :doctors))
                        :key #'car :test #'string=))
            (plist-put gear :fonts
                       (cl-remove-duplicates
                        (append (plist-get gear :fonts)
                                (plist-get entry :fonts))
                        :key #'car :test #'string=))))))

    ;; Convert hash-table to alist
    (let (result)
      (maphash (lambda (_k v) (push v result)) gears)
      ;; Sort by gear name
      (sort result (lambda (a b)
                     (string< (symbol-name (plist-get a :name))
                              (symbol-name (plist-get b :name))))))))

;;; Registry builder

(defun backpack-inventory--build-registry ()
  "Scan all gear files and build the full registry.
Returns an alist of (POUCH-KEYWORD . GEARS-LIST)."
  (let ((pouches (backpack-inventory--discover-pouches))
        registry)
    (dolist (pouch-entry pouches)
      (let* ((pouch-kw (car pouch-entry))
             (pouch-dir (cdr pouch-entry))
             (gear-files (backpack-inventory--discover-gear-files pouch-dir))
             all-entries)
        ;; Parse all files in this pouch
        (dolist (filepath gear-files)
          (let* ((entries (backpack-inventory--parse-file filepath pouch-kw))
                 (merged (backpack-inventory--merge-gear-entries entries filepath)))
            (setq all-entries (append all-entries merged))))

        ;; Deduplicate gears that appear across multiple files in the same pouch.
        ;; When a gear appears from multiple files (e.g., `org` from both
        ;; org.el and haskell.el due to cross-gear references), prefer
        ;; the entry that has a source file matching the gear name, or
        ;; the entry with more information (doc, flags, etc.).
        (let ((gear-table (make-hash-table :test 'eq)))
          (dolist (gear all-entries)
            (let* ((name (plist-get gear :name))
                   (existing (gethash name gear-table)))
              (if (not existing)
                  (puthash name gear gear-table)
                ;; Decide which entry to keep
                (let* ((existing-file (file-name-sans-extension
                                       (file-name-nondirectory
                                        (or (plist-get existing :source-file) ""))))
                       (new-file (file-name-sans-extension
                                  (file-name-nondirectory
                                   (or (plist-get gear :source-file) ""))))
                       (name-str (symbol-name name))
                       (existing-matches (string= existing-file name-str))
                       (new-matches (string= new-file name-str)))
                                     (cond
                   ;; New file matches gear name but existing doesn't => replace
                   ;; but still merge data from existing into the new entry
                   ((and new-matches (not existing-matches))
                    (plist-put gear :flags
                               (cl-remove-duplicates
                                (append (plist-get gear :flags)
                                        (plist-get existing :flags))
                                :key (lambda (f) (plist-get f :name))))
                    (plist-put gear :doctors
                               (cl-remove-duplicates
                                (append (plist-get gear :doctors)
                                        (plist-get existing :doctors))
                                :key #'car :test #'string=))
                    (plist-put gear :fonts
                               (cl-remove-duplicates
                                (append (plist-get gear :fonts)
                                        (plist-get existing :fonts))
                                :key #'car :test #'string=))
                    (puthash name gear gear-table))
                   ;; Existing matches, keep it but merge flags/doctors/fonts from new
                   (existing-matches
                    (plist-put existing :flags
                               (cl-remove-duplicates
                                (append (plist-get existing :flags)
                                        (plist-get gear :flags))
                                :key (lambda (f) (plist-get f :name))))
                    (plist-put existing :doctors
                               (cl-remove-duplicates
                                (append (plist-get existing :doctors)
                                        (plist-get gear :doctors))
                                :key #'car :test #'string=))
                    (plist-put existing :fonts
                               (cl-remove-duplicates
                                (append (plist-get existing :fonts)
                                        (plist-get gear :fonts))
                                :key #'car :test #'string=)))
                   ;; Neither matches -- prefer the one with more info
                   ((and (plist-get gear :doc)
                         (not (plist-get existing :doc)))
                    (plist-put gear :flags
                               (cl-remove-duplicates
                                (append (plist-get gear :flags)
                                        (plist-get existing :flags))
                                :key (lambda (f) (plist-get f :name))))
                    (plist-put gear :doctors
                               (cl-remove-duplicates
                                (append (plist-get gear :doctors)
                                        (plist-get existing :doctors))
                                :key #'car :test #'string=))
                    (plist-put gear :fonts
                               (cl-remove-duplicates
                                (append (plist-get gear :fonts)
                                        (plist-get existing :fonts))
                                :key #'car :test #'string=))
                    (puthash name gear gear-table))
                   ;; Otherwise keep existing, merge all data
                   (t
                    (plist-put existing :flags
                               (cl-remove-duplicates
                                (append (plist-get existing :flags)
                                        (plist-get gear :flags))
                                :key (lambda (f) (plist-get f :name))))
                    (plist-put existing :doctors
                               (cl-remove-duplicates
                                (append (plist-get existing :doctors)
                                        (plist-get gear :doctors))
                                :key #'car :test #'string=))
                    (plist-put existing :fonts
                               (cl-remove-duplicates
                                (append (plist-get existing :fonts)
                                        (plist-get gear :fonts))
                                :key #'car :test #'string=))))))))
          ;; Collect and sort
          (let (deduped)
            (maphash (lambda (_k v) (push v deduped)) gear-table)
            (push (cons pouch-kw
                        (sort deduped
                              (lambda (a b)
                                (string< (symbol-name (plist-get a :name))
                                         (symbol-name (plist-get b :name))))))
                  registry)))))
    (nreverse registry)))

;;; Gear status determination

(defun backpack-inventory--gear-status (pouch-keyword gear-plist)
  "Determine the display status of a gear.
POUCH-KEYWORD is the pouch, GEAR-PLIST is the gear data.
Returns one of: :enabled, :disabled, :default-on."
  (let ((name (plist-get gear-plist :name))
        (default-on (plist-get gear-plist :default-on)))
    (if default-on
        ;; Default-on gear: check if user explicitly disabled it
        (let ((neg-name (intern (concat "-" (symbol-name name)))))
          (cond
           ((backpack--gearp!-impl pouch-keyword neg-name) :disabled)
           ;; If user explicitly listed the gear (even though it's default-on),
           ;; show as enabled rather than default
           ((backpack--gearp!-impl pouch-keyword name) :enabled)
           (t :default-on)))
      ;; Opt-in gear: check if user enabled it
      (if (backpack--gearp!-impl pouch-keyword name)
          :enabled
        :disabled))))

(defun backpack-inventory--flag-status (pouch-keyword gear-name flag-plist)
  "Determine the display status of a flag.
POUCH-KEYWORD is the pouch, GEAR-NAME is the gear, FLAG-PLIST is the flag data.
Returns one of: :enabled, :disabled, :default-on."
  (let* ((flag-name (plist-get flag-plist :name))
         (default-on (plist-get flag-plist :default-on)))
    (if default-on
        ;; Default-on flag: on unless user explicitly set it
        (if (backpack--gearp!-impl pouch-keyword gear-name flag-name)
            :disabled
          :default-on)
      ;; Opt-in flag
      (if (backpack--gearp!-impl pouch-keyword gear-name flag-name)
          :enabled
        :disabled))))

;;; Flag description generation

(defun backpack-inventory--flag-description (flag-plist)
  "Generate a description for FLAG-PLIST.
Uses the :doc if available, otherwise auto-generates from the flag name."
  (or (plist-get flag-plist :doc)
      (let* ((name (symbol-name (plist-get flag-plist :name)))
             (is-negative (string-prefix-p "-" name))
             (clean-name (if is-negative (substring name 1) name)))
        (if is-negative
            (format "disable %s (on by default)" clean-name)
          (format "enable %s" clean-name)))))

;;; Rendering helpers

(defun backpack-inventory--insert-header (text)
  "Insert TEXT as a header with underline."
  (insert (propertize text 'face 'info-title-2) "\n")
  (insert (make-string (length text) ?=) "\n\n"))

(defun backpack-inventory--insert-subheader (text)
  "Insert TEXT as a sub-header with underline."
  (insert (propertize text 'face 'info-title-3) "\n")
  (insert (make-string (length text) ?-) "\n\n"))

(defun backpack-inventory--status-string (status)
  "Return a display string for STATUS keyword."
  (pcase status
    (:enabled    (propertize "[*]" 'face 'success))
    (:disabled   (propertize "[ ]" 'face 'shadow))
    (:default-on (propertize "[D]" 'face 'warning))))

(defun backpack-inventory--insert-nav-hint (text)
  "Insert navigation hint TEXT in a subtle face."
  (insert "\n" (propertize text 'face 'shadow) "\n"))

;;; Pouch listing renderer (top-level view)

(defun backpack-inventory--render-pouches ()
  "Render the top-level pouch listing."
  (let ((registry backpack-inventory--registry)
        (inhibit-read-only t))
    (erase-buffer)
    (backpack-inventory--insert-header "Backpack Inventory")
    (insert "Browse the pouches, gears and flags available in your backpack.\n\n")
    (insert (propertize "Pouches:" 'face 'bold) "\n\n")

    (let ((max-name-len
           (cl-reduce #'max registry
                      :key (lambda (entry)
                             (length (symbol-name (car entry))))
                      :initial-value 0)))
      (dolist (pouch-entry registry)
        (let* ((pouch-kw (car pouch-entry))
               (gears (cdr pouch-entry))
               (gear-count (length gears))
               (desc (alist-get pouch-kw backpack-inventory--pouch-descriptions))
               (name-str (symbol-name pouch-kw))
               (padding (make-string (- max-name-len (length name-str) -2) ?\s))
               (count-str (format "(%d %s)" gear-count
                                  (if (= gear-count 1) "gear" "gears")))
               (line-start (point)))
          (insert "  " (propertize name-str 'face 'font-lock-keyword-face)
                  padding count-str)
          (when desc
            (insert "  " (propertize (concat "-- " desc) 'face 'font-lock-comment-face)))
          (insert "\n")
          (put-text-property line-start (point)
                             'backpack-inventory-item
                             (list :type :pouch :keyword pouch-kw)))))

    (backpack-inventory--insert-nav-hint
     "Press RET on a pouch to see its gears.  Press q to quit.")))

;;; Gear listing renderer (per-pouch view)

(defun backpack-inventory--render-gears (pouch-keyword)
  "Render the gear listing for POUCH-KEYWORD."
  (let* ((gears (alist-get pouch-keyword backpack-inventory--registry))
         (inhibit-read-only t))
    (erase-buffer)
    (backpack-inventory--insert-subheader (symbol-name pouch-keyword))

    (let ((desc (alist-get pouch-keyword backpack-inventory--pouch-descriptions)))
      (when desc
        (insert desc "\n\n")))

    (insert (propertize "Gears:" 'face 'bold) "\n\n")

    (if (null gears)
        (insert "  (no gears found)\n")
      (let ((max-name-len
             (cl-reduce #'max gears
                        :key (lambda (g) (length (symbol-name (plist-get g :name))))
                        :initial-value 0)))
        (dolist (gear gears)
          (let* ((name (plist-get gear :name))
                 (doc (plist-get gear :doc))
                 (status (backpack-inventory--gear-status pouch-keyword gear))
                 (status-str (backpack-inventory--status-string status))
                 (name-str (symbol-name name))
                 (padding (make-string (- max-name-len (length name-str) -1) ?\s))
                 (line-start (point)))
            (insert "  " status-str " "
                    (propertize name-str 'face 'font-lock-function-name-face)
                    padding)
            (when doc
              (insert (propertize (concat "-- " doc) 'face 'font-lock-comment-face)))
            (insert "\n")
            (put-text-property line-start (point)
                               'backpack-inventory-item
                               (list :type :gear
                                     :pouch pouch-keyword
                                     :data gear))))))

    (insert "\n")
    (insert (propertize "[*]" 'face 'success) " = enabled  "
            (propertize "[ ]" 'face 'shadow)  " = disabled  "
            (propertize "[D]" 'face 'warning) " = on by default\n")

    (backpack-inventory--insert-nav-hint
     "Press RET on a gear for details.  Press l or DEL to go back.")))

;;; Gear detail renderer

(defun backpack-inventory--theme-flags-p (pouch-keyword gear-name)
  "Return non-nil if POUCH-KEYWORD / GEAR-NAME is the theme gear."
  (and (eq pouch-keyword :ui) (eq gear-name 'theme)))

(defun backpack-inventory--render-theme-flags (flags pouch-keyword gear-name)
  "Render FLAGS for the theme gear with collapsing.
POUCH-KEYWORD and GEAR-NAME are used for status checks."
  (let (builtin-flags doom-flags)
    (dolist (flag flags)
      (let ((fname (symbol-name (plist-get flag :name))))
        (if (string-prefix-p "doom-" fname)
            (push flag doom-flags)
          (push flag builtin-flags))))
    (setq builtin-flags (nreverse builtin-flags))
    (setq doom-flags (nreverse doom-flags))

    (when builtin-flags
      (insert "\n  " (propertize (format "Builtin themes (%d):" (length builtin-flags))
                                 'face 'bold)
              "\n")
      (let ((names (mapcar (lambda (f)
                             (let* ((status (backpack-inventory--flag-status
                                            pouch-keyword gear-name f))
                                    (name (symbol-name (plist-get f :name)))
                                    (face (if (eq status :enabled)
                                              'success
                                            'shadow)))
                               (propertize name 'face face)))
                           builtin-flags)))
        (insert "    " (string-join names ", ") "\n")))

    (when doom-flags
      (insert "\n  " (propertize (format "Doom themes (%d):" (length doom-flags))
                                 'face 'bold)
              "\n")
      (let ((names (mapcar (lambda (f)
                             (let* ((status (backpack-inventory--flag-status
                                            pouch-keyword gear-name f))
                                    (name (symbol-name (plist-get f :name)))
                                    (face (if (eq status :enabled)
                                              'success
                                            'shadow)))
                               (propertize name 'face face)))
                           doom-flags)))
        ;; Wrap at ~70 chars
        (let ((line "    ")
              (col 4))
          (dolist (name names)
            (let ((name-len (+ (length (substring-no-properties name)) 2)))
              (when (and (> col 4) (> (+ col name-len) 76))
                (insert line "\n")
                (setq line "    " col 4))
              (setq line (concat line (if (> col 4) ", " "") name))
              (setq col (+ col name-len))))
          (insert line "\n"))))))

(defun backpack-inventory--render-gear-detail (pouch-keyword gear-plist)
  "Render the full detail view for GEAR-PLIST in POUCH-KEYWORD."
  (let* ((name (plist-get gear-plist :name))
         (doc (plist-get gear-plist :doc))
         (flags (plist-get gear-plist :flags))
         (doctors (plist-get gear-plist :doctors))
         (fonts (plist-get gear-plist :fonts))
         (source-file (plist-get gear-plist :source-file))
         (status (backpack-inventory--gear-status pouch-keyword gear-plist))
         (inhibit-read-only t))
    (erase-buffer)

    ;; Breadcrumb
    (backpack-inventory--insert-subheader
     (format "%s / %s" (symbol-name pouch-keyword) (symbol-name name)))

    ;; Description
    (when doc
      (insert doc "\n\n"))

    ;; Status
    (insert (propertize "Status: " 'face 'bold)
            (pcase status
              (:enabled    (propertize "enabled" 'face 'success))
              (:disabled   (propertize "disabled" 'face 'shadow))
              (:default-on (propertize "on by default" 'face 'warning)))
            "\n")

    ;; Source file (relative path)
    (when source-file
      (let ((rel-path (file-relative-name source-file
                                          (expand-file-name ".." backpack-core-dir))))
        (insert (propertize "Source: " 'face 'bold)
                (propertize rel-path 'face 'font-lock-string-face)
                "\n")))

    ;; Flags
    (when flags
      (insert "\n" (propertize "Flags:" 'face 'bold) "\n")
      (if (backpack-inventory--theme-flags-p pouch-keyword name)
          (backpack-inventory--render-theme-flags flags pouch-keyword name)
        (dolist (flag flags)
          (let* ((flag-name (plist-get flag :name))
                 (flag-desc (backpack-inventory--flag-description flag))
                 (flag-status (backpack-inventory--flag-status
                               pouch-keyword name flag))
                 (status-str (backpack-inventory--status-string flag-status)))
            (insert "  " status-str " "
                    (propertize (symbol-name flag-name)
                                'face 'font-lock-variable-name-face))
            (when flag-desc
              (insert "  "
                      (propertize (concat "-- " flag-desc)
                                  'face 'font-lock-comment-face)))
            (insert "\n")))))

    ;; External tools
    (when doctors
      (insert "\n" (propertize "External tools:" 'face 'bold) "\n")
      (dolist (doc-entry doctors)
        (insert "  " (propertize (car doc-entry) 'face 'font-lock-constant-face)
                "  "
                (propertize (concat "-- " (cdr doc-entry))
                            'face 'font-lock-comment-face)
                "\n")))

    ;; Fonts
    (when fonts
      (insert "\n" (propertize "Required fonts:" 'face 'bold) "\n")
      (dolist (font-entry fonts)
        (insert "  " (propertize (car font-entry) 'face 'font-lock-constant-face)
                "  "
                (propertize (concat "-- " (cdr font-entry))
                            'face 'font-lock-comment-face)
                "\n")))

    ;; Example gear! snippet
    (insert "\n" (propertize "Example:" 'face 'bold) "\n")
    (let* ((opt-in-flags (cl-remove-if
                          (lambda (f)
                            (backpack-inventory--flag-default-on-p
                             (plist-get f :name)))
                          flags))
           (flag-names (mapcar (lambda (f) (symbol-name (plist-get f :name)))
                               opt-in-flags))
           ;; For theme gear, show a brief example instead of all flags
           (flag-names (if (backpack-inventory--theme-flags-p pouch-keyword name)
                           '("doom-one")
                         flag-names)))
      (insert (propertize "  (gear!\n" 'face 'font-lock-doc-face))
      (insert (propertize (format "    %s\n" (symbol-name pouch-keyword))
                          'face 'font-lock-doc-face))
      (if flag-names
          (insert (propertize
                   (format "    (%s %s))\n" (symbol-name name)
                           (string-join flag-names " "))
                   'face 'font-lock-doc-face))
        (insert (propertize (format "    %s)\n" (symbol-name name))
                            'face 'font-lock-doc-face))))

    (insert "\n")
    (insert (propertize "[*]" 'face 'success) " = enabled  "
            (propertize "[ ]" 'face 'shadow)  " = disabled  "
            (propertize "[D]" 'face 'warning) " = on by default\n")

    (backpack-inventory--insert-nav-hint
     "Press l or DEL to go back.")))

;;; Navigation

(defun backpack-inventory--navigate-to (view-type view-data &optional push-history)
  "Navigate to a view of VIEW-TYPE with VIEW-DATA.
If PUSH-HISTORY is non-nil, push the current view onto the history stack."
  (when (and push-history backpack-inventory--current-view)
    (push backpack-inventory--current-view backpack-inventory--history))
  (setq backpack-inventory--current-view (cons view-type view-data))
  (pcase view-type
    (:pouches
     (backpack-inventory--render-pouches))
    (:gears
     (backpack-inventory--render-gears view-data))
    (:gear-detail
     (let ((pouch-kw (plist-get view-data :pouch))
           (gear-data (plist-get view-data :data)))
       (backpack-inventory--render-gear-detail pouch-kw gear-data))))
  (goto-char (point-min)))

(defun backpack-inventory-enter ()
  "Enter the item at point (drill into a pouch or gear)."
  (interactive)
  (let ((item (get-text-property (line-beginning-position) 'backpack-inventory-item)))
    (when item
      (pcase (plist-get item :type)
        (:pouch
         (backpack-inventory--navigate-to
          :gears (plist-get item :keyword) t))
        (:gear
         (backpack-inventory--navigate-to
          :gear-detail (list :pouch (plist-get item :pouch)
                             :data (plist-get item :data))
          t))))))

(defun backpack-inventory-back ()
  "Go back to the previous view."
  (interactive)
  (if backpack-inventory--history
      (let ((prev (pop backpack-inventory--history)))
        (setq backpack-inventory--current-view prev)
        (pcase (car prev)
          (:pouches
           (backpack-inventory--render-pouches))
          (:gears
           (backpack-inventory--render-gears (cdr prev)))
          (:gear-detail
           (let ((data (cdr prev)))
             (backpack-inventory--render-gear-detail
              (plist-get data :pouch)
              (plist-get data :data)))))
        (goto-char (point-min)))
    (message "Already at the top level.")))

(defun backpack-inventory-refresh ()
  "Re-scan gear files and refresh the current view."
  (interactive)
  (setq backpack-inventory--registry (backpack-inventory--build-registry))
  ;; Re-render current view
  (when backpack-inventory--current-view
    (pcase (car backpack-inventory--current-view)
      (:pouches
       (backpack-inventory--render-pouches))
      (:gears
       (backpack-inventory--render-gears (cdr backpack-inventory--current-view)))
      (:gear-detail
       (let ((data (cdr backpack-inventory--current-view)))
         (backpack-inventory--render-gear-detail
          (plist-get data :pouch)
          (plist-get data :data)))))
    (goto-char (point-min))))

;;; Entry point

;;;###autoload
(defun backpack-inventory ()
  "Browse the Backpack inventory of available pouches, gears and flags.

Opens a navigable buffer showing all pouches (categories), their
gears (features) and flags (options).  Press RET to drill into a
pouch or gear, and l or DEL to go back."
  (interactive)
  (let ((buf (get-buffer-create backpack-inventory--buffer-name)))
    (with-current-buffer buf
      (backpack-inventory-mode)
      (setq backpack-inventory--registry (backpack-inventory--build-registry))
      (setq backpack-inventory--history nil)
      (backpack-inventory--navigate-to :pouches nil))
    (switch-to-buffer buf)))

(provide 'backpack-inventory)
