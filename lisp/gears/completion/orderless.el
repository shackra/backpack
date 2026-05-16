(leaf orderless
  :doc "an orderless «completion style» that divides the pattern into space-separated components, and matches candidates that match all of the components in any order"
  :unless (gearp! :completion -orderless)
  :ensure (orderless :ref "9cf1c90e2501566ceba59f3220b4630995004efd")
  :custom
  (completion-styles . '(orderless basic))
  (completion-category-overrides . '((file (styles partial-completion orderless))))
  (completion-category-defaults . nil) ;; Disable defaults, use orderless settings
  (orderless-matching-styles . '(orderless-literal
				 orderless-prefixes
				 orderless-initialism
				 orderless-regexp)))

(leaf orderless
  :unless (gearp! :completion -orderless)
  :emacs>= '31
  :custom
  (completion-pcm-leading-wildcard . t)) ;; partial-completion behaves like substring

(leaf orderless
  :doc "style dispatchers for affix-based matching.
@ matches against marginalia annotations, ~ triggers flex,
` triggers initialism, ! negates.
Credit: dispatcher functions adapted from chiply/.zetta.d"
  :unless (gearp! :completion -orderless)
  :init
  (defun backpack-orderless-flex-dispatcher (pattern _index _total)
    "Match flexibly when PATTERN has ~ prefix or suffix."
    (cond
     ((string-prefix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1)))))

  (defun backpack-orderless-annotation-dispatcher (pattern _index _total)
    "Match against marginalia annotations when PATTERN has @ prefix or suffix."
    (let ((rest (cond
		 ((string-prefix-p "@" pattern) (substring pattern 1))
		 ((string-suffix-p "@" pattern) (substring pattern 0 -1)))))
      (when rest
	`(orderless-annotation . ,rest))))

  (defun backpack-orderless-initialism-dispatcher (pattern _index _total)
    "Match as initialism when PATTERN has ` prefix or suffix."
    (cond
     ((string-prefix-p "`" pattern)
      `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern)
      `(orderless-initialism . ,(substring pattern 0 -1)))))

  (defun backpack-orderless-without-dispatcher (pattern _index _total)
    "Negate match when PATTERN has ! prefix or suffix."
    (cond
     ((equal "!" pattern) '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))
     ((string-suffix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 0 -1)))))

  :custom
  (orderless-component-separator . ",")
  (orderless-style-dispatchers . '(backpack-orderless-initialism-dispatcher
				    backpack-orderless-annotation-dispatcher
				    backpack-orderless-flex-dispatcher
				    backpack-orderless-without-dispatcher)))
