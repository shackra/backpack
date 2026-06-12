;; -*- lexical-binding: t; -*-

(leaf eca
  :doc "Editor Code Assistant (ECA) integration for Emacs"
  :url "https://eca.dev"
  :when (gearp! :ai eca)
  :ensure (eca :host github :repo "editor-code-assistant/eca-emacs"
	       :ref "0e1c7b4e924d7d7d99720342e60483b6dda187a3"
	       :files ("*.el"))
  :doctor ("eca" . ("server binary" 'required)))
