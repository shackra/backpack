(require 'backpack-pouch)

(leaf ws-butler
  :unless (gearp! :tools -whitespaces)
  :doc "unobtrusive way to trim whitespace"
  :ensure (ws-butler :ref "9ee5a7657a22e836618813c2e2b64a548d27d2ff")
  :global-minor-mode ws-butler-global-mode
  :custom
  (ws-butler-keep-whitespace-before-point . nil))
