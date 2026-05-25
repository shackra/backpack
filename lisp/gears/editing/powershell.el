;; -*- lexical-binding: t; -*-

(leaf powershell-mode
  :doc "Major mode for editing PowerShell scripts"
  :when (gearp! :editing powershell)
  :ensure (powershell :ref "ae60e11c96cc1767f05ce0cab6a917240ce2e37a")
  :doctor ("pwsh" . ("PowerShell 7+ cross-platform shell" optional))
  :hook
  (powershell-mode-hook .
   (lambda ()
     (unless (gearp! :editing powershell -display-line-numbers)
       (display-line-numbers-mode +1)))))
