(leaf diff-hl
  :doc "VCS change indicators in the fringe"
  :when (gearp! :ui diff-hl)
  :ensure (diff-hl :ref "7d873b2f58908de1ea2f499da9bf993e088953d7")
  :hook
  (dired-mode-hook . diff-hl-dired-mode)
  :global-minor-mode global-diff-hl-mode diff-hl-flydiff-mode
  :config
  ;; Magit integration: refresh fringe after magit operations
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(leaf diff-hl-amend
  :doc "Diff against the parent of HEAD instead of HEAD itself"
  :when (gearp! :ui diff-hl amend)
  :after diff-hl
  :global-minor-mode diff-hl-amend-mode)

(leaf diff-hl-margin
  :doc "Use margin indicators in terminal Emacs (no fringe available)"
  :when (and (gearp! :ui diff-hl)
             (not (display-graphic-p)))
  :after diff-hl
  :global-minor-mode diff-hl-margin-mode)

(when (gearp! :ui diff-hl)
  (with-eval-after-load 'transient
    (transient-define-prefix backpack-diff-hl-menu ()
      "Diff-HL commands for the current project."
      ["Navigation"
       ("n" "Next hunk" diff-hl-next-hunk :transient t)
       ("p" "Previous hunk" diff-hl-previous-hunk :transient t)
       ("h" "Show hunk" diff-hl-show-hunk)]
      ["Reference revision"
       ("r" "Set revision" diff-hl-set-reference-rev-in-project)
       ("R" "Reset to HEAD" diff-hl-reset-reference-rev-in-project)]
      ["Amend"
       ("a" "Toggle amend mode" diff-hl-amend-mode)]))
  ;; Ensure transient is loaded before first invocation
  (unless (fboundp 'backpack-diff-hl-menu)
    (defun backpack-diff-hl-menu ()
      "Diff-HL commands for the current project.
Stub that loads transient on first call, then re-dispatches."
      (interactive)
      (require 'transient)
      (call-interactively #'backpack-diff-hl-menu)))
  (with-eval-after-load 'project
    (keymap-set project-prefix-map "R" #'backpack-diff-hl-menu)))
