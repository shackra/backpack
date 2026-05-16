;; -*- lexical-binding: t; -*-

(leaf activities
  :when (gearp! :tools activities)
  :doc "Manages activities (workspaces) as sets of window configurations, buffers, and tabs."
  :ensure (activities :ref "a7e7842c615e149ad7d7e57f383936b49dcb129f")
  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b"   . activities-switch-buffer)
   ("C-x C-a g"   . activities-revert)
   ("C-x C-a l"   . activities-list))
  :custom
  (activities-bookmark-store . t)
  ;; Kill buffers unique to a tab when suspending an activity.
  ;; Opt out: (gear! ... :tools (activities -kill-buffers) ...)
  (activities-kill-buffers . t)
  :global-minor-mode activities-mode
  :config
  ;; Respect opt-out flag for kill-buffers
  (when (gearp! :tools activities -kill-buffers)
    (setq activities-kill-buffers nil))

  ;; Use tab-bar integration by default; opt out with the -tabs flag:
  ;;   (gear! ... :tools (activities -tabs) ...)
  (unless (gearp! :tools activities -tabs)
    (activities-tabs-mode 1))

  ;; Consult integration: add an "Activity Buffer" source so
  ;; `consult-buffer' (C-x b) shows activity-scoped buffers first.
  (with-eval-after-load 'consult
    (defvar backpack-consult--source-activity-buffer
      `(:name "Activity Buffer"
        :narrow ?a
        :category buffer
        :face consult-buffer
        :state ,#'consult--buffer-state
        :items
        ,(lambda ()
           (when-let* ((tab (and (bound-and-true-p activities-tabs-mode)
                                 (tab-bar--current-tab-find)))
                       (bufs (alist-get 'activities-buffer-list (cdr tab))))
             (mapcar #'buffer-name
                     (cl-remove-if-not #'buffer-live-p bufs)))))
      "Consult source for buffers in the current activity.")
    (add-to-list 'consult-buffer-sources 'backpack-consult--source-activity-buffer))
  )

