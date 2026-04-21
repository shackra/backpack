;; -*- lexical-binding: t; -*-

(defvar backpack--persistence-window-state nil
  "Saved window state from the last closed frame.
Cleared on daemon exit since it is an in-memory variable.")

(defvar backpack--persistence-saved-p nil
  "Non-nil when window state has been saved and not yet restored.")

(leaf persistence
  :doc "restore window layout when reopening emacsclient after closing the last frame"
  :unless (gearp! :config -persistence)
  :tag "config" "persistence"
  :config
  (add-hook 'delete-frame-functions
            (defun backpack--save-frame-state (frame)
              "Save window configuration when the last visible frame is closed."
              (when (= (length (visible-frame-list)) 2)
                (with-selected-frame frame
                  (setq backpack--persistence-window-state (window-state-get nil t)
                        backpack--persistence-saved-p t)))))

  (add-hook 'server-after-make-frame-hook
            (defun backpack--restore-frame-state ()
              "Restore saved window configuration on the first client frame."
              (when (and backpack--persistence-saved-p
                         backpack--persistence-window-state)
                (let ((window-restore-killed-buffer-windows
                       (lambda (windows &rest _)
                         (dolist (win windows)
                           (set-window-buffer win (get-buffer "*scratch*"))))))
                  (window-state-put backpack--persistence-window-state nil t))
                (setq backpack--persistence-window-state nil
                      backpack--persistence-saved-p nil)))))
