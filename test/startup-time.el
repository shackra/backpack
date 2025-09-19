(require 'ert)

(defun measure-emacs-startup ()
  (float-time (time-subtract after-init-time before-init-time)))

(ert-deftest temacs-startup-time ()
  :tags '(performance startup-time)
  (let ((load-time (measure-emacs-startup)))
    (should numberp load-time)
    (message "⏰ Emacs startup time: %.3f second(s)" load-time)
    (should t)))

(provide 'startup-time)
