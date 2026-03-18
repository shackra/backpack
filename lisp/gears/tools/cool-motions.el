(require 'backpack-pouch)

(leaf cool-motions
  :unless (gearp! :tools -cool-motions)
  :doc "Smart motions: C-a cycles between first non-whitespace character and beginning of line"
  :tag "builtin"
  :preface
  (defun backpack-move-beginning-of-line ()
    "Move point to the first non-whitespace character on the line.
     If point is already there, move to column 0. If point is at column 0,
     move back to the first non-whitespace character."
    (interactive "^")
    (let ((orig-point (point))
          (first-non-ws
           (save-excursion
             (back-to-indentation)
             (point)))
          (bol
           (line-beginning-position)))
      (cond
       ;; If we're at the first non-whitespace character, go to column 0
       ((= orig-point first-non-ws)
        (move-beginning-of-line 1))
       ;; If we're at column 0, go to first non-whitespace character
       ((= orig-point bol)
        (back-to-indentation))
       ;; Otherwise, go to first non-whitespace character
       (t
        (back-to-indentation)))))
  :bind
  ([remap move-beginning-of-line] . backpack-move-beginning-of-line))
