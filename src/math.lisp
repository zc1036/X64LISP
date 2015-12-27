
;;;; Math functions

(defun ceil-to-nearest-multiple (n multiple-of)
    (+ n (mod (- multiple-of (mod n multiple-of)) multiple-of)))
