
;;;; Some basic data structures

(in-package :data-structures)

(defstruct (list-builder (:conc-name :list-builder.))
  "A structure for efficiently building lists from beginning to end."
  head tail)

(defun list-builder.push-end (builder item)
    (with-slots (head tail) builder
        (if head
            (progn
                (setf (cdr tail) (cons item nil))
                (setf tail (cdr tail)))
            (progn
                (setf head (cons item nil))
                (setf tail head)))))

(defun list-builder.push-begin (builder item)
    (with-slots (head tail) builder
        (setf head (cons item head))
        (unless tail
            (setf tail head))))
