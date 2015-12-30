
;;;; Functional utilities

(in-package :functional)

;; map-accum :: init-state :: a ->
;;              f :: (a, b) -> (a, c),
;;              list :: [b] -> (a, [c])
(defun map-accum (init-state f list)
    (map-accum* init-state f list nil))

(defun map-accum* (state f list accum)
    (if list
        (multiple-value-bind (new-state list-item) (funcall f state (car list))
            (map-accum* new-state f (cdr list) (cons list-item accum)))
        (values state (nreverse accum))))

(defmacro bind (func &rest args)
    `(lambda (&rest restargs)
         (apply ,func (append (list ,@args) restargs))))

(defmacro destructuring-lambda (lambda-list &body body)
    (with-gensyms (args-sym)
        `(lambda (&rest ,args-sym)
             (destructuring-bind ,lambda-list ,args-sym
                 ,@body))))

(defun flatten (list &optional accum resume)
    "Tail-recursive list flatten; removes NILs."
    (declare (optimize (debug 0) (safety 0) (speed 3)))
    (if list
        (ctypecase (car list)
          (null (flatten (cdr list) accum resume))
          (atom (flatten (cdr list) (cons (car list) accum) resume))
          (cons (flatten (car list) accum (cons (cdr list) resume))))
        (if resume
            (flatten (car resume) accum (cdr resume))
            (reverse accum))))
