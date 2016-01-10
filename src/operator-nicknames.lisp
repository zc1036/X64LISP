
;;;; Nicknames for commonly used operators that would conflict with
;;;; the exports of other packages

(in-package :operator-nicknames)

(defmacro define-forwarding-function (name forward-to)
    (with-gensyms (args-sym)
        `(defun ,name (&rest ,args-sym)
             (apply #',forward-to ,args-sym))))

(define-forwarding-function + core-forms:operator+)
