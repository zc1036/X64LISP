
;;;; Nicknames for commonly used operators that would conflict with
;;;; the exports of other packages

(in-package :operator-nicknames)

(defmacro define-forwarding-macro (name forward-to)
    (with-gensyms (args-sym)
        `(defmacro ,name (&rest ,args-sym)
             (cons ',forward-to ,args-sym))))

(define-forwarding-macro + core-forms:operator+)
(define-forwarding-macro = core-forms:operator=)
(define-forwarding-macro let core-forms:let-var)
