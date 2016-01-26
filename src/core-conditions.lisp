
;;;; Conditions that can occur in the core forms

(in-package :core-conditions)

(define-condition assignment-to-non-lvalue (assembly-error)
  ())

(define-condition malformed-struct-definition (assembly-error)
  ())

(defmethod assembly-error.text ((s malformed-struct-definition))
    (format nil "Malformed struct defintion: ~a" (slot-value s 'conditions::text)))

(define-condition unexpected-toplevel-form (assembly-error)
  ())

(define-condition unexpected-scoped-form (assembly-error)
  ())

(define-condition malformed-let-binding (assembly-error)
  ())
