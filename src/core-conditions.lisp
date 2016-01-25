
;;;; Conditions that can occur in the core forms

(in-package :core-conditions)

(define-condition assignment-to-non-lvalue (assembly-error)
  ())

(define-condition malformed-struct-definition (assembly-error)
  ())
