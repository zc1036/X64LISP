
;;;; Stuff pertaining to the abstract syntax tree

(in-package :ast)

(defclass ast-expr ()
  ((type :initarg :type
         :reader ast-expr.type)))
