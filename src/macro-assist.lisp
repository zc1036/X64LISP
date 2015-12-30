
;;;; macro tools

(in-package :macro-assist)

(defmacro with-gensyms (sym-names &body body)
    "Creates gensym-bound names"
    `(let ,(map 'list (lambda (s) (list s '(gensym))) sym-names)
         ,@body))
