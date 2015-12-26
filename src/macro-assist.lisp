
;;;; macro tools

(defmacro with-gensyms (sym-names &body body)
    `(let ,(map 'list (lambda (s) (list s '(gensym))))
         ,@body))
