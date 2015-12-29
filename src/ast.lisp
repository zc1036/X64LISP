
;;;; Stuff pertaining to the abstract syntax tree

(in-package :ast)

(defclass ast-expr ()
  ((type :initarg :type
         :reader ast-expr.type)))

;; AST-EXPR.TO-INSTRUCTIONS is idempotent for INSTR (defined in
;; instructions.lisp); for other types, it must return an array of
;; INSTR.
(defgeneric ast-expr.to-instructions (ast-expr)
    (:documentation "Converts an AST expression to instructions"))

(defmacro defstatement (name lambda-list &body body)
    (assert (typep name 'symbol))

    (with-gensyms (x-sym args-sym)
        (let ((class-name (intern (concatenate 'string "STATEMENT-CLASS." (symbol-name name)))))
            `(progn
                 (defclass ,class-name (ast-expr)
                   ((type :initform void)
                    (to-instructions-thunk :initarg :to-instructions-thunk
                                           :reader statement-class.to-instructions-thunk)))
                 (defmethod ast-expr.to-instructions ((,x-sym ,class-name))
                     (funcall (statement-class.to-instructions-thunk ,x-sym)))
                 (defmacro ,name (&rest ,args-sym)
                     ;; Unfortunately I can't make nested backquotes work here, so
                     ;; I'm having to resort to using LIST
                     (list make-instance '',class-name
                           ;; Create a function that executes the body of the macro
                           :to-instructions-thunk (append '(bind (lambda ,lambda-list ,@body))
                                                          ,args-sym)))))))
