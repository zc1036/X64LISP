
;;;; Stuff pertaining to the abstract syntax tree

(in-package :ast)

(defclass ast-expr ()
  ((type :initarg :type
         :reader ast-expr.type)))

(defmethod ast-expr.type ((x integer))
    (loop for type in integral-types
       do
         (when (typep x (int-type.lisp-typespec type))
             (return type))
       finally
         (error 'asm-type-error :text (format nil "Lisp integer ~a not representable in 64 bits" x))))

;; AST-EXPR.TO-INSTRUCTIONS is idempotent for INSTR (defined in
;; instructions.lisp); for other types, it must return a list of
;; INSTR.
(defgeneric ast-expr.to-instructions (ast-expr)
    (:documentation "Converts an AST expression to instructions"))

(deftype expression-like ()
    '(or ast-expr integer))

(defmacro defstatement (name lambda-list &body body)
    (assert (typep name 'symbol))

    (with-gensyms (instr-sym args-sym)
        (let ((class-name (intern (concatenate 'string "STATEMENT-CLASS." (symbol-name name)))))
            `(progn
                 (defclass ,class-name (ast-expr)
                   ((type :initform void)
                    (to-instructions-thunk :initarg :to-instructions-thunk
                                           :reader statement-class.to-instructions-thunk)))
                 (defmethod ast-expr.to-instructions ((,instr-sym ,class-name))
                     (funcall (statement-class.to-instructions-thunk ,instr-sym)))
                 (defmacro ,name (&rest ,args-sym)
                     `(make-instance ,'',class-name
                                     ;; Create a function that executes the body of the macro
                                     :to-instructions-thunk (lambda ()
                                                                (destructuring-bind ,',lambda-list (list ,@,args-sym)
                                                                    ,'(progn ,@body)))))))))
