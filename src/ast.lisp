
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
    (:documentation
"Takes an AST expression and returns two values, (1) a list of
instructions to compute the expression and (2) the register in which
the computed value of the expression is stored.

If the type of the expression is VOID, then (2) is NIL.

A list of expressions is taken to mean a compound expression of type
VOID, and therefore the return values in this case are (1) a possibly
nested list of instructions and (2) NIL."))

(defmethod ast-expr.to-instructions ((l list))
    (values (mapcar #'ast-expr.to-instructions l)
            nil))

(defmethod ast-expr.to-instructions ((x integer))
    (let ((reg (instructions:new-vreg)))
        (values (instructions:mov reg x)
                reg)))

(deftype expression-like ()
    '(or ast-expr integer))

(defmacro defstatement (name lambda-list &body body)
    "Creates a new statement with the given NAME.

    This defines a macro called NAME and a class called
    STATEMENT-CLASS.[NAME], which class is instantiated upon
    invocation of the macro. The AST-EXPR.TO-INSTRUCTIONS method of
    the class will invoke the body of the DEFSTATEMENT form with the
    evaluated arguments given to the macro (the arguments are only
    evaluated at that time)."
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
                                                                (values
                                                                 (destructuring-bind ,',lambda-list (list ,@,args-sym)
                                                                     ,'(progn ,@body))
                                                                 nil)))))))) ; nil because it's a statement,
                                                                             ; which evaluate to void

