
;;;; Stuff pertaining to the abstract syntax tree

(in-package :ast)

(defclass ast-expr ()
  ())

(defstruct (instr-result (:conc-name :instr-result.))
  "A structure resulting from call to AST-EXPR.TO-INSTRUCTIONS"
  (instrs nil :type (or instructions:instr list) :read-only t)
  (type nil :type btype :read-only t)
  (reg nil :type instructions:reg :read-only t))

(defgeneric ast-expr.to-instructions (ast-expr)
    (:documentation
"Takes an AST expression and returns an INSTR-RESULT.

If the type of the expression is VOID, then the register is NIL.

A list of expressions is taken to mean a compound expression of type
VOID."))

(defmethod ast-expr.to-instructions ((l list))
    (make-instr-result :instrs (mapcar (compose #'instr-result.instrs #'ast-expr.to-instructions) l)
                       :type void))

(defun integer-type (x)
    (loop for type in integral-types
       do
         (when (typep x (int-type.lisp-typespec type))
             (return type))
       finally
         (error 'asm-type-error :text (format nil "Lisp integer ~a not representable in 64 bits" x))))

(defmethod ast-expr.to-instructions ((x integer))
    (let* ((int-type (integer-type x))
           (reg (instructions:new-vreg (btype.size int-type))))
        (make-instr-result :instrs (instructions:mov reg x)
                           :type int-type
                           :reg reg)))

(deftype expression-like ()
    '(or ast-expr integer))

(defmacro def-form (name lambda-list &body body)
"   Creates a new form with the given NAME.

    This defines a macro called NAME and a class called
    STATEMENT-CLASS.[NAME], which class is instantiated upon
    invocation of the macro. The AST-EXPR.TO-INSTRUCTIONS method of
    the class will invoke the body of the DEFSTATEMENT form with the
    evaluated arguments given to the macro (the arguments are only
    evaluated at that time).

    The body of the macro must evaluate to values suitable to return
    from an implementation of AST-EXPR.TO-INSTRUCTIONS."
    (assert (typep name 'symbol))

    (with-gensyms (instr-sym args-sym)
        (let ((class-name (intern (concatenate 'string "STATEMENT-CLASS." (symbol-name name))
                                  (symbol-package name))))
            `(progn
                 (defclass ,class-name (ast-expr)
                   ((to-instructions-thunk :initarg :to-instructions-thunk
                                           :reader statement-class.to-instructions-thunk)))
                 (defmethod ast-expr.to-instructions ((,instr-sym ,class-name))
                     (funcall (statement-class.to-instructions-thunk ,instr-sym)))
                 (defmacro ,name (&rest ,args-sym)
                     `(make-instance
                       ,'',class-name
                       ;; Create a function that executes the body of the macro
                       :to-instructions-thunk (lambda ()
                                                  (destructuring-bind ,',lambda-list (list ,@,args-sym)
                                                      ,'(progn ,@body)))))))))

(defun make-generic-form-class-name (symbol)
    (intern (concatenate 'string "GENERIC-FORM-CLASS." (symbol-name symbol))
            (symbol-package symbol)))

(defun make-generic-form-generic-name (symbol)
    (intern (concatenate 'string "GENERIC-FORM-FUNC." (symbol-name symbol))
            (symbol-package symbol)))

(defmacro def-generic-form (name arg-list)
    (let ((class-name (make-generic-form-class-name name))
          (generic-name (make-generic-form-generic-name name))
          (args-sym (gensym)))
        `(progn
             (defclass ,class-name (ast-expr)
               ((to-instructions-thunk :initarg :to-instructions-thunk
                                       :reader generic-form.to-instructions-thunk)))
             (defgeneric ,generic-name ,arg-list)
             (defmacro ,name (&rest ,args-sym)
                 (with-gensyms (evald-args-sym method-sym)
                     `(make-instance
                       ,'',class-name
                       :to-instructions-thunk (lambda ()
                                                  (let* ((,evald-args-sym (list ,@,args-sym))
                                                         (,method-sym (apply ,'#',generic-name
                                                                             (mapcar #'ast-expr.type ,evald-args-sym))))
                                                      (assert (typep ,method-sym 'function))
                                                      (apply ,method-sym ,evald-args-sym)))))))))

(defmacro def-generic-instance (name arg-list &body body)
    (let ((generic-name (make-generic-form-generic-name name))
          (arg-names (mapcar (lambda (x) (etypecase x (cons (car x)) (atom x))) arg-list)))
        `(defmethod ,generic-name ,arg-list
             (lambda ,arg-names 
                 ,@body))))
