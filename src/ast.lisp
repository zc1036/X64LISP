
;;;; Stuff pertaining to the abstract syntax tree

(in-package :ast)

(defclass ast-expr ()
  ((type :initarg :type
         :reader ast-expr.type)
   (lvalue-p :initarg :lvalue-p
             :reader ast-expr.lvalue-p)))

(defstruct (instr-result (:conc-name :instr-result.))
  "A structure resulting from call to AST-EXPR.TO-INSTRUCTIONS"
  (instrs nil :read-only t) ;; A list of TAC:INSTR
  (reg nil :read-only t))

(defgeneric ast-expr.to-instructions (ast-expr)
    (:documentation
"Takes an AST expression and returns an INSTR-RESULT.

If the type of the expression is VOID, then the register is NIL.

A list of expressions is taken to mean a compound expression of type
VOID."))

(defmethod ast-expr.to-instructions ((l list))
    (make-instr-result :instrs (mapcar (compose #'instr-result.instrs #'ast-expr.to-instructions) l)))

(defun integer-type (x)
    (loop for type in integral-types
       do
         (when (typep x (int-type.lisp-typespec type))
             (return type))
       finally
         (error 'asm-type-error :text (format nil "Lisp integer ~a not representable in 64 bits" x))))

(defmethod ast-expr.to-instructions ((x integer))
    (let* ((int-type (integer-type x))
           (reg (tac:new-vreg (btype.size int-type))))
        (make-instr-result :instrs (tac:move reg x)
                           :reg reg)))

(defmethod ast-expr.lvalue-p ((x integer))
    (declare (ignore x))
    nil)

(defmethod ast-expr.type ((x integer))
    (integer-type x))

(deftype expression-like ()
    '(or ast-expr integer))

(defclass var (ast-expr)
  ((lvalue-p :initform t)
   (name :initarg :name
         :reader var.name
         :type symbol)
   (type :initarg :type
         :reader var.type
         :type btype)
   (reg :initarg :reg
        :reader var.reg
        :type tac:vreg)))

(defmethod initialize-instance :after ((v var) &key)
    (setf (slot-value v 'reg) (tac:new-vreg (btype.size (var.type v)))))

(defmethod ast-expr.to-instructions ((v var))
    (make-instr-result :reg (var.reg v)))

(defmacro def-form (name lambda-list form-type &body body)
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
                     (with-gensyms (evald-args-sym)
                         `(let* ((,evald-args-sym (list ,@,args-sym))
                                 (+declared-type+ (destructuring-bind ,',lambda-list ,evald-args-sym
                                                      ,',form-type)))
                              (make-instance
                               ,'',class-name
                               :lvalue-p nil
                               :type +declared-type+
                               ;; Create a function that executes the body of the macro
                               :to-instructions-thunk (lambda ()
                                                          (destructuring-bind ,',lambda-list ,evald-args-sym
                                                              ,'(progn ,@body)))))))))))

(defun make-generic-form-class-name (symbol)
    (intern (concatenate 'string "GENERIC-FORM-CLASS." (symbol-name symbol))
            (symbol-package symbol)))

(defun make-generic-form-generic-name (symbol)
    (intern (concatenate 'string "GENERIC-FORM-FUNC." (symbol-name symbol))
            (symbol-package symbol)))

(defun make-generic-form-typeof-name (symbol)
    (intern (concatenate 'string "GENERIC-FORM-TYPE." (symbol-name symbol))
            (symbol-package symbol)))

(defmacro def-generic-expr (name arg-list)
    (let ((class-name (make-generic-form-class-name name))
          (generic-name (make-generic-form-generic-name name))
          (generic-typeof-name (make-generic-form-typeof-name name))
          (args-sym (gensym))
          (instr-sym (gensym)))
        `(progn
             (defclass ,class-name (ast-expr)
               ((to-instructions-thunk :initarg :to-instructions-thunk
                                       :reader generic-form.to-instructions-thunk)))
             (defmethod ast-expr.to-instructions ((,instr-sym ,class-name))
                 (funcall (generic-form.to-instructions-thunk ,instr-sym)))
             (defgeneric ,generic-typeof-name ,arg-list)
             (defgeneric ,generic-name ,arg-list)
             (defmacro ,name (&rest ,args-sym)
                 (with-gensyms (evald-args-sym method-sym typeof-method-sym form-type-sym args-types-sym)
                     `(let* ((,evald-args-sym (list ,@,args-sym))
                             (,args-types-sym (mapcar #'ast-expr.type ,evald-args-sym))
                             ;; Only in lisp does a programmer get to
                             ;; use the ALIEN MONSTER WITH GAPING MAW emoji ,'#',
                             (,form-type-sym (let ((,typeof-method-sym 
                                                    (apply ,'#',generic-typeof-name ,args-types-sym)))
                                                 (assert (functionp ,typeof-method-sym))
                                                 (apply ,typeof-method-sym ,evald-args-sym))))
                          (make-instance
                           ,'',class-name
                           :lvalue-p nil
                           :type ,form-type-sym
                           :to-instructions-thunk
                           (lambda ()
                               (with-backtrace-guard ,,(symbol-name name)
                                   ;; then we call the generic with the types of the 
                                   ;; args to get the body defined with DEF-GENERIC-INSTANCE
                                   (let* ((,method-sym (apply ,'#',generic-name ,args-types-sym)))
                                       ;; and finally we invoke that body
                                       (assert (functionp ,method-sym))
                                       (apply ,method-sym (cons ,form-type-sym ,evald-args-sym))))))))))))

(defmacro def-expr-instance (name arg-list type-form &body body)
    (let ((generic-name (make-generic-form-generic-name name))
          (arg-names (mapcar (lambda (x) (etypecase x (cons (car x)) (atom x))) arg-list))
          (generic-typeof-name (make-generic-form-typeof-name name)))
        `(progn
             (defmethod ,generic-typeof-name ,arg-list
                 (lambda ,arg-names
                     ,type-form))
             (defmethod ,generic-name ,arg-list
                 (lambda ,(cons '+declared-type+ arg-names)
                     (declare (ignorable +declared-type+))
                     ,@body)))))

;; POSITION can be an argument to DEFMETHOD (i.e. :before, :after, :around) or
;; NIL in which case it's a normal method
(defmacro define-def-generic-form-aux (name position)
    `(defmacro ,name (name arg-list &body body)
         (let ((generic-name (make-generic-form-generic-name name))
               (arg-names (mapcar (lambda (x) (etypecase x (cons (car x)) (atom x))) arg-list)))
             `(defmethod ,generic-name ,,position ,arg-list
                         (lambda ,(cons '+declared-type+ arg-names)
                             (declare (ignorable +declared-type+))
                             ,@body)))))

(define-def-generic-form-aux def-expr-instance-before :before)
