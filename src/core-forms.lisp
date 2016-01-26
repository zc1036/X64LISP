
;;;; Defines the core declarative forms for x64lisp

(in-package :core-forms)

(defmacro evaluate-body-exprs (body &rest rest)
    `(list ,@body ,@rest))

(defmacro let-var (bindings &body body)
    (let (var-bindings init-exprs init-bindings)
        (loop for binding in bindings do
             (match binding
                 ((list* var-name var-type (or nil (cons var-value nil)))
                  (push `(,var-name (make-instance 'var :name ',var-name :type ,var-type))
                        var-bindings)
                  (when var-value
                      (with-gensyms (temp-name)
                          (push `(,temp-name ,var-value) init-bindings)
                          (push `(binary-op= ,var-name ,temp-name) init-exprs))))
                 (_ (error 'malformed-let-binding
                           :text (format nil "Malformed let binding: ~a" binding)))))
        ;; We have to have the outer LET so that we don't shadow
        ;; bindings with the inner LET that are used in the init
        ;; expressions.
        `(let ,init-bindings
             (let ,var-bindings
                 (evaluate-body-exprs ,init-exprs ,@body)))))

(defun invalid-unary-usage (arg)
    (declare (ignore arg))
    (error 'assembly-error
           :text "Expected at least two arguments, got one"))

(defmacro define-left-associative-nary-op (name binary-op unary-op)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
         (labels ((op-impl (args accum &optional (unary-p t))
                    (declare (optimize (debug 3) (safety 3) (speed 2)))

                    (if args
                        (op-impl (cdr args) (,binary-op accum (car args)) nil)
                        (if unary-p (,unary-op accum) accum))))
             (defun ,name (&rest args)
                 (unless args
                     (error 'assembly-error
                            :text (format nil "Too few arguments to ~a; expected at least one" ',name)))
                 (op-impl (cdr args) (car args))))))

(defmacro define-right-associative-nary-op (name binary-op unary-op)
    `(labels ((op-impl (first rest &optional (unary-p t))
                  (declare (optimize (debug 3) (safety 3) (speed 2)))

                  (if rest
                      (,binary-op first (op-impl (car rest) (cdr rest) nil))
                      (if unary-p (,unary-op first) first))))
         (defun ,name (&rest args)
             (unless args
                 (error 'assembly-error
                        :text (format nil "Too few arguments to ~a; expected at least one" ',name)))
             (op-impl (car args) (cdr args)))))

(def-generic-expr binary-op= (lhs rhs))

(def-expr-instance-before binary-op= (x y)
    (declare (ignore y))

    (unless (ast-expr.lvalue-p x)
        (error 'assignment-to-non-lvalue :text "Assignment to non-lvalue")))

(def-expr-instance binary-op= ((x int-type) (y int-type))
    (progn
        (the t y) ;; ignore unused Y
        (or (ast-expr.type x) y))

    (multiple-with-slots (((x-instrs ast::instrs) (x-reg ast::reg) (ast-expr.to-instructions x))
                          ((y-instrs ast::instrs) (y-reg ast::reg) (ast-expr.to-instructions y)))
        (make-instr-result :instrs (list x-instrs
                                         y-instrs
                                         (tac:move x-reg y-reg))
                           :reg x-reg)))

(define-right-associative-nary-op operator= binary-op= invalid-unary-usage)

(def-generic-expr binary-op+ (lhs rhs))

(def-expr-instance binary-op+ ((x int-type) (y int-type))
    (common-type (ast-expr.type x) (ast-expr.type y))

    (multiple-with-slots (((x-instrs ast::instrs) (x-reg ast::reg) (ast-expr.to-instructions x))
                          ((y-instrs ast::instrs) (y-reg ast::reg) (ast-expr.to-instructions y)))
        (let* ((out-reg (tac:new-vreg (btype.size +declared-type+))))
            (make-instr-result :instrs (list x-instrs
                                             y-instrs
                                             (tac:add out-reg x-reg y-reg))
                               :reg out-reg))))

(define-left-associative-nary-op operator+ binary-op+ identity)

(def-form binary-member-access (obj member)
    (struct-field-info.type (struct-type.field (ast-expr.type obj) member))

    (with-slots ((instrs ast::instrs) (reg ast::reg)) (ast-expr.to-instructions obj)
        (let* ((field-offset (struct-type.field-offset +declared-type+ member))
               (out-reg (tac:new-vreg (btype.size +declared-type+))))
            (make-instr-result :instrs (list instrs
                                             (tac:member-access
                                              out-reg reg field-offset (+ field-offset
                                                                          (btype.size +declared-type+))))
                               :reg out-reg))))

(define-left-associative-nary-op operator-member-access binary-member-access invalid-unary-usage)

(defun read-member-access-list (char stream)
    (declare (ignore char))

    (cons 'operator-member-access (read-delimited-list #\] stream t)))

(set-macro-character #\[ #'read-member-access-list nil source-readtable:x64lisp-readtable)

(defun process-struct-field-decl (decl)
    (destructuring-bind (member-name member-type) decl
        (unless (symbolp member-name)
            (error 'malformed-struct-definition
                   :text (format nil "expected symbol for field name, got ~a" member-name)))
        `(list ',member-name ,member-type)))

;; Like PROG1
(def-form sequence-1 (&body body)
    (if body
        (ast-expr.type (car body))
        void)

    (if body
        (let ((first-instrs (ast-expr.to-instructions (car body)))
              (rest-instrs (ast-expr.to-instructions (cdr body))))
            (make-instr-result :instrs (cons (instr-result.instrs first-instrs)
                                             (instr-result.instrs rest-instrs))
                               :reg (instr-result.reg first-instrs)))
        (make-instr-result)))

(defun make-struct (struct-type args)
    (let-var ((nameless-struct struct-type))
        (let ((init-exprs))
            ;; Fix this to error on unrecognized struct fields
            (loop for field in (struct-type.fields struct-type)
               for field-name = (struct-field-info.name field)
               for init-form = (getf args field-name)
               when init-form do
                 (push (binary-op= (binary-member-access nameless-struct field-name) init-form) init-exprs))
            init-exprs)))

(defun process-struct-decl (struct-name field-decls)
    `(progn
         (require-toplevel-module :struct)
         (defparameter ,struct-name
           (make-instance 'struct-type
                          :fields (list ,@(map 'list #'process-struct-field-decl field-decls))
                          :name ,(symbol-name struct-name)))
         (defun ,struct-name (&rest args)
             (make-struct ,struct-name args))))

(defun process-proc-decl (proc-name args body)
    (declare (ignore args))
    
    (with-gensyms (proc-sym evald-body-sym)
        `(progn
             (require-toplevel-module :proc)
             (defparameter ,proc-name nil)
             (defun ,proc-name (&rest args)
                 (declare (ignore args))
                 (error 'error :text "Not implemented"))
             ;; We separate the declarations and the assignment so
             ;; that the compiler is aware of the existence of the
             ;; symbols and doesn't give false warnings
             (let ((,proc-sym)
                   (,evald-body-sym (evaluate-body-exprs ,body)))
                 (setf ,proc-sym (make-instance
                                  'asm-proc
                                  :name ,(symbol-name proc-name)
                                  :thunk (lambda ()
                                             (let ((*is-toplevel* nil)
                                                   (*current-proc* ,proc-sym))
                                                 (with-backtrace-guard (format nil "procedure ~a" ',proc-name)
                                                     (ast-expr.to-instructions ,evald-body-sym))))))
                 (setf ,proc-name ,proc-sym)
                 (push ,proc-sym (asm-module.procs *current-module*))))))

(defmacro module (module-name)
    (require-toplevel :module)

    (when *current-module*
        (error 'unexpected-toplevel-form
               :text "Duplicate 'module' declaration; only one module per file allowed"))

    (setf *current-module* (make-instance 'asm-module
                                          :procs nil
                                          :name (symbol-name module-name)))
    (push *current-module* *asm-modules*)
    (values))

(defmacro struct (struct-name &body field-decls)
    (process-struct-decl struct-name field-decls))

(defmacro proc (proc-name args &body body)
    (process-proc-decl proc-name args body))

(def-form while (condition &body body)
    (progn
        (the t condition) (the t body) ;; ignore unused vars
        void)

    (require-procedure :while)

    (type-assert (ast-expr.type condition) 'int-type)

    (with-slots ((cond-instrs ast::instrs) (cond-reg ast::reg)) (ast-expr.to-instructions condition)
        (tac:with-labels ((!while-test "Test while condition") (!while-end "End while-loop"))
            (make-instr-result :instrs (list (tac:lbl !while-test)
                                             cond-instrs
                                             (tac:j-zero !while-end cond-reg)
                                             (instr-result.instrs (ast-expr.to-instructions body))
                                             (tac:jump !while-test)
                                             (tac:lbl !while-end))))))
