
;;;; Defines the core declarative forms for x64lisp

(in-package :core-forms)

(defmethod assembly-error.text ((s malformed-struct-definition))
    (format nil "Malformed struct defintion: ~a" (slot-value s 'conditions::text)))

(defun process-struct-field-decl (decl)
    (destructuring-bind (member-name member-type) decl
        (unless (symbolp member-name)
            (error 'malformed-struct-definition
                   :text (format nil "expected symbol for field name, got ~a" member-name)))
        `(list ',member-name ,member-type)))

(defun process-struct-decl (struct-name field-decls)
    `(progn
         (require-toplevel-module :struct)
         (defparameter ,struct-name
           (make-instance 'struct-type
                          :fields (list ,@(map 'list #'process-struct-field-decl field-decls))
                          :name ,(symbol-name struct-name)))))

(defun process-proc-decl (proc-name args body)
    (declare (ignore args))
    
    (with-gensyms (proc-sym)
        `(progn
             (require-toplevel-module :proc)
             (defparameter ,proc-name nil)
             (defun ,proc-name (&rest args)
                 (declare (ignore args))
                 (error 'error :text "Not implemented"))
             ;; We separate the declarations and the assignment so
             ;; that the compiler is aware of the existence of the
             ;; symbols and doesn't give false warnings
             (let ((,proc-sym))
                 (setf ,proc-sym (make-instance
                                  'asm-proc
                                  :name ,(symbol-name proc-name)
                                  :thunk (lambda ()
                                             (let ((*is-toplevel* nil)
                                                   (*current-proc* ,proc-sym))
                                                 (with-backtrace-guard (format nil "procedure ~a" ',proc-name)
                                                     (ast-expr.to-instructions (list ,@body)))))))
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

(defun make-struct (struct &rest args)
    (unless (typep struct 'struct-type)
        (error 'assembly-error :text (format nil "~a is not a struct" struct)))

    (let ((struct-reg (tac:new-vreg (btype.size struct))))
        (make-instr-result :instrs (tac:move struct-reg 0)
                           :type struct
                           :reg struct-reg)))

(defun read-struct-literal (char stream)
    (declare (ignore char))

    (cons 'make-struct (read-delimited-list #\} stream t)))

(set-macro-character #\{ #'read-struct-literal nil source-readtable:x64lisp-readtable)

(defmacro proc (proc-name args &body body)
    (process-proc-decl proc-name args body))

(def-form while (condition &body body)
    (require-procedure :while)

    (with-slots ((cond-instrs ast::instrs)
                 (cond-type ast::type)
                 (cond-reg ast::reg)) (ast-expr.to-instructions condition)
        (type-assert cond-type 'int-type)
        
        (tac:with-labels ((!while-test "Test while condition") (!while-end "End while-loop"))
            (make-instr-result :instrs (list (tac:lbl !while-test)
                                             cond-instrs
                                             (tac:j-zero !while-end cond-reg)
                                             (instr-result.instrs (ast-expr.to-instructions body))
                                             (tac:jump !while-test)
                                             (tac:lbl !while-end))
                               :type void))))

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

    (unless (instr-result.lvalue-p x)
        (error 'assignment-to-non-lvalue :text "Assignment to non-lvalue")))

(def-expr-instance binary-op= ((x int-type) (y int-type))
    (multiple-with-slots (((x-instrs ast::instrs) (x-reg ast::reg) (x-type ast::type) x)
                          ((y-instrs ast::instrs) (y-reg ast::reg) y))
        (make-instr-result :instrs (list x-instrs
                                         y-instrs
                                         (tac:move x-reg y-reg))
                           :type x-type
                           :reg x-reg)))

(define-right-associative-nary-op operator= binary-op= invalid-unary-usage)

(def-generic-expr binary-op+ (lhs rhs))

(def-expr-instance binary-op+ ((x int-type) (y int-type))
    (multiple-with-slots (((x-instrs ast::instrs) (x-type ast::type) (x-reg ast::reg) x)
                          ((y-instrs ast::instrs) (y-type ast::type) (y-reg ast::reg) y))
        (let* ((common-int-type (common-type x-type y-type))
               (out-reg (tac:new-vreg (btype.size common-int-type))))
            (make-instr-result :instrs (list x-instrs
                                             y-instrs
                                             (tac:add out-reg x-reg y-reg))
                               :type common-int-type
                               :reg out-reg))))

(define-left-associative-nary-op operator+ binary-op+ identity)

(def-form binary-member-access (obj member)
    (with-slots ((instrs ast::instrs) (type ast::type) (reg ast::reg)) (ast-expr.to-instructions obj)
        (let* ((field-type (struct-field-info.type (struct-type.field type member)))
               (field-offset (struct-type.field-offset obj member))
               (out-reg (tac:new-vreg (btype.size field-type))))
            (make-instr-result :instrs (list instrs
                                             (tac:member-access
                                              out-reg reg field-offset (+ field-offset
                                                                          (btype.size field-type))))
                               :type field-type
                               :reg out-reg))))

(define-left-associative-nary-op operator-member-access binary-member-access invalid-unary-usage)

(defun read-member-access-list (char stream)
    (declare (ignore char))

    (cons 'operator-member-access (read-delimited-list #\] stream t)))

(set-macro-character #\[ #'read-member-access-list nil source-readtable:x64lisp-readtable)
