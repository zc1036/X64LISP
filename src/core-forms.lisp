
;;;; Defines the core declarative forms for x64lisp

(in-package :core-forms)

(define-condition malformed-struct-definition (assembly-error)
  ())

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

(defmacro proc (proc-name args &body body)
    (process-proc-decl proc-name args body))

(def-form while (condition &body body)
    (require-procedure :while)

    (with-slots ((cond-instrs ast::instrs)
                 (cond-type ast::type)
                 (cond-reg ast::reg)) (ast-expr.to-instructions condition)
        (type-assert cond-type 'int-type)
        
        (with-labels ((!while-test "Test while condition") (!while-end "End while-loop"))
            (make-instr-result :instrs (list !while-test
                                             cond-instrs
                                             (tst cond-reg)
                                             (jz !while-end)
                                             (instr-result.instrs (ast-expr.to-instructions body))
                                             (jmp !while-test)
                                             !while-end)
                               :type void))))

(def-generic-expr binary-op+ (int-type int-type))

(def-expr-instance binary-op+ ((x int-type) (y int-type))
    (multiple-with-slots (((x-instrs ast::instrs) (x-type ast::type) (x-reg ast::reg) x)
                          ((y-instrs ast::instrs) (y-type ast::type) (y-reg ast::reg) y))
        (let* ((common-int-type (common-type x-type y-type))
               (out-reg (new-vreg (btype.size common-int-type))))
            (make-instr-result :instrs (list x-instrs
                                             y-instrs
                                             (def out-reg (add x-reg y-reg)))
                               :type common-int-type
                               :reg out-reg))))

(defun operator+-impl (args accum)
    (declare (optimize (debug 3) (safety 3) (speed 2)))

    (if args
        (operator+-impl (cdr args) (binary-op+ accum (car args)))
        accum))

(defun operator+ (&rest args)
    (unless args
        (error 'assembly-error :text "Too few arguments to + operator"))

    (operator+-impl (cdr args) (car args)))

(defun member-access (obj member)
    (with-slots ((instrs ast::instrs) (type ast::type) (reg ast::reg)) (ast-expr.to-instructions obj)
        (let ((field-type (struct-field-info.type (struct-type.field type member)))
              (field-offset (struct-type.field-offset obj member)))
            (mac reg field-offset (+ field-offset (btype.size field-type))))))

(defun process-member-access-list* (list accum)
    (declare (optimize (debug 3) (safety 3) (speed 2)))

    (if list
        (process-member-access-list* (cdr list) `(member-access ,accum ,(car list)))
        accum))

(defun process-member-access-list (list)
    ;; Make sure the list has length of at least 2
    (unless (and list (cdr list))
        (error 'assembly-error :text "Malformed member access: expected at least two items"))

    (process-member-access-list* (cddr list) `(member-access ,(car list) ,(cadr list))))

(defun read-member-access-list (char stream)
    (declare (ignore char))

    (process-member-access-list (read-delimited-list #\] stream t)))

(set-macro-character #\[ #'read-member-access-list nil source-readtable:readtable)
