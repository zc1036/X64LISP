
;;;; Defines the core declarative forms for x64lisp

(in-package :core-forms)

(defun process-struct-field-decl (decl)
    (destructuring-bind (member-name member-type) decl
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
                                                     (mapcar #'ast-expr.to-instructions (list ,@body)))))))
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

(defstatement while (condition &body body)
    (with-backtrace-guard (format nil "WHILE ~a" condition)
        (require-procedure :while)

        (type-assert (ast-expr.type condition) 'int-type)

        body))
