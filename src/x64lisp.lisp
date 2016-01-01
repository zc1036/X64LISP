
;;;; A small lisp for writing x64 assembly in C-like style
;;;
;;; x64lisp supports at least the following:
;;; - Functions with variables
;;; - Arbitrary memory/register access
;;; - Structs, unions
;;; - While, for loops
;;; - Function pointers
;;; - Basic C integer types (int8/16/32/64)
;;; - Direct access to assembly instructions
;;; - And all the compile-time power that comes with lisp
;;;

(in-package :x64lisp)

(define-condition assembly-error (error)
  ((text :initarg :text :reader assembly-error.text)
   (backtrace-reports :initform nil
                      :accessor assembly-error.backtrace-reports)))

(defstruct (backtrace-report (:conc-name backtrace-report.))
  form-name)

(defmacro with-backtrace-guard (name &body body)
    (with-gensyms (e-sym)
        `(handler-case (progn ,@body)
           (assembly-error (,e-sym)
               ;; tack on our own backtrace info to the condition object
               (push (make-backtrace-report :form-name ,name) (assembly-error.backtrace-reports ,e-sym))
               ;; propagate the error upwards
               (error ,e-sym)))))

(define-condition size-of-sizeless-type (assembly-error)
  ())

(define-condition alignment-of-sizeless-type (assembly-error)
  ())

(defclass asm-module ()
  ((procs :initarg :procs
          :accessor asm-module.procs)
   (name :initarg :name
         :accessor asm-module.name)))

(defclass asm-proc ()
  ((instrs :accessor asm-proc.instrs)
   (name :initarg :name
         :reader asm-proc.name)
   (thunk :initarg :thunk
          :reader asm-proc.thunk)))

(defparameter *asm-modules* nil)
(defparameter *current-module* nil)
(defparameter *current-proc* nil)
(defparameter *is-toplevel* t)

(define-condition unexpected-toplevel-form (assembly-error)
  ())

(define-condition unexpected-scoped-form (assembly-error)
  ())

(defun require-toplevel (construct-name)
    (when (not *is-toplevel*)
        (error 'unexpected-toplevel-form
               :text (format nil "~a declarations are required to appear at global scope" construct-name))))

(defun require-toplevel-module (construct-name)
    (require-toplevel construct-name)

    (when (not *current-module*)
        (error 'unexpected-toplevel-form
               :text (format nil "~a declarations must be preceded by a module declaration" construct-name))))

(defun require-procedure (construct-name)
    (unless (and (not *is-toplevel*) *current-proc*)
        (error 'unexpected-scoped-form
               :text (format nil "~a declarations are required to appear in procedure scope" construct-name))))

(defun load-file (filename)
    (load filename))

(defun load-files (filenames)
    (loop for filename in filenames do
         (with-backtrace-guard (format nil "file ~a" filename)
             (load-file filename)
             (with-open-file (stream (concatenate 'string filename ".s") :direction :output :if-exists :supersede)
                 (loop for proc in (asm-module.procs *current-module*) do
                      (with-slots (instrs name thunk) proc
                          (setf instrs (flatten (funcall thunk)))

                          (format t "Procedure ~a instructions:~%" name)

                          (map nil (bind #'format t "    ~a~%") instrs))
                    ;; process (ASM-PROC.INSTRS PROC) here
                      ))
             (setf *current-module* nil))))
