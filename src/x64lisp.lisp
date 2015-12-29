
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
  ((text :initarg :text :reader error.text)))

(define-condition size-of-sizeless-type (error)
  ())

(define-condition alignment-of-sizeless-type (error)
  ())

(defclass asm-module ()
  ((procs :initarg :procs
          :accessor asm-module.procs)
   (name :initarg :name
         :accessor asm-module.name)))

(defclass asm-proc ()
  ((toplevel-forms :initform (make-array 0 :adjustable t :fill-pointer 0)
                   :accessor asm-proc.toplevel-forms)
   (instrs :initform (make-array 0 :adjustable t :fill-pointer 0)
           :accessor asm-proc.instrs)
   (name :initarg :name
         :reader asm-proc.name)
   (thunk :initarg :thunk
          :reader asm-proc.thunk)))

(defun asm-proc.push-instr (instr proc)
    (vector-push-extend instr (asm-proc.instrs proc)))

(defun asm-proc.push-form (form proc)
    (vector-push-extend form (asm-proc.toplevel-forms proc)))

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

(defun require-not-toplevel (construct-name)
    (when *is-toplevel*
        (error 'unexpected-scoped-form
               :text (format nil "~a declarations are required to appear in local scope" construct-name))))

(defun load-file (filename)
    (load filename))

(defun load-files (filenames)
    (loop for file in filenames do
         (load-file file)
         (with-open-file (stream (concatenate 'string file ".s") :direction :output :if-exists :supersede)
             (format stream "hi there lol")
             (loop for proc in (asm-module.procs *current-module*) do
                  (funcall (asm-proc.thunk proc))

                  (format t "Procedure ~a~%" (asm-proc.name proc))

                  (map nil (bind #'format t "~a~%") (asm-proc.instrs proc))
                  ;; process (ASM-PROC.INSTRS PROC) here
                  ))
         (setf *current-module* nil)))
