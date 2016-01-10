
;;;; Structures representing core source-level constructs

(in-package :core-structures)

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
