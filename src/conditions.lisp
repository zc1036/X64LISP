
;;;; Some conditions usable from any part of the compiler

(in-package :conditions)

(define-condition assembly-error (error)
  ((text :initarg :text :reader assembly-error.text)
   (backtrace-reports :initform nil
                      :accessor assembly-error.backtrace-reports)))

(define-condition internal-compiler-error (error)
  ((text :initarg :text :reader internal-compiler-error.text)))

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
