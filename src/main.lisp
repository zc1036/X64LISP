
(defpackage :x64lisp-driver
  (:use :cl))

(in-package :x64lisp-driver)

(declaim (optimize (debug 3) (safety 3) (speed 0)))

(require :asdf)

(asdf:load-system :unix-opts)
(asdf:operate 'asdf:load-source-op :x64lisp)

(opts:define-opts)

(defun print-error (e)
    (loop for bt in (conditions:assembly-error.backtrace-reports e) do
         (format *error-output* "In ~a~%" (conditions:backtrace-report.form-name bt)))
    
    (format *error-output* "Error assembling file: ~a~%" (conditions:assembly-error.text e))

    (values))

(multiple-value-bind (opts args) (opts:get-opts)
    (declare (ignore opts))

    (let ((*package* (find-package :x64lisp-user))
          (*readtable* source-readtable:x64lisp-readtable))
        (handler-case (x64lisp:load-files args)
          (conditions:assembly-error (e) (print-error e)))))
