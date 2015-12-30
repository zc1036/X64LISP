
(defpackage :x64lisp-driver
  (:use :cl))

(in-package :x64lisp-driver)

(declaim (optimize (debug 3) (safety 0) (speed 0)))

(require :asdf)

(asdf:load-system :x64lisp)
(asdf:load-system :unix-opts)

(opts:define-opts)

(defun print-error (e)
    (loop for bt in (x64lisp:assembly-error.backtrace-reports e) do
         (format *error-output* "In ~a~%" (x64lisp:backtrace-report.form-name bt)))
    
    (format *error-output* "Error assembling file: ~a~%" (x64lisp:assembly-error.text e))

    (values))

(multiple-value-bind (opts args) (opts:get-opts)
    (declare (ignore opts))

    (let ((*package* (find-package :x64lisp-user)))
        (handler-case (x64lisp:load-files args)
          (x64lisp:assembly-error (e) (print-error e)))))
