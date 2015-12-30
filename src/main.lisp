
(require :asdf)

(asdf:load-system :x64lisp)

(opts:define-opts)

(multiple-value-bind (opts args) (opts:get-opts)
    (declare (ignore opts))

    (let ((*package* (find-package :x64lisp-user)))
        (handler-case (x64lisp:load-files args)
          (x64lisp:assembly-error (e) (format *error-output*
                                              "Error assembling file: ~a~%"
                                              (x64lisp:assembly-error.text e))))))
