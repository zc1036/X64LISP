
(require :asdf)

(asdf:load-system :x64lisp)
;(asdf:load-system :unix-opts)

(opts:define-opts)

(multiple-value-bind (opts args) (opts:get-opts)
    (declare (ignore opts))

    (let ((*package* (find-package :x64lisp-user)))
        (x64lisp:load-files args)))
