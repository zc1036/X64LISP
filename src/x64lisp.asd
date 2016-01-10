
(in-package :asdf-user)

(defsystem "x64lisp"
    :description "A low-level lisp for x64 processors"
    :version "0.1"
    :author "Zach"
    :licence "RBRSPL 1"
    :components ((:file "packages")
                 (:file "macro-assist")
                 (:file "functional")
                 (:file "math")
                 (:file "conditions")
                 (:file "types")
                 (:file "ast")
                 (:file "instructions")
                 (:file "core-structures")
                 (:file "core-forms")
                 (:file "x64lisp")))
