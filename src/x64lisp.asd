
(in-package :asdf-user)

(defsystem "x64lisp"
    :description "A low-level lisp for x64 processors"
    :version "0.1"
    :author "Zach"
    :licence "MIT"
    :components ((:file "packages")
                 (:file "macro-assist")
                 (:file "data-structures")
                 (:file "functional")
                 (:file "math")
                 (:file "conditions")
                 (:file "source-readtable")
                 (:file "types")
                 (:file "ast")
                 (:file "x64")
                 (:file "tac")
                 (:file "core-conditions")
                 (:file "core-structures")
                 (:file "cfg")
                 (:file "core-forms")
                 (:file "operator-nicknames")
                 (:file "x64lisp"))
    :depends-on (:unix-opts :optima))
