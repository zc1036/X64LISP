
(in-package :asdf-user)

(defsystem "x64lisp"
    :description "A low-level lisp for x64 processors"
    :version "0.1"
    :author "Zach"
    :licence "RBRSPL 1"
    :components ((:file "packages")
                 (:file "functional")
                 (:file "math")
                 (:file "macro-assist")
                 (:file "x64lisp")
                 (:file "x64"))
    :depends-on (:unix-opts))