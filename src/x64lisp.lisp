
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

(defpackage :x64lisp
  (:use :cl)
  (:export :process-struct-decl))

(defpackage :x64lisp-user
  (:use :cl))

(in-package :x64lisp)

(defclass ctype ()
  ((ctype.size :reader ctype.size)
   (ctype.alignment :reader ctype.alignment)))

(defclass int-type (ctype)
  ((ctype.size :initarg size
               :reader ctype.size)
   (int.signed :initarg signed
               :reader int.signed)))

(defmethod ctype.alignment ((x int-type))
    (ctype.size x))

(defclass ptr-type (ctype)
  ((ptr.pointee-type :initarg pointee-type
                     :reader pointee-type)
   (ctype.size :initform 8)
   (ctype.alignment :initform 8)))

(defclass struct-type (ctype)
  ((struct.fields :initarg fields
                  :reader struct.fields)
   (struct.name :initarg name
                :reader struct.name)))

(defmethod ctype.alignment ((s struct-type))
    (reduce #'max (map 'list #'ctype.alignment (struct.fields s))))

(defun struct.field-offsets (s)
    (map-accum 0
               (lambda (offset field)
                   (let* ((fieldsize (ctype.size field))
                          (padding (mod (- fieldsize (mod offset fieldsize)) fieldsize)))
                       (values (+ offset padding fieldsize) (+ offset padding))))
               (struct.fields s)))

(defclass union-type (ctype)
  ((struct.fields :initarg fields
                  :reader union.fields)))

(defmethod ctype.alignment ((s union-type))
    (reduce #'max (map 'list #'ctype.alignment (struct.fields s))))

(defun process-struct-field-decl (decl)
    (destructuring-bind (member-name member-type) decl
        `(list ',member-name ,member-type)))

(defun process-struct-decl (struct-name field-decls)
    `(defparameter ,struct-name
       (make-instance 'struct-type
                      :fields (list ,@(map 'list #'process-struct-field-decl field-decls))
                      :name ,(symbol-name struct-name))))

(in-package :x64lisp-user)

(defmacro struct (struct-name &body field-decls)
    (x64lisp:process-struct-decl struct-name field-decls))

(defmacro proc (proc-name args &body body)
    )
