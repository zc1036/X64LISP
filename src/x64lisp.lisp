
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
  (:export :btype
           :int-type
           :ptr-type
           :typeless-ptr
           :struct-type
           :union-type
           :proc-type
           :process-struct-decl
           :process-proc-decl
           :asm-module))

(defpackage :x64lisp-user
  (:use :cl))

(in-package :x64lisp)

;; The base for all type classes
(defclass btype ()
  ((size :reader btype.size)
   (alignment :reader btype.alignment)))

(defgeneric btype.equalp (btype btype)
    (:documentation "Compares two BTYPEs for equality"))

(defmethod btype.equalp (btype btype)
    nil) ;; default implementation is to evaluate to false

(defmethod btype.equalp (()))

(defclass int-type (btype)
  ((size :initarg :size ; size in bytes
         :reader btype.size)
   (signed :initarg :signed
           :reader int.signed)))

(defmethod btype.alignment ((x int-type))
    (btype.size x))

(defmethod btype.equalp ((x int-type) (y int-type))
    (eq x y))

(defclass ptr-type (btype)
  ((pointee-type :initarg :pointee-type
                 :reader ptr.pointee-type)
   (size :initform 8)
   (alignment :initform 8)))

(defmethod btype.equalp ((x ptr-type) (y ptr-type))
    (btype.equalp (ptr.pointee-type x) (ptr.pointee-type y)))

(defclass typeless-ptr (btype)
  ((size :initform 8)
   (alignment :initform 8)))

(defmethod btype.equalp ((x typeless-ptr) (y typeless-ptr))
    t)

(defclass struct-type (btype)
  ((fields :initarg :fields
           :reader struct.fields)
   (name :initarg :name
         :reader struct.name)))

(defmethod btype.equalp ((x struct-type) (y struct-type))
    (eq x y))

(defmethod btype.alignment ((s struct-type))
    (apply #'lcm (map 'list #'btype.alignment (struct.fields s))))

;;; Returns the size of the struct and a list of field offsets
(defun struct.size-and-field-offsets (s)
    (multiple-value-bind (size offsets) (map-accum 0
                                       (lambda (current-offset field)
                                           (let ((aligned-offset (ceil-to-nearest-multiple current-offset (btype.alignment field))))
                                               (values (+ aligned-offset (btype.size field)) aligned-offset)))
                                       (struct.fields s))
        (values
         (ceil-to-nearest-multiple size (btype.alignment s))
         offsets)))

(defmethod btype.size ((s struct-type))
    ;; get rid of the second return value of
    ;; STRUCT.SIZE-AND-FIELD-OFFSETS with VALUES
    (values (struct.size-and-field-offsets s)))

(defclass union-type (btype)
  ((struct.fields :initarg :fields
                  :reader union.fields)))

(defmethod btype.alignment ((s union-type))
    (apply #'lcm (map 'list #'btype.alignment (struct.fields s))))

(defmethod btype.equalp ((x union-type) (y union-type))
    (eq x y))

(define-condition size-of-sizeless-type (error)
  ((text :initarg :text :reader error.text)))

(defclass proc-type (btype)
  ((ret-type :initarg :ret-type
             :reader proc-type.ret-type)
   (arg-types :initarg :arg-types
              :reader proc-type.arg-types)))

(defmethod btype.size ((s proc-type))
    (error 'size-of-sizeless-type :text "Size of sizeless type erroneously required: function"))

(defmethod btype.alignment ((s proc-type))
    (error 'size-of-sizeless-type :text "Alignment of sizeless type erroneously required: function"))

(defmethod btype.equalp ((x proc-type) (y proc-type))
    (eq x y))

(defun process-struct-field-decl (decl)
    (destructuring-bind (member-name member-type) decl
        `(list ',member-name ,member-type)))

(defun process-struct-decl (struct-name field-decls)
    `(defparameter ,struct-name
       (make-instance 'struct-type
                      :fields (list ,@(map 'list #'process-struct-field-decl field-decls))
                      :name ,(symbol-name struct-name))))

(defclass asm-module ()
  ((procs :initarg :procs
          :accessor asm-module.procs)
   (name :initarg :name
         :accessor asm-module.name)))

(defparameter *modules* nil)
(defparameter *current-module* nil)

(defun process-proc-decl (proc-name args body)
    `(defparameter ,proc-name
       (let ((proc (lambda () ,@body)))
           (push proc (asm-module.procs *current-module*)))))

(in-package :x64lisp-user)

(defmacro module (module-name)
    (setf *current-module* (make-instance 'x64lisp:asm-module
                                          :procs nil
                                          :name (symbol-name module-name)))
    (push *current-module* *modules*)
    (values))

(defparameter int8 (make-instance 'x64lisp:int-type :size 1 :signed t))
(defparameter int16 (make-instance 'x64lisp:int-type :size 2 :signed t))
(defparameter int32 (make-instance 'x64lisp:int-type :size 4 :signed t))
(defparameter int64 (make-instance 'x64lisp:int-type :size 8 :signed t))
(defparameter uint8 (make-instance 'x64lisp:int-type :size 1 :signed nil))
(defparameter uint16 (make-instance 'x64lisp:int-type :size 2 :signed nil))
(defparameter uint32 (make-instance 'x64lisp:int-type :size 4 :signed nil))
(defparameter uint64 (make-instance 'x64lisp:int-type :size 8 :signed nil))

(defparameter char uint8)

(defmacro struct (struct-name &body field-decls)
    (x64lisp:process-struct-decl struct-name field-decls))

(defmacro proc (proc-name args &body body)
    (x64lisp:process-proc-decl proc-name args body))
