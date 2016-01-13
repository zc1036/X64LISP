
;;;; Defines the basic language types

(in-package :types)

;; The base for all type classes
(defclass btype ()
  ((size :reader btype.size)
   (alignment :reader btype.alignment)))

(define-condition size-of-sizeless-type (assembly-error)
  ())

(define-condition alignment-of-sizeless-type (assembly-error)
  ())

(defgeneric btype.equalp (btype btype)
    (:documentation "Compares two BTYPEs for equality"))

(defmethod btype.equalp (a b)
    (declare (ignore a))
    (declare (ignore b))
    nil) ;; default implementation is to evaluate to false

(defclass void-type (btype)
  ())

(defmethod btype.size ((s void-type))
    (error 'size-of-sizeless-type :text "Size of sizeless type erroneously required: void"))

(defmethod btype.alignment ((s void-type))
    (error 'alignment-of-sizeless-type :text "Alignment of sizeless type erroneously required: void"))

(defmethod btype.equalp ((a void-type) (b void-type))
    (declare (ignore a))
    (declare (ignore b))
    t)

(defclass int-type (btype)
  ((size :initarg :size ; size in bytes
         :reader btype.size)
   (signed :initarg :signed
           :reader int-type.signed)
   ;; For testing whether lisp integers can be represented by this
   ;; integer type class
   (lisp-typespec :initarg :lisp-typespec
                  :reader int-type.lisp-typespec)))

(defmethod btype.alignment ((x int-type))
    (btype.size x))

(defmethod btype.equalp ((x int-type) (y int-type))
    (and (= (btype.size x) (btype.size y))
         (eq (int-type.signed x) (int-type.signed y))))

(defclass ptr-type (btype)
  ((pointee-type :initarg :pointee-type
                 :reader ptr-type.pointee-type)
   (size :initform 8)
   (alignment :initform 8)))

(defmethod btype.equalp ((x ptr-type) (y ptr-type))
    (btype.equalp (ptr-type.pointee-type x) (ptr-type.pointee-type y)))

(defclass typeless-ptr (btype)
  ((size :initform 8)
   (alignment :initform 8)))

(defmethod btype.equalp ((x typeless-ptr) (y typeless-ptr))
    t)

(defclass array-type (btype)
  ((element-type :initarg :element-type
                 :reader array-type.element-type)
   (element-count :initarg :element-count
                  :reader array-type.element-count)))

(defmethod btype.alignment ((x array-type))
    (btype.alignment (array-type.element-type x)))

(defmethod btype.size ((x array-type))
    (* (btype.size (array-type.element-type x) (array-type.element-count x))))

(defmethod btype.equalp ((x array-type) (y array-type))
    (and (= (array-type.element-count x) (array-type.element-count y))
         (btype.equalp (array-type.element-type x) (array-type.element-type y))))

(defstruct (struct-field-info (:conc-name struct-field-info.))
  name type)

(defun struct-field-info= (a b)
    (eq (struct-field-info.name a) (struct-field-info.name b)))

(defclass struct-type (btype)
  ((fields :initarg :fields
           :reader struct-type.fields)
   (name :initarg :name
         :reader struct-type.name)
   (field-offsets)))

(defmethod btype.equalp ((x struct-type) (y struct-type))
    (eq x y))

(defmethod btype.alignment ((s struct-type))
    (apply #'lcm (map 'list #'btype.alignment (struct-type.fields s))))

;;; Returns the size of the struct and a list of field offsets (memoized)
(defun struct-type.size-and-field-offsets (s)
    (if (slot-boundp s 'field-offsets)
        (values (slot-value s 'size) (slot-value s 'field-offsets))
        (multiple-value-bind (size offsets) (map-accum 0
                                                       (lambda (current-offset field)
                                                           (let ((aligned-offset (ceil-to-nearest-multiple current-offset (btype.alignment field))))
                                                               (values (+ aligned-offset (btype.size field)) aligned-offset)))
                                                       (struct-type.fields s))
            (setf (slot-value s 'size) (ceil-to-nearest-multiple size (btype.alignment s)))
            (setf (slot-value s 'field-offsets) offsets)
            (struct-type.size-and-field-offsets s))))

(define-condition nonexistent-struct-field (assembly-error)
  ((field-name :initarg :field-name
                :reader nonexistent-struct-field.field-name)))

(defmethod assembly-error.text ((x nonexistent-struct-field))
    (format nil "Nonexistent struct field name '~a'" (nonexistent-struct-field.field-name x)))

(defun struct-type.field-offset (s member)
    (unless (typep member 'symbol)
        (error 'assembly-error :text "Struct member must be a symbol"))

    (multiple-value-bind (size offsets) (struct-type.size-and-field-offsets s)
        (declare (ignore size))
        (let ((field-position (position member (struct-type.fields s))))
            (unless field-position
                (error 'nonexistent-struct-field :field-name member))
            (elt offsets field-position))))

(defun struct-type.field (s field-name)
    (let ((field-info (find field-name (struct-type.fields s) :test #'struct-field-info=)))
        (unless field-info
            (error 'nonexistent-struct-field :field-name field-name))
        field-info))

(defmethod btype.size ((s struct-type))
    ;; get rid of the second return value of
    ;; STRUCT.SIZE-AND-FIELD-OFFSETS with VALUES
    (values (struct-type.size-and-field-offsets s)))

(defclass union-type (btype)
  ((fields :initarg :fields
           :reader union-type.fields)))

(defmethod btype.alignment ((s union-type))
    (apply #'lcm (map 'list #'btype.alignment (union-type.fields s))))

(defmethod btype.equalp ((x union-type) (y union-type))
    (eq x y))

(defclass proc-type (btype)
  ((ret-type :initarg :ret-type
             :reader proc-type.ret-type)
   (arg-types :initarg :arg-types
              :reader proc-type.arg-types)))

(defmethod btype.size ((s proc-type))
    (error 'size-of-sizeless-type :text "Size of sizeless type erroneously required: function"))

(defmethod btype.alignment ((s proc-type))
    (error 'alignment-of-sizeless-type :text "Alignment of sizeless type erroneously required: function"))

(defmethod btype.equalp ((x proc-type) (y proc-type))
    (and (btype.equalp (proc-type.ret-type x) (proc-type.ret-type y))
         ;; ensure that the type of every argument is BTYPE.EQUALP
         (reduce (destructuring-lambda (carry (xarg yarg)) (and carry (btype.equalp xarg yarg)))
                 (mapcar #'list (proc-type.arg-types x) (proc-type.arg-types y)) ; (a b) -> (c d) -> ((a c) (b d))
                 :initial-value t)))

(define-condition asm-type-error (assembly-error)
  ())

(defun type-assert (object type)
    "OBJECT is EXPRESSION-LIKE, and TYPE is either an instance or subclass of BTYPE"
    (unless (or (btype.equalp object type) (typep object type))
         (error 'asm-type-error
                :text (format nil "Type mismatch: got ~a, expected ~a" object type))))

(defun bits-typespec (num-bits signedp)
    (if signedp
        `(integer ,(- (expt 2 (1- num-bits))) ,(1- (expt 2 (1- num-bits))))
        `(integer 0 ,(1- (expt 2 num-bits)))))

(defparameter int8 (make-instance 'int-type :size 1 :signed t :lisp-typespec (bits-typespec 8 t)))
(defparameter int16 (make-instance 'int-type :size 2 :signed t :lisp-typespec (bits-typespec 16 t)))
(defparameter int32 (make-instance 'int-type :size 4 :signed t :lisp-typespec (bits-typespec 32 t)))
(defparameter int64 (make-instance 'int-type :size 8 :signed t :lisp-typespec (bits-typespec 64 t)))
(defparameter uint8 (make-instance 'int-type :size 1 :signed nil :lisp-typespec (bits-typespec 8 nil)))
(defparameter uint16 (make-instance 'int-type :size 2 :signed nil :lisp-typespec (bits-typespec 16 nil)))
(defparameter uint32 (make-instance 'int-type :size 4 :signed nil :lisp-typespec (bits-typespec 32 nil)))
(defparameter uint64 (make-instance 'int-type :size 8 :signed nil :lisp-typespec (bits-typespec 64 nil)))

;; A list of the integral types
(defparameter integral-types
  (list int8 int16 int32 int64 uint8 uint16 uint32 uint64))

(defparameter void (make-instance 'void-type))

(let ((int-type-table (make-array '(4 4)
                                  :initial-contents
                                  (list (list uint8 uint16 uint32 uint64)
                                        (list uint16 uint16 uint32 uint64)
                                        (list uint32 uint32 uint32 uint64)
                                        (list uint64 uint64 uint64 uint64)))))
    (defun common-type (x y)
        (cond
            ((and (typep x 'int-type) (typep y 'int-type))
             (aref int-type-table (round (log (btype.size x) 2))
                   (round (log (btype.size y) 2))))
            (t (error 'assembly-error
                      :text (format nil "Cannot find common type of ~a and ~a (types ~a and ~a)"
                                    x y (type-of x) (type-of y)))))))
