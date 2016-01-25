
;;;; Three-address code

(in-package :tac)

(defclass instr-arg ()
  ((repr :initarg :repr
         :reader instr-arg.repr)))

;; A virtual register
(defclass vreg (instr-arg)
  ((id :initarg :id
       :reader vreg.id)
   (size :initarg :size
         :reader vreg.size)))

(defmethod instr-arg.repr ((r vreg))
    (format nil "v~a" (vreg.id r)))

(defmethod print-object ((x instr-arg) stream)
    (princ (instr-arg.repr x) stream))

(let ((next-vreg-id 0))
    (defun new-vreg (size)
        (make-instance 'vreg
                       :id (incf next-vreg-id)
                       :size size)))

(defclass mem-ref (instr-arg)
  ((address :initarg :address
            :reader mem-ref.address)))

;; A label object
(defclass label (instr-arg)
  ;; For making the assembly output more readable
  ((comment :initarg :comment
            :initform nil
            :reader @label.comment)))

;;; The superclass of all instruction classes
(defclass instr ()
  ((name :initarg :name
         :reader instr.name)
   (repr :initarg :repr
         :reader instr.repr)
   (type :initform void)))

(defmethod print-object ((x instr) stream)
    (princ (instr.repr x) stream))

(defclass output-op (instr)
  ((dst :initarg :dst
        :reader output-op.dst)))

(defclass side-effect-op (instr)
  ())

(defclass unary-op (output-op)
  ((op :initarg :op
       :reader unary-op.op)))

(defmethod instr.repr ((x unary-op))
    (format nil "~a := ~a ~a" (output-op.dst x) (instr.name x) (unary-op.op x)))

;; flow of control operator
(defclass foc-op (instr)
  ((target :initarg :target
            :reader foc-op.target)))

(defmethod instr.repr ((x foc-op))
    (format nil "~a ~a" (instr.name x) (foc-op.target x)))

(defclass test-foc-op (foc-op)
  ((op :initarg :op
       :reader foc-op.op)))

(defmethod instr.repr ((x test-foc-op))
    (format nil "~a ~a, ~a" (instr.name x) (foc-op.op x) (foc-op.target x)))

(defclass binary-op (output-op)
  ((srcl :initarg :srcl
         :reader binary-op.dst)
   (srcr :initarg :srcr
         :reader binary-op.src)))

(defmethod instr.repr ((x binary-op))
    (with-slots (name dst srcl srcr) x
        (format nil "~a := ~a ~a, ~a" dst name srcl srcr)))

(defmacro with-labels (label-specs &body body)
    ;; The gensym below has to be evaluated at runtime, not
    ;; compile-time, or else a function that uses WITH-LABELS multiple
    ;; times will be getting the same label names.
    `(let ,(mapcar (lambda (x) (etypecase x
                                 (cons `(,(car x) (make-instance 'label :repr (gensym) :comment ,(cadr x))))
                                 (atom `(,x (make-instance 'label :repr (gensym))))))
                   label-specs)
         ,@body))

;; The instruction to mark a particular location with a label
(defclass @label (unary-op)
  ((name :initform "label")))

(defmethod instr.repr ((x @label))
    (multiple-with-slots ((op x) (comment op))
        (if comment
            (format nil "~a: ;# ~a" op comment)
            (format nil "~a:" op))))

;; High-level instruction to "slice" a register. Evaluates to a byte
;; sequence that is (END - BEGIN) bytes long which are copies of the
;; bytes in REG starting at byte BEGIN.
(defclass @member-access (output-op)
  ((name :initform "member-access")
   (reg :initarg :reg
        :reader @member-access.reg)
   (begin :initarg :begin
          :reader @member-access.begin)
   (end :initarg :end
        :reader @member-access.end)))

(defmethod instr.repr ((x @member-access))
    (with-slots (dst name reg begin end) x
        (format nil "~a := ~a ~a[~a:~a]" dst name reg begin end)))

(defclass @add (binary-op)
  ((name :initform "add")))

(defclass @move (side-effect-op)
  ((name :initform "mov")
   (dst :initarg :dst
        :reader @move.dst)
   (src :initarg :src
        :reader @move.src)))

(defmethod instr.repr ((x @move))
    (with-slots (name dst src) x
        (format nil "~a ~a, ~a" name dst src)))

(defclass @jump (foc-op)
  ((name :initform "jump")))

(defclass @j-not-equal (test-foc-op)
  ((name :initform "j-not-equal")))

(defclass @j-zero (test-foc-op)
  ((name :initform "j-zero")))

;; Quick way to define an instruction function that simply
;; instantiates an instruction class
(defmacro make-instr-interface (name class &rest lambda-list)
    ;; the below MAPCAR takes a list (a b c) and turns it into (:a a :b b :c c)
    (let ((ctor-args (apply #'append (mapcar (lambda (x) (list (intern (symbol-name x) :keyword) x)) lambda-list))))
        `(defun ,name ,lambda-list
             (make-instance (intern ,(symbol-name class) :tac) ,@ctor-args))))

(make-instr-interface $ mem-ref address)
(make-instr-interface add @add dst srcl srcr)
(make-instr-interface move @move dst src)
(make-instr-interface member-access @member-access dst reg begin end)

(make-instr-interface lbl @label op)
(make-instr-interface jump @jump target)
(make-instr-interface j-not-equal @j-not-equal op target)
(make-instr-interface j-zero @j-zero op target)
