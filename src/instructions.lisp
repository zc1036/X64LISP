
;;;; Defines the instructions and structures that comprise an x64
;;;; assembly program

(in-package :instructions)

(defclass instr-arg ()
  ((repr :initarg :repr
         :reader instr-arg.repr)))

;;; Superclass of all registers
(defclass reg (instr-arg)
  ((size :initarg :size
         :reader reg.size)))

;; A virtual register
(defclass vreg (reg)
  ((id :initarg :id
       :reader vreg.id)))

(defmethod instr-arg.repr ((r vreg))
    (format nil "v~a" (vreg.id r)))

(let ((next-vreg-id 0))
    (defun new-vreg (size)
        (make-instance 'vreg
                       :id (incf next-vreg-id)
                       :size size)))

;; A general-purpose x64 register
(defclass gpreg (reg ast-expr)
  ((name :initarg :name
         :reader reg.name)
   ;; ALIASES-OF is NIL if this register doesn't alias other
   ;; registers, or otherwise a list of the form
   ;;   (aliased-reg begin-alias-byte end-alias-byte)
   ;; begin-alias-byte and end-alias-byte form a half-open range where
   ;; 0 is the LSB and SIZE is the MSB
   (alias-of :initarg :alias-of
             :reader reg.alias-of)
   (repr :initarg :name)))

(defmethod ast-expr.to-instructions ((r gpreg))
    (values nil r))

(defmethod print-object ((x instr-arg) stream)
    (princ (instr-arg.repr x) stream))

(defmacro make-gpreg (name repr size &optional alias-of)
    (let ((type (ccase size
                  (1 'uint8)
                  (2 'uint16)
                  (4 'uint32)
                  (8 'uint64))))
        `(defparameter ,name (make-instance 'gpreg
                                            :name ,repr
                                            :size ,size
                                            :type ,type
                                            :alias-of ,alias-of))))

(defmacro make-gpregs (&rest lists)
    `(progn
         ,@(mapcar (lambda (args) `(make-gpreg ,@args)) lists)))

(defclass mem-ref (instr-arg)
  ((address :initarg :address
            :reader mem-ref.address)))

;;; The superclass of all instruction classes
(defclass instr (ast-expr)
  ((name :initarg :name
         :reader instr.name)
   (repr :initarg :repr
         :reader instr.repr)
   (type :initform void)))

(defmethod ast-expr.to-instructions ((x instr)) (values x nil))

(defmethod print-object ((x instr) stream)
    (princ (instr.repr x) stream))

(defclass nullary-op (instr)
  ())

(defmethod instr.repr ((x nullary-op))
    (instr.name x))

(defclass unary-op (instr)
  ((op :initarg :op
       :reader unary-op.op)))

(defmethod instr.repr ((x unary-op))
    (format nil "~a ~a" (instr.name x) (unary-op.op x)))

;; flow of control operator
(defclass foc-op (unary-op)
  ())

(defclass binary-op (instr)
  ((dst :initarg :dst
        :reader binary-op.dst)
   (src :initarg :src
        :reader binary-op.src)))

(defmethod instr.repr ((x binary-op))
    (format nil "~a ~a, ~a" (instr.name x) (binary-op.dst x) (binary-op.src x)))

(defclass @cli (nullary-op)
  ((name :initform "cli")
   (repr :initform "cli")))

(defclass @add (binary-op)
  ((name :initform "add")))

(defclass @mov (binary-op)
  ((name :initform "mov")))

(defclass @label (nullary-op)
  ;; For making the assembly output more readable
  ((comment :initarg :comment
            :initform nil
            :reader @label.comment)))

(defmethod instr.repr ((x @label))
    (with-slots (name comment) x
        (if comment
            (format nil "~a: ;# ~a" name comment)
            (format nil "~a:" name))))

(defmacro with-labels (label-specs &body body)
    ;; The gensym below has to be evaluated at runtime, not
    ;; compile-time, or else a function that uses WITH-LABELS multiple
    ;; times will be getting the same label names.
    `(let ,(mapcar (lambda (x) `(,(car x) (make-instance '@label :name (gensym) :comment ,(cadr x))))
                   label-specs)
         ,@body))

(defclass @jmp (foc-op)
  ((name :initform "jmp")))

(defclass @jne (foc-op)
  ((name :initform "jne")))

(defclass @jz (foc-op)
  ((name :initform "jz")))

(defclass @tst (unary-op)
  ((name :initform "tst")))

;; Quick way to define an instruction function that simply
;; instantiates an instruction class
(defmacro make-instr-interface (name class &rest lambda-list)
    ;; the below MAPCAR takes a list (a b c) and turns it into (:a a :b b :c c)
    (let ((ctor-args (apply #'append (mapcar (lambda (x) (list (intern (symbol-name x) :keyword) x)) lambda-list))))
        `(defun ,name ,lambda-list
             (make-instance (intern ,(symbol-name class) :instructions) ,@ctor-args))))

;;;
;;; x86 registers and their 64-bit aliases
;;;
(make-gpregs
 ;; the "true" registers
 (%r0 "r0" 8) (%r1 "r1" 8) (%r2  "r2" 8)  (%r3  "r3" 8)  (%r4  "r4" 8)  (%r5  "r5" 8)  (%r6  "r6" 8)  (%r7  "r7" 8)
 (%r8 "r8" 8) (%r9 "r9" 8) (%r10 "r10" 8) (%r11 "r11" 8) (%r12 "r12" 8) (%r13 "r13" 8) (%r14 "r14" 8) (%r15 "r15" 8)

 ;; old-style aliases
 (%rax "rax" 8 `(,%r0 0 8)) (%rbx "rbx" 8 `(,%r3 0 8)) (%rcx "rcx" 8 `(,%r1 0 8)) (%rdx "rdx" 8 `(,%r2 0 8))
 (%rbp "rbp" 8 `(,%r5 0 8)) (%rsp "rsp" 8 `(,%r4 0 8)) (%rsi "rsi" 8 `(,%r6 0 8)) (%rdi "rdi" 8 `(,%r7 0 8))

 ;; x86 registers
 (%eax "eax" 4 `(,%r0 0 4)) (%ebx "ebx" 4 `(,%r3 0 4)) (%ecx "ecx" 4 `(,%r1 0 4)) (%edx "edx" 4 `(,%r2 0 4))
 (%ebp "ebp" 4 `(,%r5 0 4)) (%esp "esp" 4 `(,%r4 0 4)) (%esi "esi" 4 `(,%r6 0 4)) (%edi "edi" 4 `(,%r7 0 4))

 (%ax "ax" 2 `(,%r0 0 2)) (%bx "bx" 2 `(,%r3 0 2)) (%cx "cx" 2 `(,%r1 0 2)) (%dx "dx" 2 `(,%r2 0 2))
 (%bp "bp" 2 `(,%r5 0 2)) (%sp "sp" 2 `(,%r4 0 2)) (%si "si" 2 `(,%r6 0 2)) (%di "di" 2 `(,%r7 0 2))

 (%ah  "ah"  1 `(,%r0 1 2)) (%bh  "bh"  1 `(,%r3 1 2)) (%ch  "ch"  1 `(,%r1 1 2)) (%dh  "dh"  1 `(,%r2 1 2))
 (%bph "bph" 1 `(,%r5 1 2)) (%sph "sph" 1 `(,%r4 1 2)) (%sih "sih" 1 `(,%r6 1 2)) (%dih "dih" 1 `(,%r7 1 2))

 (%al  "al"  1 `(,%r0 0 1)) (%bl  "bl"  1 `(,%r3 0 1)) (%cl  "cl"  1 `(,%r1 0 1)) (%dl  "dl"  1 `(,%r2 0 1))
 (%bpl "bpl" 1 `(,%r5 0 1)) (%spl "spl" 1 `(,%r4 0 1)) (%sil "sil" 1 `(,%r6 0 1)) (%dil "dil" 1 `(,%r7 0 1))

 ;; a bunch of aliases for parts of the rXX registers

 (%r0L "r0L" 1 `(,%r0 0 1)) (%r1L "r1L" 1 `(,%r1 0 1)) (%r2L "r2L" 1 `(,%r2 0 1)) (%r3L "r3L" 1 `(,%r3 0 1))
 (%r4L "r4L" 1 `(,%r4 0 1)) (%r5L "r5L" 1 `(,%r5 0 1)) (%r6L "r6L" 1 `(,%r6 0 1)) (%r7L "r7L" 1 `(,%r7 0 1))
 (%r8L "r8L" 1 `(,%r8 0 1)) (%r9L "r9L" 1 `(,%r9 0 1)) (%r10L "r10L" 1 `(,%r10 0 1)) (%r11L "r11L" 1 `(,%r11 0 1))
 (%r12L "r12L" 1 `(,%r12 0 1)) (%r13L "r13L" 1 `(,%r13 0 1)) (%r14L "r14L" 1 `(,%r14 0 1)) (%r15L "r15L" 1 `(,%r15 0 1))

 (%r0W "r0W" 2 `(,%r0 0 2)) (%r1W "r1W" 2 `(,%r1 0 2)) (%r2W "r2W" 2 `(,%r2 0 2)) (%r3W "r3W" 2 `(,%r3 0 2))
 (%r4W "r4W" 2 `(,%r4 0 2)) (%r5W "r5W" 2 `(,%r5 0 2)) (%r6W "r6W" 2 `(,%r6 0 2)) (%r7W "r7W" 2 `(,%r7 0 2))
 (%r8W "r8W" 2 `(,%r8 0 2)) (%r9W "r9W" 2 `(,%r9 0 2)) (%r10W "r10W" 2 `(,%r10 0 2)) (%r11W "r11W" 2 `(,%r11 0 2))
 (%r12W "r12W" 2 `(,%r12 0 2)) (%r13W "r13W" 2 `(,%r13 0 2)) (%r14W "r14W" 2 `(,%r14 0 2)) (%r15W "r15W" 2 `(,%r15 0 2))

 (%r0D "r0D" 4 `(,%r0 0 4)) (%r1D "r1D" 4 `(,%r1 0 4)) (%r2D "r2D" 4 `(,%r2 0 4)) (%r3D "r3D" 4 `(,%r3 0 4))
 (%r4D "r4D" 4 `(,%r4 0 4)) (%r5D "r5D" 4 `(,%r5 0 4)) (%r6D "r6D" 4 `(,%r6 0 4)) (%r7D "r7D" 4 `(,%r7 0 4))
 (%r8D "r8D" 4 `(,%r8 0 4)) (%r9D "r9D" 4 `(,%r9 0 4)) (%r10D "r10D" 4 `(,%r10 0 4)) (%r11D "r11D" 4 `(,%r11 0 4))
 (%r12D "r12D" 4 `(,%r14 0 4)) (%r13D "r13D" 4 `(,%r13 0 4)) (%r14D "r14D" 4 `(,%r14 0 4)) (%r15D "r15D" 4 `(,%r15 0 4)))

(make-instr-interface $ mem-ref address)
(make-instr-interface cli @cli)
(make-instr-interface add @add dst src)
(make-instr-interface mov @mov dst src)
(make-instr-interface tst @tst op)

(make-instr-interface label @label name)
(make-instr-interface jmp @jmp op)
(make-instr-interface jne @jne op)
(make-instr-interface jz @jz op)
