
;;;; Defines the instructions and structures that comprise an x64
;;;; assembly program

(in-package :x64)

(defclass instr-arg ()
  ((repr :initarg :repr
         :reader instr-arg.repr)))

;;; Superclass of all registers
(defclass reg (instr-arg)
  ((name :initarg :name
         :reader reg.name)
   (size :initarg :size
         :reader reg.size)
   ;; ALIASES-OF is NIL if this register doesn't alias other
   ;; registers, or otherwise a list of lists of the form
   ;;   (aliased-reg begin-alias-byte end-alias-byte)
   ;; begin-alias-byte and end-alias-byte form a half-open range where
   ;; 0 is the LSB and SIZE is the MSB
   (aliases-of :initarg :aliases-of
               :reader reg.aliases-of)))

(defclass gpreg (reg)
  ())

(defmethod print-object ((x instr-arg) stream)
    (format stream "~a" (instr-arg.repr x)))

(defmacro make-gpreg (name repr size &optional aliases-of)
    `(defparameter ,name (make-instance 'gpreg :name ,repr :size ,size :aliases-of ,aliases-of)))

;;;
;;; x86 registers and their 64-bit aliases
;;;

(make-gpreg %r0 "r0" 8)
(make-gpreg %r1 "r1" 8)
(make-gpreg %r2 "r2" 8)
(make-gpreg %r3 "r3" 8)
(make-gpreg %r4 "r4" 8)
(make-gpreg %r5 "r5" 8)
(make-gpreg %r6 "r6" 8)
(make-gpreg %r7 "r7" 8)

(make-gpreg %rax "rax" 8 `((,%r0 0 8)))
(make-gpreg %rbx "rbx" 8 `((,%r3 0 8)))
(make-gpreg %rcx "rcx" 8 `((,%r1 0 8)))
(make-gpreg %rdx "rdx" 8 `((,%r2 0 8)))
(make-gpreg %rbp "rbp" 8 `((,%r5 0 8)))
(make-gpreg %rsp "rsp" 8 `((,%r4 0 8)))
(make-gpreg %rsi "rsi" 8 `((,%r6 0 8)))
(make-gpreg %rdi "rdi" 8 `((,%r7 0 8)))

(make-gpreg %eax "eax" 4 `((,%rax 0 4)))
(make-gpreg %ebx "ebx" 4 `((,%rbx 0 4)))
(make-gpreg %ecx "ecx" 4 `((,%rcx 0 4)))
(make-gpreg %edx "edx" 4 `((,%rdx 0 4)))
(make-gpreg %ebp "ebp" 4 `((,%rbp 0 4)))
(make-gpreg %esp "esp" 4 `((,%rsp 0 4)))
(make-gpreg %esi "esi" 4 `((,%rsi 0 4)))
(make-gpreg %edi "edi" 4 `((,%rdi 0 4)))

(make-gpreg %ax "ax" 2 `((,%eax 0 2)))
(make-gpreg %bx "bx" 2 `((,%ebx 0 2)))
(make-gpreg %cx "cx" 2 `((,%ecx 0 2)))
(make-gpreg %dx "dx" 2 `((,%edx 0 2)))
(make-gpreg %bp "bp" 2 `((,%ebp 0 2)))
(make-gpreg %sp "sp" 2 `((,%esp 0 2)))
(make-gpreg %si "si" 2 `((,%esi 0 2)))
(make-gpreg %di "di" 2 `((,%edi 0 2)))

(make-gpreg %ah "ah" 1 `((,%ax 1 2)))
(make-gpreg %bh "bh" 1 `((,%bx 1 2)))
(make-gpreg %ch "ch" 1 `((,%cx 1 2)))
(make-gpreg %dh "dh" 1 `((,%dx 1 2)))
(make-gpreg %bph "bph" 1 `((,%bp 1 2)))
(make-gpreg %sph "sph" 1 `((,%sp 1 2)))
(make-gpreg %sih "sih" 1 `((,%si 1 2)))
(make-gpreg %dih "dih" 1 `((,%di 1 2)))

(make-gpreg %al "al" 1 `((,%ax 0 1)))
(make-gpreg %bl "bl" 1 `((,%bx 0 1)))
(make-gpreg %cl "cl" 1 `((,%cx 0 1)))
(make-gpreg %dl "dl" 1 `((,%dx 0 1)))
(make-gpreg %bpl "bpl" 1 `((,%bp 0 1)))
(make-gpreg %spl "spl" 1 `((,%sp 0 1)))
(make-gpreg %sil "sil" 1 `((,%si 0 1)))
(make-gpreg %dil "dil" 1 `((,%di 0 1)))

;;;
;;; new x64 registers
;;;

(make-gpreg %r7 "r7" 8)
(make-gpreg %r8 "r8" 8)
(make-gpreg %r9 "r9" 8)
(make-gpreg %r10 "r10" 8)
(make-gpreg %r11 "r11" 8)
(make-gpreg %r12 "r12" 8)
(make-gpreg %r13 "r13" 8)
(make-gpreg %r14 "r14" 8)
(make-gpreg %r15 "r15" 8)

;; a bunch of aliases

(make-gpreg %r0L "r0L" 1 `((,%al 0 1)))
(make-gpreg %r1L "r1L" 1 `((,%cl 0 1)))
(make-gpreg %r2L "r2L" 1 `((,%dl 0 1)))
(make-gpreg %r3L "r3L" 1 `((,%bl 0 1)))
(make-gpreg %r4L "r4L" 1 `((,%spl 0 1)))
(make-gpreg %r5L "r5L" 1 `((,%bpl 0 1)))
(make-gpreg %r6L "r6L" 1 `((,%sil 0 1)))
(make-gpreg %r7L "r7L" 1 `((,%dil 0 1)))
(make-gpreg %r8L "r8L" 1 `((,%r8 0 1)))
(make-gpreg %r9L "r9L" 1 `((,%r9 0 1)))
(make-gpreg %r10L "r10L" 1 `((,%r10 0 1)))
(make-gpreg %r11L "r11L" 1 `((,%r11 0 1)))
(make-gpreg %r12L "r12L" 1 `((,%r12 0 1)))
(make-gpreg %r13L "r13L" 1 `((,%r13 0 1)))
(make-gpreg %r14L "r14L" 1 `((,%r14 0 1)))
(make-gpreg %r15L "r15L" 1 `((,%r15 0 1)))

(make-gpreg %r0W "r0W" 2 `((,%ax 0 2)))
(make-gpreg %r1W "r1W" 2 `((,%cx 0 2)))
(make-gpreg %r2W "r2W" 2 `((,%dx 0 2)))
(make-gpreg %r3W "r3W" 2 `((,%bx 0 2)))
(make-gpreg %r4W "r4W" 2 `((,%sp 0 2)))
(make-gpreg %r5W "r5W" 2 `((,%bp 0 2)))
(make-gpreg %r6W "r6W" 2 `((,%si 0 2)))
(make-gpreg %r7W "r7W" 2 `((,%di 0 2)))
(make-gpreg %r8W "r8W" 2 `((,%r8 0 2)))
(make-gpreg %r9W "r9W" 2 `((,%r9 0 2)))
(make-gpreg %r10W "r10W" 2 `((,%r10 0 2)))
(make-gpreg %r11W "r11W" 2 `((,%r11 0 2)))
(make-gpreg %r12W "r12W" 2 `((,%r12 0 2)))
(make-gpreg %r13W "r13W" 2 `((,%r13 0 2)))
(make-gpreg %r14W "r14W" 2 `((,%r14 0 2)))
(make-gpreg %r15W "r15W" 2 `((,%r15 0 2)))

(make-gpreg %r0D "r0D" 4 `((,%eax 0 4)))
(make-gpreg %r1D "r1D" 4 `((,%ecx 0 4)))
(make-gpreg %r2D "r2D" 4 `((,%edx 0 4)))
(make-gpreg %r3D "r3D" 4 `((,%ebx 0 4)))
(make-gpreg %r4D "r4D" 4 `((,%esp 0 4)))
(make-gpreg %r5D "r5D" 4 `((,%ebp 0 4)))
(make-gpreg %r6D "r6D" 4 `((,%esi 0 4)))
(make-gpreg %r7D "r7D" 4 `((,%edi 0 4)))
(make-gpreg %r8D "r8D" 4 `((,%r8 0 4)))
(make-gpreg %r9D "r9D" 4 `((,%r9 0 4)))
(make-gpreg %r10D "r10D" 4 `((,%r10 0 4)))
(make-gpreg %r11D "r11D" 4 `((,%r11 0 4)))
(make-gpreg %r12D "r12D" 4 `((,%r14 0 4)))
(make-gpreg %r13D "r13D" 4 `((,%r13 0 4)))
(make-gpreg %r14D "r14D" 4 `((,%r14 0 4)))
(make-gpreg %r15D "r15D" 4 `((,%r15 0 4)))

(defclass mem-ref (instr-arg)
  ((address :initarg :address
            :reader mem-ref.address)))

;;; The superclass of all instruction classes
(defclass instr ()
  ((name :initarg :name
         :reader instr.name)
   (repr :initarg :repr
         :reader instr.repr)))

(defclass nullary-op (instr)
  ())

(defclass unary-op (instr)
  ((op :initarg :op
       :reader unary-op.op)))

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

;; Quick way to define an instruction function that simply
;; instantiates an instruction class
(defmacro make-instr-interface (name class &rest lambda-list)
    (let ((ctor-args (apply #'append (map 'list (lambda (x) (list (intern (symbol-name x) :keyword) x)) lambda-list))))
        `(defun ,name ,lambda-list
             (make-instance (intern ,(symbol-name class) :x64) ,@ctor-args))))

(in-package :x64lisp-user)

(x64:make-instr-interface $ mem-ref address)
(x64:make-instr-interface cli @cli)
(x64:make-instr-interface add @add dst src)
(x64:make-instr-interface mov @mov dst src)
