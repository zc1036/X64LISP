
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
               :reader reg.aliases-of)
   (repr :initarg :name)))

(defclass gpreg (reg)
  ())

(defmethod print-object ((x instr-arg) stream)
    (princ (instr-arg.repr x) stream))

(defmacro make-gpreg (name repr size &optional aliases-of)
    `(defparameter ,name (make-instance 'gpreg :name ,repr :size ,size :aliases-of ,aliases-of)))

(defmacro make-gpregs (&rest lists)
    `(progn
         ,@(mapcar (lambda (args) `(make-gpreg ,@args)) lists)))

(defclass mem-ref (instr-arg)
  ((address :initarg :address
            :reader mem-ref.address)))

;;; The superclass of all instruction classes
(defclass instr ()
  ((name :initarg :name
         :reader instr.name)
   (repr :initarg :repr
         :reader instr.repr)))

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

(defclass @label (nullary-op) ())

(defmethod instr.repr ((x @label))
    (format nil "~a:" (instr.name x)))

(defclass @jmp (unary-op)
  ((name :initform "jmp")))

(defclass @jne (unary-op)
  ((name :initform "jne")))

;; Quick way to define an instruction function that simply
;; instantiates an instruction class
(defmacro make-instr-interface (name class &rest lambda-list)
    (let ((ctor-args (apply #'append (map 'list (lambda (x) (list (intern (symbol-name x) :keyword) x)) lambda-list))))
        `(defun ,name ,lambda-list
             (let ((instr (make-instance (intern ,(symbol-name class) :x64) ,@ctor-args)))
                 (x64lisp:asm-proc.push-instr instr x64lisp:*current-proc*)
                 instr))))

(in-package :x64lisp-user)

;;;
;;; x86 registers and their 64-bit aliases
;;;
(x64:make-gpregs
 (%r0 "r0" 8)
 (%r1 "r1" 8)
 (%r2 "r2" 8)
 (%r3 "r3" 8)
 (%r4 "r4" 8)
 (%r5 "r5" 8)
 (%r6 "r6" 8)
 (%r7 "r7" 8)

 (%rax "rax" 8 `((,%r0 0 8)))
 (%rbx "rbx" 8 `((,%r3 0 8)))
 (%rcx "rcx" 8 `((,%r1 0 8)))
 (%rdx "rdx" 8 `((,%r2 0 8)))
 (%rbp "rbp" 8 `((,%r5 0 8)))
 (%rsp "rsp" 8 `((,%r4 0 8)))
 (%rsi "rsi" 8 `((,%r6 0 8)))
 (%rdi "rdi" 8 `((,%r7 0 8)))

 (%eax "eax" 4 `((,%rax 0 4)))
 (%ebx "ebx" 4 `((,%rbx 0 4)))
 (%ecx "ecx" 4 `((,%rcx 0 4)))
 (%edx "edx" 4 `((,%rdx 0 4)))
 (%ebp "ebp" 4 `((,%rbp 0 4)))
 (%esp "esp" 4 `((,%rsp 0 4)))
 (%esi "esi" 4 `((,%rsi 0 4)))
 (%edi "edi" 4 `((,%rdi 0 4)))

 (%ax "ax" 2 `((,%eax 0 2)))
 (%bx "bx" 2 `((,%ebx 0 2)))
 (%cx "cx" 2 `((,%ecx 0 2)))
 (%dx "dx" 2 `((,%edx 0 2)))
 (%bp "bp" 2 `((,%ebp 0 2)))
 (%sp "sp" 2 `((,%esp 0 2)))
 (%si "si" 2 `((,%esi 0 2)))
 (%di "di" 2 `((,%edi 0 2)))

 (%ah "ah" 1 `((,%ax 1 2)))
 (%bh "bh" 1 `((,%bx 1 2)))
 (%ch "ch" 1 `((,%cx 1 2)))
 (%dh "dh" 1 `((,%dx 1 2)))
 (%bph "bph" 1 `((,%bp 1 2)))
 (%sph "sph" 1 `((,%sp 1 2)))
 (%sih "sih" 1 `((,%si 1 2)))
 (%dih "dih" 1 `((,%di 1 2)))

 (%al "al" 1 `((,%ax 0 1)))
 (%bl "bl" 1 `((,%bx 0 1)))
 (%cl "cl" 1 `((,%cx 0 1)))
 (%dl "dl" 1 `((,%dx 0 1)))
 (%bpl "bpl" 1 `((,%bp 0 1)))
 (%spl "spl" 1 `((,%sp 0 1)))
 (%sil "sil" 1 `((,%si 0 1)))
 (%dil "dil" 1 `((,%di 0 1)))

;;;
;;; new x64 registers
;;;

 (%r7 "r7" 8)
 (%r8 "r8" 8)
 (%r9 "r9" 8)
 (%r10 "r10" 8)
 (%r11 "r11" 8)
 (%r12 "r12" 8)
 (%r13 "r13" 8)
 (%r14 "r14" 8)
 (%r15 "r15" 8)

 ;; a bunch of aliases

 (%r0L "r0L" 1 `((,%al 0 1)))
 (%r1L "r1L" 1 `((,%cl 0 1)))
 (%r2L "r2L" 1 `((,%dl 0 1)))
 (%r3L "r3L" 1 `((,%bl 0 1)))
 (%r4L "r4L" 1 `((,%spl 0 1)))
 (%r5L "r5L" 1 `((,%bpl 0 1)))
 (%r6L "r6L" 1 `((,%sil 0 1)))
 (%r7L "r7L" 1 `((,%dil 0 1)))
 (%r8L "r8L" 1 `((,%r8 0 1)))
 (%r9L "r9L" 1 `((,%r9 0 1)))
 (%r10L "r10L" 1 `((,%r10 0 1)))
 (%r11L "r11L" 1 `((,%r11 0 1)))
 (%r12L "r12L" 1 `((,%r12 0 1)))
 (%r13L "r13L" 1 `((,%r13 0 1)))
 (%r14L "r14L" 1 `((,%r14 0 1)))
 (%r15L "r15L" 1 `((,%r15 0 1)))

 (%r0W "r0W" 2 `((,%ax 0 2)))
 (%r1W "r1W" 2 `((,%cx 0 2)))
 (%r2W "r2W" 2 `((,%dx 0 2)))
 (%r3W "r3W" 2 `((,%bx 0 2)))
 (%r4W "r4W" 2 `((,%sp 0 2)))
 (%r5W "r5W" 2 `((,%bp 0 2)))
 (%r6W "r6W" 2 `((,%si 0 2)))
 (%r7W "r7W" 2 `((,%di 0 2)))
 (%r8W "r8W" 2 `((,%r8 0 2)))
 (%r9W "r9W" 2 `((,%r9 0 2)))
 (%r10W "r10W" 2 `((,%r10 0 2)))
 (%r11W "r11W" 2 `((,%r11 0 2)))
 (%r12W "r12W" 2 `((,%r12 0 2)))
 (%r13W "r13W" 2 `((,%r13 0 2)))
 (%r14W "r14W" 2 `((,%r14 0 2)))
 (%r15W "r15W" 2 `((,%r15 0 2)))

 (%r0D "r0D" 4 `((,%eax 0 4)))
 (%r1D "r1D" 4 `((,%ecx 0 4)))
 (%r2D "r2D" 4 `((,%edx 0 4)))
 (%r3D "r3D" 4 `((,%ebx 0 4)))
 (%r4D "r4D" 4 `((,%esp 0 4)))
 (%r5D "r5D" 4 `((,%ebp 0 4)))
 (%r6D "r6D" 4 `((,%esi 0 4)))
 (%r7D "r7D" 4 `((,%edi 0 4)))
 (%r8D "r8D" 4 `((,%r8 0 4)))
 (%r9D "r9D" 4 `((,%r9 0 4)))
 (%r10D "r10D" 4 `((,%r10 0 4)))
 (%r11D "r11D" 4 `((,%r11 0 4)))
 (%r12D "r12D" 4 `((,%r14 0 4)))
 (%r13D "r13D" 4 `((,%r13 0 4)))
 (%r14D "r14D" 4 `((,%r14 0 4)))
 (%r15D "r15D" 4 `((,%r15 0 4))))

(x64:make-instr-interface $ mem-ref address)
(x64:make-instr-interface cli @cli)
(x64:make-instr-interface add @add dst src)
(x64:make-instr-interface mov @mov dst src)

(x64:make-instr-interface label @label name)
(x64:make-instr-interface jmp @jmp op)
(x64:make-instr-interface jne @jne op)