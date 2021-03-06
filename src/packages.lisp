
(defpackage :macro-assist
  (:use :cl)
  (:export :with-gensyms))

(defpackage :math
  (:use :cl)
  (:export :ceil-to-nearest-multiple))

(defpackage :data-structures
  (:use :cl :macro-assist)
  (:export :list-builder
           :list-builder.head
           :list-builder.push-end
           :list-builder.push-begin

           :vector-push-extend-d))

(defpackage :functional
  (:use :cl :macro-assist :data-structures)
  (:export :map-accum
           :bind
           :destructuring-lambda
           :flatten
           :multiple-value-identity
           :compose
           :elet
           :multiple-with-slots
           :hash-table-values
           :hash-table-keys))

(defpackage :conditions
  (:use :cl :macro-assist)
  (:export :assembly-error
           :internal-compiler-error
           :with-backtrace-guard

           :assembly-error
           :assembly-error.text
           :assembly-error.backtrace-reports
           :with-backtrace-guard
           :make-backtrace-report
           :backtrace-report.form-name))

(defpackage :source-readtable
  (:use :cl)
  (:export :x64lisp-readtable))

(defpackage :types
  (:use :cl :macro-assist :functional :math :conditions)
  (:export :btype
           :btype.equalp
           :btype.alignment
           :btype.size

           :size-of-sizeless-type
           :alignment-of-sizeless-type
           :nonexistent-struct-field

           :void-type
           :int-type
           :int-type.signed
           :int-type.lisp-typespec
           :ptr-type.pointee-type
           :array-type
           :array-type.element-type
           :array-type.element-count
           :struct-field-info
           :struct-field-info.name
           :struct-field-info.type
           :struct-field-info=
           :struct-type
           :struct-type.field
           :struct-type.fields
           :struct-type.name
           :struct-type.size-and-field-offsets
           :struct-type.field-offset
           :union-type
           :union-type.fields
           :proc-type
           :proc-type.ret-type
           :proc-type.arg-types

           :type-assert
           :asm-type-error

           :void
           :int8
           :int16
           :int32
           :int64
           :uint8
           :uint16
           :uint32
           :uint64
           :integral-types
           :common-type))

(defpackage :ast
  (:use :cl :macro-assist :functional :conditions)
  (:import-from :types
                :btype
                :void
                :integral-types
                :int-type.lisp-typespec
                :asm-type-error
                :btype.size
                :common-type)
  (:export :ast-expr
           :ast-expr.to-instructions
           :ast-expr.type
           :ast-expr.lvalue-p

           :var
           :var.type
           :var.name

           :def-form
           :def-generic-expr
           :def-expr-instance
           :def-expr-instance-before
           :+declared-type+

           :instr-result
           :make-instr-result
           :instr-result.instrs
           :instr-result.reg))

(defpackage :tac
  (:use :cl :macro-assist :functional)
  (:import-from :types :void)
  (:export :vreg
           :new-vreg

           :with-labels
           :make-label-name

           :instr :instr.repr

           :instr-arg :instr-arg.repr

           :unary-op :unary-op.op

           :foc-op :foc-op.target :foc-op.next-instr-implicit-target-p

           :@label :@ret :@retz

           :$ :add :move :member-access :lbl :jump :j-not-equal :j-zero :ret :retz))

(defpackage :x64
  (:use :cl :macro-assist :functional :ast :conditions)
  (:import-from :types :void :uint8 :uint16 :uint32 :uint64)
  (:export :instr-arg
           :reg
           :gpreg

           :%r0 :%r1 :%r2 :%r3 :%r4 :%r5 :%r6 :%r7 :%r8 :%r9 :%r10 :%r11 :%r12
           :%r13 :%r14 :%r15

           :%r0L :%r1L :%r2L :%r3L :%r4L :%r5L :%r6L :%r7L
           :%r8L :%r9L :%r10L :%r11L :%r12L :%r13L :%r14L :%r15L

           :%r0W :%r1W :%r2W :%r3W :%r4W :%r5W :%r6W :%r7W
           :%r8W :%r9W :%r10W :%r11W :%r12W :%r13W :%r14W :%r15W

           :%r0D :%r1D :%r2D :%r3D :%r4D :%r5D :%r6D :%r7D
           :%r8D :%r9D :%r10D :%r11D :%r12D :%r13D :%r14D :%r15D

           :%rax :%rbx :%rcx :%rdx :%rbp :%rsp :%rsi :%rdi
           :%eax :%ebx :%ecx :%edx :%ebp :%esp :%esi :%edi
           :%ax :%bx :%cx :%dx :%bp :%sp :%si :%di
           :%ah :%bh :%ch :%dh :%bph :%sph :%sih :%dih
           :%al :%bl :%cl :%dl :%bpl :%spl :%sil :%dil

           :def :cli :add :mov :tst :label :jmp :jne :jz :mac :lbl))

(defpackage core-structures
  (:use :cl :conditions :ast :types)
  (:export :require-toplevel
           :require-toplevel-module
           :require-procedure

           :asm-proc
           :asm-proc.push-instr
           :asm-proc.instrs

           :asm-module
           :asm-module.procs
           :asm-module.name

           :*asm-modules*
           :*current-module*
           :*current-proc*
           :*is-toplevel*))

(defpackage :cfg
  (:use :cl :macro-assist :functional :data-structures :tac :core-structures)
  (:export :cfg-node
           :cfg-node.instrs
           :cfg-node.successors
           :cfg-node.predecessors

           :cfg-from-proc))

(defpackage :core-conditions
  (:use :cl :conditions)
  (:export :assignment-to-non-lvalue
           :malformed-struct-definition
           :malformed-let-binding

           :unexpected-toplevel-form
           :unexpected-scoped-form))

(defpackage :core-forms
  (:use :cl :macro-assist :functional :core-structures
        :conditions :core-conditions :types :ast)
  (:import-from :optima :match)
  (:export :module
           :struct
           :proc
           :while
           :let-var

           :binary-op+
           :operator+
           :binary-op=
           :operator=
           :binary-member-access
           :operator-member-access))

(defpackage :operator-nicknames
  (:use :cl :macro-assist)
  (:shadow :+ := :let)
  (:export :+ := :let))

(defpackage :x64lisp-user
  (:use :types :operator-nicknames)
  (:import-from :core-forms
                :module
                :struct
                :proc
                :while))

(defpackage :x64lisp
  (:use :cl :macro-assist :functional :conditions :math :core-structures :cfg)
  (:export :load-files))
