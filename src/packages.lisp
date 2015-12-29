
(defpackage :macro-assist
  (:use :cl)
  (:export :with-gensyms))

(defpackage :math
  (:use :cl)
  (:export :ceil-to-nearest-multiple))

(defpackage :functional
  (:use :cl :macro-assist)
  (:export :map-accum
           :bind
           :destructuring-lambda))

(defpackage :x64lisp
  (:use :cl :macro-assist :functional :math)
  (:export :require-toplevel
           :require-toplevel-module
           :require-not-toplevel

           :asm-proc
           :asm-proc.push-instr
           :asm-proc.instrs

           :asm-module
           :asm-module.procs
           :asm-module.name

           :assembly-error
           :size-of-sizeless-type
           :alignment-of-sizeless-type
           :unexpected-toplevel-form
           :unexpected-scoped-form

           :*asm-modules*
           :*current-module*
           :*current-proc*
           :*is-toplevel*

           :load-files))

(defpackage :types
  (:use :cl :macro-assist :functional :math :x64lisp)
  (:export :btype.equalp
           :btype.alignment
           :btype.size

           :void-type
           :int-type
           :int-type.signed
           :ptr-type.pointee-type
           :array-type
           :array-type.element-type
           :array-type.element-count
           :struct-type
           :struct-type.fields
           :struct-type.name
           :struct-type.size-and-field-offsets
           :union-type
           :union-type.fields
           :proc-type
           :proc-type.ret-type
           :proc-type.arg-types

           :type-assert
           :type-error

           :void
           :int8
           :int16
           :int32
           :int64
           :uint8
           :uint16
           :uint32
           :uint64))

(defpackage :ast
  (:use :cl :macro-assist :functional)
  (:import-from :types :void)
  (:export :ast-expr
           :defstatement))

(defpackage :core-forms
  (:use :cl :macro-assist :x64lisp :types)
  (:export :module
           :struct
           :proc
           :ast))

(defpackage :cfg
  (:use :cl :x64lisp :macro-assist))

(defpackage :instructions
  (:use :cl :macro-assist :ast)
  (:import-from :types :void)
  (:export :%r0 :%r1 :%r2 :%r3 :%r4 :%r5 :%r6 :%r7 :%r8 :%r9 :%r10 :%r11 :%r12
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

           :cli
           :add
           :move
           :label
           :jmp
           :jne))

(defpackage :x64lisp-user
  (:use :cl :core-forms :types :instructions))
