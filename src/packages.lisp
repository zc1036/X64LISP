
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

(defpackage :x64
  (:use :cl)
  (:export :reg
           :gpreg
           :instr-arg
           :instr

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

           :mem-ref

           :@cli
           :cli
           :@add
           :add

           :make-instr-interface))
