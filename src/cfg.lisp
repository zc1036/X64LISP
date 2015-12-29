
;;;; Defines the structures and functions for working with
;;;; control-flow graphs

(in-package :cfg)

(defstruct (cfg-node :conc-name :cfg-node.)
  (instrs (make-array 0 :adjustable t :fill-pointer 0)))

;; Takes a ASM-PROC and returns a CFG-NODE
(defun cfg-from-proc (proc)
    (let ((instrs (asm-proc.instrs proc))
          (root (make-cfg-node)))
        ))
