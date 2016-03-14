
;;;; Defines the structures and functions for working with
;;;; control-flow graphs

(in-package :cfg)

(defstruct (cfg-node (:conc-name :cfg-node.))
  (instrs (make-array 0 :adjustable t :fill-pointer 0))
  (successors (make-array 0 :adjustable t :fill-pointer 0))
  (predecessors (make-array 0 :adjustable t :fill-pointer 0)))

(defun cfg-node.add-ref (from to)
    "Adds directed CFG edge from FROM to TO"
    (vector-push-extend-d to (cfg-node.successors from))
    (vector-push-extend-d from (cfg-node.predecessors to))
    (values))

(defun add-new-node-to-cfg (label-to-node &optional (name (make-label-name)))
    (let ((node (make-cfg-node)))
        (setf (gethash name label-to-node) node)
        node))

;;; Takes a ASM-PROC and returns two values: the root of the CFG and
;;; the list of basic blocks in the CFG.
(defun cfg-from-proc (proc)
    (loop for instr in (asm-proc.instrs proc)
       with label-to-node = (make-hash-table) ;; SYMBOL -> CFG-NODE
       with root = (make-cfg-node)
       with tail = (make-cfg-node)
       with current-node = root
       do
         (etypecase instr
           (tac:foc-op
            (vector-push-extend-d instr (cfg-node.instrs current-node))
            (let* ((jump-target-name (tac:instr-arg.repr (tac:foc-op.target instr)))
                   (target-bb (or (gethash jump-target-name label-to-node)
                                  (add-new-node-to-cfg label-to-node jump-target-name)))
                   (fallthrough-bb-name (tac:make-label-name "FT"))
                   (fallthrough-bb (add-new-node-to-cfg label-to-node fallthrough-bb-name)))
                ;; Add the instruction to the current basic block's list
                (cfg-node.add-ref current-node target-bb)
                ;; If we can fall through, then add an edge from
                ;; current bb to the fallthrough bb
                (when (foc-op.next-instr-implicit-target-p instr)
                    (cfg-node.add-ref current-node fallthrough-bb))
                ;; Continue processing the fallthrough bbs
                (setf current-node fallthrough-bb)))
           (tac:@label
            (let* ((label-name (tac:instr-arg.repr (tac:unary-op.op instr)))
                   (new-node (or (gethash label-name label-to-node)
                                 (add-new-node-to-cfg label-to-node label-name))))
                (vector-push-extend-d instr (cfg-node.instrs new-node))
                (cfg-node.add-ref current-node new-node)
                (setf current-node new-node)))
           (tac:@ret
            (vector-push-extend-d instr (cfg-node.instrs current-node))
            (cfg-node.add-ref current-node tail)
            (setf current-node (add-new-node-to-cfg label-to-node (tac:make-label-name "PR"))))
           (t (vector-push-extend-d instr (cfg-node.instrs current-node))))
       finally (progn
                   (vector-push-extend-d (tac:retz) (cfg-node.instrs tail))
                   (cfg-node.add-ref current-node tail)
                   (return (values root (list* root tail (hash-table-values label-to-node)))))))
