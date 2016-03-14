
;;;; A small lisp for writing x64 assembly in C-like style
;;;
;;; x64lisp supports at least the following:
;;; - Functions with variables
;;; - Arbitrary memory/register access
;;; - Structs, unions
;;; - While, for loops
;;; - Function pointers
;;; - Basic C integer types (int8/16/32/64)
;;; - Direct access to assembly instructions
;;; - And all the compile-time power that comes with lisp
;;;

(in-package :x64lisp)

(defun load-file (filename)
    (load filename))

(defun get-node-name (node node-names)
    (let ((name (gethash node node-names)))
        (if name
            (values name t)
            (let ((new-node-name (gensym)))
                (setf (gethash node node-names) new-node-name)
                (values new-node-name nil)))))

(defun print-cfg-to-dot (root node-names visited-nodes)
    (unless (gethash root visited-nodes)
        (let ((node-name (get-node-name root node-names)))
            (setf (gethash root visited-nodes) t)
            (format t "    ~a [shape=rectangle, label=\"" node-name)
            (map nil (bind #'format t "~a\\l") (cfg-node.instrs root))
            (format t "\"];~%")
            (loop for succ being the elements of (cfg-node.successors root) do
                 (format t "    ~a -> ~a;~%" node-name (get-node-name succ node-names))
                 (print-cfg-to-dot succ node-names visited-nodes)))))

(defun load-files (filenames)
    (loop for filename in filenames do
         (with-backtrace-guard (format nil "file ~a" filename)
             (load-file filename)
             (with-open-file (stream (concatenate 'string filename ".s") :direction :output :if-exists :supersede)
                 (loop for proc in (asm-module.procs *current-module*) do
                      (with-slots ((instrs core-structures::instrs)
                                   (name core-structures::name)
                                   (thunk core-structures::thunk)) proc
                          (setf instrs (flatten (ast:instr-result.instrs (funcall thunk))))

                          (format t "Procedure ~a instructions:~%" name)
                          (map nil (bind #'format t "    ~a~%") instrs)

                          (format t "Procedure ~a BB graph:~%" name)

                          (format t "digraph blocks {~%")
                          (multiple-value-bind (root nodes) (cfg-from-proc proc)
                              (declare (ignore root))
                              (let ((node-names (make-hash-table))
                                    (visited-nodes (make-hash-table)))
                                  (mapc (lambda (x) (print-cfg-to-dot x node-names visited-nodes)) nodes))))
                          (format t "}~%")
                    ;; process (ASM-PROC.INSTRS PROC) here
                      ))
             (setf *current-module* nil))))
