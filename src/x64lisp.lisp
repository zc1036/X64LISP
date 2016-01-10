
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

                          (map nil (bind #'format t "    ~a~%") instrs))
                    ;; process (ASM-PROC.INSTRS PROC) here
                      ))
             (setf *current-module* nil))))
