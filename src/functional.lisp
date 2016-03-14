
;;;; Functional utilities

(in-package :functional)

(defun map-accum* (state f list accum)
    (if list
        (multiple-value-bind (new-state list-item) (funcall f state (car list))
            (map-accum* new-state f (cdr list) (cons list-item accum)))
        (values state (nreverse accum))))

;; map-accum :: init-state :: a ->
;;              f :: (a, b) -> (a, c),
;;              list :: [b] -> (a, [c])
(defun map-accum (init-state f list)
    (map-accum* init-state f list (list)))

(defun multiple-value-identity (&rest args) (values-list args))

(defmacro bind (func &rest args)
    `(lambda (&rest restargs)
         (apply ,func (append (list ,@args) restargs))))

(defmacro destructuring-lambda (lambda-list &body body)
    (with-gensyms (args-sym)
        `(lambda (&rest ,args-sym)
             (destructuring-bind ,lambda-list ,args-sym
                 ,@body))))

(defun flatten (list &optional accum resume)
    "Tail-recursive list flatten; removes NILs."
    (declare (optimize (debug 1) (safety 1) (speed 3)))
    (if list
        (ctypecase (car list)
          (null (flatten (cdr list) accum resume))
          (atom (flatten (cdr list) (cons (car list) accum) resume))
          (cons (flatten (car list) accum (cons (cdr list) resume))))
        (if resume
            (flatten (car resume) accum (cdr resume))
            (reverse accum))))

;; Like Haskell's init function
;; LOOP incantation from http://stackoverflow.com/a/10636649/5451968
(defun init (list)
    (loop for x on list while (cdr x) collect (car x)))

(defun compose* (funcs carry)
    (if funcs
        (let ((func (car funcs)))
            (compose* (cdr funcs)
                      (lambda (&rest args)
                          (apply carry (multiple-value-list (apply func args))))))
        carry))

(defun compose (&rest funcs)
    (compose* funcs #'multiple-value-identity))

(defun elet* (bindings body)
    (if (null bindings)
        `(progn ,@body)
        `(multiple-value-bind ,(init (car bindings)) ,(car (last (car bindings)))
             ,(elet* (cdr bindings) body))))

(defmacro elet (bindings &body body)
    (elet* bindings body))

(defun multiple-with-slots* (bindings body)
    (if (null bindings)
        `(progn ,@body)
        `(with-slots ,(init (car bindings)) ,(car (last (car bindings)))
             ,(multiple-with-slots* (cdr bindings) body))))

(defmacro multiple-with-slots (bindings &body body)
    (multiple-with-slots* bindings body))

(defun progall* (body accum-sym)
    (if body
        `((list-builder.push-end ,accum-sym ,(car body))
          ,@(progall* (cdr body) accum-sym))
        `((list-builder.head ,accum-sym))))

;; Like PROGN but returns a list of all evaluation results
(defmacro progall (&body body)
    (with-gensyms (accum-sym)
        `(let ((,accum-sym (make-list-builder)))
             ,@(progall* body accum-sym))))

(defmacro if-let (bindings then &optional else)
    (let ((binding-names (mapcar #'car bindings)))
        `(let ,bindings
             (if (and ,@binding-names)
                 ,then
                 ,else))))

(defun hash-table-keys (table)
    (let ((values nil))
        (with-hash-table-iterator (get-node table)
            (loop
               (multiple-value-bind (continue-p key value) (get-node)
                   (declare (ignore value))
                   (if continue-p
                       (setf values (cons key values))
                       (return values)))))))

(defun hash-table-values (table)
    (let ((values nil))
        (with-hash-table-iterator (get-node table)
            (loop
               (multiple-value-bind (continue-p key value) (get-node)
                   (declare (ignore key))
                   (if continue-p
                       (setf values (cons value values))
                       (return values)))))))
