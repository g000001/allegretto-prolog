(cl:in-package cl-user)


#-ccl
(declaim (optimize (speed 3) (safety 0) (compilation-speed 0)
                   (debug 0)))


(defpackage :allegro-moderato
  (:use :cl)
  (:shadow #:symbol)
  (:shadowing-import-from prolog var-tag unbound)
  ;;(:shadowing-import-from prolog make-anonymous anonymous-variables-in anon-vars-in)
  (:export #:?-
           #:<-
           #:<--
           #:?
           #:fail
           #:true
           #:call
           #:!
           #:and
           #:or
           #:if
           #:catch
           #:throw
           #:=
           #:unify-with-occurs-check
           #:\\=
           #:var
           #:atom
           #:integer
           #:real
           #:atomic
           #:compound
           #:nonvar
           #:number
           #:==
           #:\\==
           #:@<
           #:@=<
           #:@>2
           #:@>=
           #:functor
           #:arg
           #:copy-term
           #:is
           #:|=:=|
           #:=\\=
           #:<
           #:=<
           #:>
           #:>=
           #:clause
           #:current-predicate
           #:asserta
           #:assertz
           #:retract
           #:abolish
           #:findall
           #:bagof
           #:current-input
           #:current-output
           #:set-input
           #:set-output
           #:close
           #:flush-output
           #:stream-property
           #:at-end-of-stream
           #:set-stream-position
           #:get-char
           #:put-char
           #:nl
           #:get-code
           #:put-code
           #:read
           #:write
           #:fail-if
           #:once
           #:repeat
           #:atom-length
           #:atom-concat
           #:sub-atom
           #:atom-chars
           #:atom-codes
           #:atom-characters
           #:string-atom
           #:string-list
           #:char-code
           #:number-chars
           #:number-codes
           #:lisp
           #:prolog-compile-symbols
           #:prolog
           #:prolog*
           #:prolog-collect
           #:prolog-first))


;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; Code from Paradigms of AI Programming
;;; Copyright (c) 1991 Peter Norvig

;;; File auxfns.lisp: Auxiliary functions used by all other programs
;;; Load this file before running any other programs.

(in-package "ALLEGRO-MODERATO")


(defmacro nlet (name bindspec &body body)
  (let ((gs (loop :for nil :in bindspec :collect (gensym)))
        (gname (gensym "name"))
        (gblock (gensym "block")))
    `(macrolet ((,name ,gs
                  `(progn
                     (psetq
                      ,@(apply #'nconc
                               (mapcar #'list ',(mapcar #'car bindspec)
                                       (list ,@gs))))
                     (go ,',gname))))
       (block ,gblock
         (let ,bindspec
           (tagbody
            ,gname (return-from
                       ,gblock (progn ,@body))))))))


(defmacro tail-recursive-defun (name (&rest args) &body body)
  `(defun ,name (,@args)
     (nlet ,name (,@(mapcar (lambda (v) `(,v ,v)) args))
       ,@body)))


(defmacro fast (&body body)
  `(locally
       #+(or sbcl allegro) (declare (optimize (speed 3) (safety 0)))
       ,@body))


(defun ignorer (&rest args)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (ignore args))
  nil)


;;;; Macros (formerly in auxmacs.lisp: that file no longer needed)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun find-anywhere (item tree)
    "Does item occur anywhere in tree?"
    (if (atom tree)
	(if (eql item tree) tree)
	(or (find-anywhere item (first tree))
	    (find-anywhere item (rest tree)))))

  (defun starts-with (list x)
    "Is x a list whose first element is x?"
    (and (consp list) (eql (first list) x)))
  )


;;;; Auxiliary Functions

(setf (symbol-function 'find-all-if) #'remove-if-not)


(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence 
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))


'(loop :for s :being :the :symbols :of :prolog
      :when (compiler-macro-function s)
        :collect s)
'(;;PROLOG::DEREF-EXP
  ;;PROLOG::VAR-P
  PROLOG::WHEN-UNIFY!
  PROLOG::CONS-ANON-VAR-P
  ;;PROLOG::UNBOUND-VAR-P
  PROLOG::LET-DE
  PROLOG::DE-CONSIFY
  PROLOG::ADD-CLAUSE
  PROLOG::NON-ANON-BOUND-P
  ;;PROLOG::SET-VAR-BINDING
  ;;PROLOG::BOUND-P
  PROLOG::LENGTH=1
  PROLOG::UNIFY!
  PROLOG::CONS-VAR-P
  ;;PROLOG:?
  )



;;; NOTE: In ANSI Common Lisp, the effects of adding a definition (or most
;;; anything else) to a symbol in the common-lisp package is undefined.
;;; Therefore, it would be best to rename the function SYMBOL to something 
;;; else.  This has not been done (for compatibility with the book).  

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun symbol (&rest args)
  "Concatenate symbols or strings to form an interned symbol"
  (with-standard-io-syntax
    (intern (format nil "~:@(~{~A~}~)" args) "ALLEGRO-MODERATO")))

(defun new-symbol (&rest args)
  "Concatenate symbols or strings to form an uninterned symbol"
  (with-standard-io-syntax
    (make-symbol (format nil "~{~A~}" args)))))


(defvar *anonymous-var* prolog::*anonymous-var*)


(declaim (type fixnum *var-counter*))


(defvar *var-counter* 0)


'''(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant unbound (if (boundp 'unbound)
			 (symbol-value 'unbound)
			 "Unbound"))

(defconstant var-tag
  (if (boundp 'var-tag)
      (symbol-value 'var-tag)
      (load-time-value "PrologVar"))))


(deftype var ()
  `(cons (eql ,var-tag) T))


(defun var-p (obj)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (and (consp obj)
       (eq var-tag (car obj))))


(define-compiler-macro var-p (var)
  (let ((x (gensym)))
    `(fast
       (let ((,x ,var))
         (and (consp ,x)
              (eq (car ,x) (load-time-value var-tag)))))))


#|
;; disassembly of #<Function VAR-P>
;; formals: PROLOG::X
;; constant vector:
0: "PrologVar"

;; code start: #x10003d3e630:
   0: 48 89 f8       movq	%RAX,%RDI
   3: 48 83 e0 0f    and	%RAX,$15
   7: 48 83 f8 01    cmp	%RAX,$1
  11: 74 0a          jZ	23
  13: 4c 89 ff       movq	%RDI,%R15
  16: f8             clc
  17: 4c 8b 74 24 10 movq	%R14,[%RSP+16]
  22: c3             ret
  23: 4c 8b 6f ef    movq	%R13,[%RDI-17]
  27: 4d 3b 6e 36    cmpq	%R13,[%R14+54]  ; "PrologVar"
  31: 49 8d 7f 82    leaq	%RDI,[%R15-126] ; T
  35: 49 0f 45 ff    cmovnzq	%RDI,%R15
  39: f8             clc
  40: eb e7          jmp	17

|#


(defun ? ()
  (cons var-tag unbound))


(define-compiler-macro ? ()
  `(cons (load-time-value var-tag)
         (load-time-value unbound)))


(defmacro var-binding (var)
  `(cdr ,var))


(defun var-name (var)
  (cdr var))


#|
;; disassembly of #<Function BOUND-P>
;; formals: PROLOG:VAR
;; constant vector:
0: {Anon}
1: "Unbound"

;; code start: #x10003d3dfd0:
   0: 48 83 ec 68    sub	%RSP,$104
   4: 4c 89 74 24 08 movq	[%RSP+8],%R14
   9: 48 83 f8 08    cmp	%RAX,$8
  13: 74 01          jZ	16
  15: 06             (push es)          ; SYS::TRAP-ARGERR
  16: 41 80 7f a7 00 cmpb	[%R15-89],$0    ; SYS::C_INTERRUPT-PENDING
  21: 74 01          jZ	24
  23: 17             (pop ss)           ; SYS::TRAP-SIGNAL-HIT
  24: 49 3b 7e 36    cmpq	%RDI,[%R14+54]  ; {Anon}
  28: 4d 8d 6f 82    leaq	%R13,[%R15-126] ; T
  32: 4d 0f 45 ef    cmovnzq	%R13,%R15
  36: 4d 39 ef       cmpq	%R15,%R13
  39: 74 1f          jZ	72
  41: 4d 39 ef       cmpq	%R15,%R13
  44: 49 8d 7f 82    leaq	%RDI,[%R15-126] ; T
  48: 49 0f 45 ff    cmovnzq	%RDI,%R15
  52: f8             clc
  53: 4c 8b 74 24 78 movq	%R14,[%RSP+120]
  58: 4c 89 bc 24 88 movq	[%RSP+136],%R15
      00 00 00 
  66: 48 8d 64 24 68 leaq	%RSP,[%RSP+104]
  71: c3             ret
  72: 41 ff 57 67    call	*[%R15+103]     ; SYS::QCDR
  76: 49 3b 7e 3e    cmpq	%RDI,[%R14+62]  ; "Unbound"
  80: 4d 8d 6f 82    leaq	%R13,[%R15-126] ; T
  84: 4d 0f 45 ef    cmovnzq	%R13,%R15
  88: eb cf          jmp	41

|#

;(declaim (inline bound-p))
;(unintern 'bound-p 'allegro-moderato)
(defun bound-p (var)
  (not (or ;;(eq var (load-time-value *anonymous-var*))
           (eq (var-binding var)
               (load-time-value unbound)))))


(define-compiler-macro bound-p (var)
  `(fast
     (not (or ;;(eq ,var (load-time-value *anonymous-var*))
              (eq (var-binding ,var)
                  (load-time-value unbound))))))


(defmacro deref (exp)
  "Follow pointers for bound variables."
  `(progn
     (loop :while (and (var-p ,exp) (bound-p ,exp))
           :do (setf ,exp (var-binding ,exp)))
     ,exp))


(defmacro deref-fail-return (block-tag exp)
  `(fast
    (loop :while (var-p 8)
          :do (unless (bound-p 8) (return-from ,block-tag))
              (setq ,exp (var-binding ,exp)))
    ,exp))


(defmacro deref-fail (exp)
  "Follow pointers for bound variables."
  `(progn
     (loop :while (and (var-p ,exp) (bound-p ,exp))
           :do (setf ,exp (var-binding ,exp)))
     (or ,exp (throw 'deref-fail ,exp))))


(defun print-var (var stream depth)
  (if (or (and *print-level*
               (>= depth *print-level*))
          (var-p (deref var)))
      (format stream "?~A" (var-name var))
      (write var :stream stream)))


(unless (boundp 'null-var)
  (defconstant null-var (?)))


;;;; TRAIL
(declaim (type list *trail*))


(deftype adim ()
  `(integer 0 ,(1- array-total-size-limit)))


(defvar *trail* nil)


(defconstant default-trail-size 512)


(defun allocate-trail-vec (size)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (the simple-vector (make-array size :initial-element nil)))


(defun allocate-trail (size)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (the cons (cons 0 (allocate-trail-vec size))))


(defmacro trail-ndx (trail)
  `(the adim (car ,trail)))


(defmacro trail-vec (trail)
  `(the simple-vector (cdr ,trail)))


(defun grow-prolog-trail (trail)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((size (length (trail-vec trail)))
         (new-trail-vec (allocate-trail-vec (* 2 size))))
    (declare (simple-vector new-trail-vec))
    (setf (trail-vec trail)
          (replace new-trail-vec (trail-vec trail)))
    new-trail-vec))


(defmacro vec-push (trail val)
  (let ((gval (gensym)))
    `(fast
       (let* ((trail ,trail)
              (,gval ,val)
              (ndx (trail-ndx trail))
              (vec (trail-vec trail))
              (len (length (the simple-vector vec))))
         (declare (type adim ndx len) (simple-vector vec))
         (when (>= ndx len) (setq vec (grow-prolog-trail trail)))
         (prog1 (setf (svref vec ndx) (the t ,gval))
           (setf (trail-ndx trail) (1+ ndx)))))))


;;;; PATTERN MATCHING FACILITY

(defconstant fail nil)


(defconstant no-bindings (if (boundp 'no-bindings)
			     (symbol-value 'no-bindings)
			     '((t . t))))


(defun match-variable (var input bindings)
  "Does VAR match input?  Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))


(defun make-binding (var val) (cons var val))


(defun binding-var (binding)
  "Get the variable part of a single binding."
  (car binding))


(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))


(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))


(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))


(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy no-bindings
        (if (eq bindings no-bindings)
            nil
            bindings)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
    (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))

  (defun reuse-cons (x y x-y)
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))
  
  (defun length=1 (x) 
  "Is x a list of length 1?"
    (and (consp x) (null (cdr x))))
  (define-compiler-macro length=1 (x)
    `(and (consp ,x) (null (cdr ,x))))
  )


(defun find-if-anywhere (predicate tree)
  "Does predicate apply to any atom in the tree?"
  (if (atom tree)
      (funcall predicate tree)
      (or (find-if-anywhere predicate (first tree))
          (find-if-anywhere predicate (rest tree)))))


;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File unify.lisp: Unification functions

(in-package "ALLEGRO-MODERATO")


(defparameter *occurs-check* nil "Should we do the occurs check?")


(defun unify (x y &optional (bindings no-bindings))
  "See if x and y match with given bindings."
  (cond ((eq bindings fail) fail)
        ((eql x y) bindings)
        ((variable-p x) (unify-variable x y bindings))
        ((variable-p y) (unify-variable y x bindings))
        ((and (consp x) (consp y))
         (unify (rest x) (rest y) 
                (unify (first x) (first y) bindings)))
        (t fail)))


(defun unify-variable (var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  (cond ((get-binding var bindings)
         (unify (lookup var bindings) x bindings))
        ((and (variable-p x) (get-binding x bindings))
         (unify var (lookup x bindings) bindings))
        ((and *occurs-check* (occurs-check var x bindings))
         fail)
        (t (extend-bindings var x bindings))))


(defun occurs-check (var x bindings)
  "Does var occur anywhere inside x?"
  (cond ((eq var x) t)
        ((and (variable-p x) (get-binding x bindings))
         (occurs-check var (lookup x bindings) bindings))
        ((consp x) (or (occurs-check var (first x) bindings)
                       (occurs-check var (rest x) bindings)))
        (t nil)))


;;; ==============================

(defun subst-bindings (bindings x)
  "Substitute the value of variables in bindings into x,
  taking recursively bound variables into account."
  (cond ((eq bindings fail) fail)
        ((eq bindings no-bindings) x)
        ((and (variable-p x) (get-binding x bindings))
         (subst-bindings bindings (lookup x bindings)))
        ((atom x) x)
        (t (reuse-cons (subst-bindings bindings (car x))
                       (subst-bindings bindings (cdr x))
                       x))))


;;; ==============================

(defun unifier (x y)
 "Return something that unifies with both x and y (or fail)."
 (subst-bindings (unify x y) x))


;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File prolog.lisp: prolog from (11.3), with interactive backtracking.

(in-package "ALLEGRO-MODERATO")


;;;; does not include destructive unification (11.6); see prologc.lisp

;; clauses are represented as (head . body) cons cells
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun clause-head (clause) (first clause))
  (defun clause-body (clause) (rest clause))
  ;; clauses are stored on the predicate's plist
  (defun get-clauses (pred) (get pred 'clauses))
  (defun predicate (relation)
    (if (atom relation)
        relation
        (first relation)))
  (defun args (x)
    "The arguments of a relation"
    (if (atom x)
        nil
        (rest x))))


(defvar *db-predicates* nil
  "a list of all predicates stored in the database.")


(defun clear-db ()
  "remove all clauses (for all predicates) from the data base."
  (mapc #'clear-predicate *db-predicates*))


(defun clear-predicate (predicate)
  "remove the clauses for a single predicate."
  (setf (get predicate 'clauses) nil))


(defun rename-variables (x)
  "replace all variables in x with new ones."
  (sublis (mapcar (lambda (var) (cons var (gensym (string var))))
                  (variables-in x))
          x))


(defun find-anywhere-if (predicate tree)
  "does predicate apply to any atom in the tree?"  
  (if (atom tree)
      (funcall predicate tree)
      (or (find-anywhere-if predicate (first tree))
          (find-anywhere-if predicate (rest tree)))))


(defmacro ?- (&rest goals) `(top-level-prove ',(replace-?-vars goals)))


(defun prove-all (goals bindings)
  "Find a solution to the conjunction of goals."
  (cond ((eq bindings fail) fail)
        ((null goals) bindings)
        (t (prove (first goals) bindings (rest goals)))))


(defun prove (goal bindings other-goals)
  "Return a list of possible solutions to goal."
  (let ((clauses (get-clauses (predicate goal))))
    (if (listp clauses)
        (some
         (lambda (clause)
           (let ((new-clause (rename-variables clause)))
             (prove-all
              (append (clause-body new-clause) other-goals)
              (unify goal (clause-head new-clause) bindings))))
         clauses)
        ;; The predicate's "clauses" can be an atom:
        ;; a primitive function to call
        (funcall clauses (rest goal) bindings
                 other-goals))))


(defun show-prolog-vars (vars bindings other-goals)
  "Print each variable with its binding.
  Then ask the user if more solutions are desired."
  (if (null vars)
      (format t "~&Yes")
      (dolist (var vars)
        (format t "~&~a = ~a" var
                (subst-bindings bindings var))))
  (force-output)
  (if (continue-p)
      fail
      (prove-all other-goals bindings)))


(setf (get 'show-prolog-vars 'clauses) 'show-prolog-vars)


(defun continue-p ()
  "Ask user if we should continue looking for solutions."
  (case (read-char)
    (#\; t)
    (#\. nil)
    (#\newline (continue-p))
    (otherwise 
      (format t " Type ; to see more or . to stop")
      (continue-p))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun unique-find-anywhere-if (predicate tree
                                  &optional found-so-far)
    "return a list of leaves of tree satisfying predicate,
  with duplicates removed."
    (if (atom tree)
        (if (funcall predicate tree)
            (adjoin tree found-so-far)
            found-so-far)
        (unique-find-anywhere-if
         predicate
         (first tree)
         (unique-find-anywhere-if predicate (rest tree)
                                  found-so-far))))
  (defun variables-in (exp)
    "Return a list of all the variables in EXP."
    (unique-find-anywhere-if #'non-anon-variable-p exp))


  (defun non-anon-variable-p (x)
    (and (variable-p x) (not (eq x '?))))
  (defun replace-?-vars (exp)
    "Replace any ? within exp with a var of the form ?123."
    (cond ((eq exp '?) (gensym "?"))
	  ((atom exp) exp)
	  (t (reuse-cons (replace-?-vars (first exp))
			 (replace-?-vars (rest exp))
			 exp)))))

;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File prologc.lisp: Final version of the compiler,
;;;; including all improvements from the chapter.

(in-package "ALLEGRO-MODERATO")


(defmacro set-binding! (trail var value)
  (let ((gvar (gensym))
        (gval (gensym)))
    `(let ((,gvar ,var)
           (,gval ,value))
       (fast
         (unless (eq ,gvar ,gval)
           (vec-push ,trail ,gvar)
           (setf (var-binding ,gvar) ,gval))
         t))))


(defun set-var-binding (var val)
  (setf (cdr var) val))

(define-compiler-macro set-var-binding (var val)
  `(setf (cdr ,var) ,val))

#|(let ((a prolog::*anonymous-var*)
      (trail (allocate-trail 1)))
  (list (prolog::unify! trail a 'val)
        (cdr a)
        trail))|#

#|(let ((a (cons var-tag unbound))
      (trail (allocate-trail 1)))
  (list (prolog::unify! trail a 'val)
        (cdr a)
        trail))|#


#|(defun unify! (trail x y)
  "Destructively unify two expressions"
  (declare (list trail))
  (cond ((eql (deref x) (deref y)) t)
        ((var-p x) (if (eq x *anonymous-var*)
                       (set-var-binding x y)
                       (set-binding! trail x y)))
        ((var-p y) (if (eq x *anonymous-var*)
                       (set-var-binding y x)
                       (set-binding! trail y x)))
        ((and (consp x) (consp y))
         (and (unify! trail (first x) (first y))
              (unify! trail (rest x) (rest y))))
        (t nil)))|#

(defun unify! (trail x y)
  "Destructively unify two expressions"
  (declare (list trail))
  (cond ((eql (deref x) (deref y)) t)
        ((var-p x) (set-binding! trail x y))
        ((var-p y) (set-binding! trail y x))
        ((and (consp x) (consp y))
         (and (unify! trail (first x) (first y))
              (unify! trail (rest x) (rest y))))
        (t nil)))


(defmacro undo-bindings! (trail old-trail)
  (let ((gvar (gensym)))
    `(fast
       (let* ((trail ,trail)
              (,gvar ,old-trail)
              (ndx (trail-ndx trail))
              (vec (trail-vec trail)))
         (declare (simple-vector vec)
                  (type adim ndx ,gvar))
         (loop :until (eql ndx ,gvar)
               :do (setf (var-binding (svref vec (decf ndx)))
                         (load-time-value unbound)))
         (setf (trail-ndx trail) ndx)))))


(defun prolog-compile (symbol &optional
                       (clauses (get-clauses symbol)))
  "Compile a symbol; make a separate function for each arity."
  (unless (null clauses)
    (let ((arity (relation-arity (clause-head (first clauses)))))
      ;; Compile the clauses with this arity
      (compile-predicate
        symbol arity (clauses-with-arity clauses #'= arity))
      ;; Compile all the clauses with any other arity
      (prolog-compile
        symbol (clauses-with-arity clauses #'/= arity)))))


(defun clauses-with-arity (clauses test arity)
  "Return all clauses whose head has given arity."
  (find-all arity clauses
            :key (lambda (clause)
                   (relation-arity (clause-head clause)))
            :test test))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun relation-arity (relation)
    "The number of arguments to a relation.
  Example: (relation-arity '(p a b c)) => 3"
    (length (args relation)))

  (defun make-parameters (arity)
    "Return the list (?arg1 ?arg2 ... ?arg-arity)"
    (loop for i from 1 to arity
          collect (new-symbol '?arg i)))

  (defun make-predicate (symbol arity)
    "Return the symbol: symbol/arity"
    (symbol symbol '/ arity)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-new-predicate (symbol arity)
    ;;(new-symbol symbol '/ arity)
    (symbol symbol '/ arity)            ;;;XXX
    )

  (defun ensure-functor-locf* (name arity)
    (let ((functor (assoc arity (get name 'functor))))
      (if functor
          functor
          (let ((new-functor (cons arity (make-new-predicate name arity))))
            (push new-functor (get name 'functor))
            new-functor))))


  (defmacro make-predicate* (symbol arity)
    `(the cl:symbol (cdr (ensure-functor-locf* ,symbol ,arity)))))


(defun make-= (x y) `(= ,x ,y))


(defun compile-call (predicate args cont)
  "Compile a call to a prolog predicate."
  `(,predicate ,@args ,cont))


(defun prolog-compiler-macro (name)
  "Fetch the compiler macro for a Prolog predicate."
  ;; Note NAME is the raw name, not the name/arity
  (get name 'prolog-compiler-macro))


(defmacro def-prolog-compiler-macro (name arglist &body body)
  "Define a compiler macro for Prolog."
  `(setf (get ',name 'prolog-compiler-macro)
         (lambda ,arglist .,body)))


(defun has-variable-p (x)
  "Is there a variable anywhere in the expression x?"
  (find-if-anywhere #'variable-p x))


(defun proper-listp (x)
  "Is x a proper (non-dotted) list?"
  (or (null x)
      (and (consp x) (proper-listp (rest x)))))


(defun maybe-add-undo-bindings (trail compiled-exps)
  "Undo any bindings that need undoing.
  If there are any, bind the trail before we start."
  (if (length=1 compiled-exps)
      compiled-exps
      `((let ((old-trail (trail-ndx ,trail)))
          ,(first compiled-exps)
          ,@(loop for exp in (rest compiled-exps)
                  collect `(undo-bindings! ,trail old-trail)
                  collect exp)))))


(defun bind-unbound-vars (parameters exp)
  "If there are any variables in exp (besides the parameters)
  then bind them to new vars."
  (let ((exp-vars (set-difference (variables-in exp)
                                  parameters)))
    (if exp-vars
        `(let ,(mapcar (lambda (var) `(,var (?)))
                exp-vars)
           ,exp)
        exp)))


(defmacro <- (&rest clause)
  "Add a clause to the data base."
  `(add-clause ',(make-anonymous clause)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-anonymous (exp &optional
                       (anon-vars (anonymous-variables-in exp)))
  "Replace variables that are only used once with ?."
  (cond ((consp exp)
         (reuse-cons (make-anonymous (first exp) anon-vars)
                     (make-anonymous (rest exp) anon-vars)
                     exp))
        ((member exp anon-vars) '?)
        (t exp)))

  (defun anonymous-variables-in (tree)
  "Return a list of all variables that occur only once in tree."
    (values (anon-vars-in tree nil nil)))

  (defun anon-vars-in (tree seen-once seen-more)
  "Walk the data structure TREE, returning a list of variabless
   seen once, and a list of variables seen more than once."
  (cond
    ((consp tree)
     (multiple-value-bind (new-seen-once new-seen-more)
         (anon-vars-in (first tree) seen-once seen-more)
       (anon-vars-in (rest tree) new-seen-once new-seen-more)))
    ((not (variable-p tree)) (values seen-once seen-more))
    ((member tree seen-once)
     (values (delete tree seen-once) (cons tree seen-more)))
    ((member tree seen-more)
     (values seen-once seen-more))
    (t (values (cons tree seen-once) seen-more)))))


(defun compile-unify (trail x y bindings)
  "Return 2 values: code to test if x and y unify,
  and a new binding list."
  (cond
   ;; Unify constants and conses:                       ; Case
   ((not (or (has-variable-p x) (has-variable-p y)))    ; 1,2
    (values (equal x y) bindings))
   ((and (consp x) (consp y))                           ; 3
    (multiple-value-bind (code1 bindings1)
                         (compile-unify trail (first x) (first y) bindings)
      (multiple-value-bind (code2 bindings2)
                           (compile-unify trail (rest x) (rest y) bindings1)
        (values (compile-if code1 code2) bindings2))))
   ;; Here x or y is a variable.  Pick the right one:
   ((variable-p x) (compile-unify-variable trail x y bindings))
   (t              (compile-unify-variable trail y x bindings))))


(defun compile-if (pred then-part)
  "Compile a Lisp IF form. No else-part allowed."
  (case pred
    ((t) then-part)
    ((nil) nil)
    (otherwise `(if ,pred ,then-part))))


(defun compile-unify-variable (trail x y bindings)
  "X is a variable, and Y may be."
  (let* ((xb (follow-binding x bindings))
         (x1 (if xb (cdr xb) x))
         (yb (if (variable-p y) (follow-binding y bindings)))
         (y1 (if yb (cdr yb) y)))
    (cond                                                 ; Case:
      ((or (eq x '?) (eq y '?)) (values t bindings))      ; 12
      ((not (and (equal x x1) (equal y y1)))              ; deref
       (compile-unify trail x1 y1 bindings))
      ((find-anywhere x1 y1) (values nil bindings))       ; 11
      ((consp y1)                                         ; 7,10
       (values `(unify! ,trail ,x1 ,(compile-arg y1 bindings))
               (bind-variables-in y1 bindings)))
      ((not (null xb))
       ;; i.e. x is an ?arg variable
       (if (and (variable-p y1) (null yb))
           (values 't (extend-bindings y1 x1 bindings))   ; 4
           (values `(unify! ,trail ,x1 ,(compile-arg y1 bindings))
                   (extend-bindings x1 y1 bindings))))    ; 5,6
      ((not (null yb))
       (compile-unify-variable trail y1 x1 bindings))
      (t (values 't (extend-bindings x1 y1 bindings))))))


; 8,9
(defun bind-variables-in (exp bindings)
  "Bind all variables in exp to themselves, and add that to
  bindings (except for variables already bound)."
  (dolist (var (variables-in exp))
    (unless (get-binding var bindings)
      (setf bindings (extend-bindings var var bindings))))
  bindings)


(defun follow-binding (var bindings)
  "Get the ultimate binding of var according to bindings."
  (let ((b (get-binding var bindings)))
    (if (eq (car b) (cdr b))
        b
        (or (follow-binding (cdr b) bindings)
            b))))


(defun compile-arg (arg bindings)
  "Generate code for an argument to a goal in the body."
  (cond ((eq arg '?) '(?))
        ((variable-p arg)
         (let ((binding (get-binding arg bindings)))
           (if (and (not (null binding))
                    (not (eq arg (binding-val binding))))
               (compile-arg (binding-val binding) bindings)
               arg)))
        ((not (find-if-anywhere #'variable-p arg)) `',arg)
        ((proper-listp arg)
         `(list .,(mapcar (lambda (a) (compile-arg a bindings))
                          arg)))
        (t `(cons ,(compile-arg (first arg) bindings)
                  ,(compile-arg (rest arg) bindings)))))


(defun bind-new-variables (bindings goal)
  "Extend bindings to include any unbound variables in goal."
  (let ((variables (remove-if (lambda (v) (assoc v bindings))
                              (variables-in goal))))
    (nconc (mapcar #'self-cons variables) bindings)))


(defun self-cons (x) (cons x x))


(def-prolog-compiler-macro = (trail goal body cont bindings)
  "Compile a goal which is a call to =."
  (let ((args (args goal)))
    (if (/= (length args) 2)
        :pass ;; decline to handle this goal
        (multiple-value-bind (code1 bindings1)
                             (compile-unify trail (first args) (second args) bindings)
          (compile-if code1
                      (compile-body trail body cont bindings1))))))


(defun compile-clause (trail parms clause cont)
  "Transform away the head, and compile the resulting body."
  (bind-unbound-vars       
   parms                  
   ;; fix broken compilation of (setof ?x (or clause clause ..) ?answer)
   (if (member (car clause) '(if or and))
       (compile-body trail (list clause) cont (mapcar #'self-cons parms))
       (compile-body trail
                     (nconc (mapcar #'make-= parms (args (clause-head clause)))
                            (clause-body clause))
                     cont
                     (mapcar #'self-cons parms)))))


;***
(defvar *uncompiled* nil 
  "Prolog symbols that have not been compiled.")


(defun add-clause (clause &key asserta)
  "Add a clause to the data base, indexed by head's predicate."
  ;; The predicate must be a non-variable symbol.
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *db-predicates*)
    (pushnew pred *uncompiled*)         ;***
    (setf (get pred 'clauses)
	  (if asserta
	      (nconc (list clause) (get-clauses pred))
	      (nconc (get-clauses pred) (list clause))))
    pred))


(defun retract-clause (clause)
  "Retract a clause from the data base"
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *uncompiled*)
    (setf (get pred 'clauses)
	  (delete clause (get-clauses pred) :test #'equal))
    pred))


(defun top-level-prove (goals)
  "Prove the list of goals by compiling and calling it."
  ;; First redefine top-level-query
  (clear-predicate 'top-level-query)
  (let ((vars (delete '? (variables-in goals))))
    (add-clause `((top-level-query)
                  ,@goals
                  (show-prolog-vars ,(mapcar #'symbol-name vars)
                                    ,vars))))
  ;; Now run it
  (run-prolog 'top-level-query/0 *trail* #'ignorer)
  (format t "~&No.")
  (values))


(defun run-prolog (procedure trail cont)
  "Run a 0-ary prolog procedure with a given continuation."
  ;; First compile anything else that needs it
  (prolog-compile-symbols)
  ;; Reset the trail and the new variable counter
  (setf (trail-ndx trail) 0)
  (setf *var-counter* 0)
  ;; Finally, call the query
  (catch 'top-level-prove
    (funcall procedure trail cont)))


(defun prolog-compile-symbols (&optional (symbols *uncompiled*))
  "Compile a list of Prolog symbols.
  By default, the list is all symbols that need it."
  (with-compilation-unit ()
    (mapc #'prolog-compile symbols)
    (setf *uncompiled* (set-difference *uncompiled* symbols))))


(defun show-prolog-vars/2 (var-names vars cont)
  "Display the variables, and prompt the user to see
  if we should continue.  If not, return to the top level."
  (if (null vars)
      (format t "~&Yes")
      (loop for name in var-names
            for var in vars do
              (format t "~&~a = ~a" name (deref-exp var))))
  (finish-output)
  (if (and vars (continue-p))
      (funcall cont)
      (throw 'top-level-prove nil)))


(defun deref-exp (exp)
  "Build something equivalent to EXP with variables dereferenced."
  (if (atom (deref exp))
      exp
      (reuse-cons
        (deref-exp (first exp))
        (deref-exp (rest exp))
        exp)))
 
(define-compiler-macro deref-exp (&whole w exp &environment env)
  (if (constantp exp env)
      `',exp
      w))


(defun deref-exp-fail (exp)
  (if (atom (deref-fail exp))
      exp
      (reuse-cons (deref-exp-fail (first exp))
                  (deref-exp-fail (rest exp))
                  exp)))


(defvar *predicate* nil
  "The Prolog predicate currently being compiled")


(defvar *defun* 'defun)


;(defvar *defun* 'tail-recursive-defun)


(defun compile-predicate (symbol arity clauses)
  "Compile all the clauses for a given symbol/arity
  into a single LISP function."
  (let ((*predicate* (make-predicate* symbol arity))    ;***
        (parameters (make-parameters arity)))
    (setf (get *predicate* 'defun)
          `(,*defun* ,*predicate* (trail ,@parameters cont)
             .,(maybe-add-undo-bindings 'trail
                                        (mapcar (lambda (clause)
                                                  (compile-clause 'trail parameters clause 'cont))
                                                clauses))))
    (compile (eval (get *predicate* 'defun)))))


(defun goal-cut-p (goal)
  (eq goal '!))


(defun goal-conjunction-p (goal)
  (goal-and-p goal))


(defun goal-and-p (goal)
  (and (consp goal)
       (eq (car goal) 'and)))


(defun goal-disjunction-p (goal)
  (and (goal-or-p goal)
       (not (goal-if-then-p (cadr goal)))))


(defun goal-or-p (goal)
  (and (consp goal)
       (eq (car goal) 'or)))


(defun goal-if-p (goal)
  (and (consp goal)
       (eq (car goal) 'if)))


(defun goal-if-then-p (goal)
  (and (goal-if-p goal)
       (null (cdddr goal))))


(defun goal-if-then-else-p (goal)
  (or
   ;; (OR (IF A B) C)
   (and (goal-or-p goal)
        (goal-if-then-p (cadr goal)))
   ;; (IF A B C)
   (and (goal-if-p goal)
        (not (null (cdddr goal)))
        (null (cddddr goal)))))


(defun destructure-if-then-else (goal)
  (cond
    ((goal-or-p goal)
     (destructuring-bind (or/2 (if/2 if then) else) goal
       (declare (ignore or/2 if/2))
       (values if then else)))
    ((goal-if-p goal)
     (destructuring-bind (if/3 if then else) goal
       (declare (ignore if/3))
       (values if then else)))
    (t (error "Goal not an IF-THEN-ELSE: ~S" goal))))


(defun compile-body (trail body cont bindings)
  "Compile the body of a clause."
  (if (null body)
      `(funcall ,cont)
      (let ((goal (first body)))
	(cond
         ((goal-cut-p goal)
          `(progn
             ,(compile-body trail (rest body) cont bindings)
             (return-from ,*predicate* nil)))
         ((goal-conjunction-p goal)
          (compile-body trail (append (cdr goal) (rest body)) cont bindings))
         ((goal-disjunction-p goal)
          (let ((bindings (bind-new-variables bindings goal)))
            `(let ((old-trail (trail-ndx trail))
                   (cont (lambda () ,(compile-body trail (rest body) cont bindings))))
               ,(compile-body trail (list (cadr goal)) 'cont bindings)
               (undo-bindings! ,trail old-trail)
               ,(compile-body trail (list (caddr goal)) 'cont bindings))))
         ((goal-if-then-p goal)
          (let ((bindings (bind-new-variables bindings goal)))
            `(let ((cont (lambda ()
                           ,(compile-body trail (cons (caddr goal) (rest body)) cont bindings))))
               (block nil
                 ,(compile-body trail (list (cadr goal)) '(lambda () (funcall cont) (return nil)) bindings)))))
         ((goal-if-then-else-p goal)
          (let ((bindings (bind-new-variables bindings goal)))
            (multiple-value-bind (if then else)
                                 (destructure-if-then-else goal)
              `(let ((old-trail (trail-ndx ,trail))
                     (cont (lambda ()
                             ,(compile-body trail (rest body) cont bindings))))
                 (block nil
                   ,(compile-body trail
                                  (list if)
                                  `(lambda ()
                                     ,(compile-body trail (list then) cont bindings) (return nil)) bindings)
                   (undo-bindings! ,trail old-trail)
                   ,(compile-body trail (list else) 'cont bindings))))))
         (t
          (let* ((macro (prolog-compiler-macro (predicate goal)))
                 (macro-val (if macro 
                                (funcall macro trail goal (rest body) cont bindings))))
            (if (and macro (not (eq macro-val :pass)))
                macro-val
                `(,(make-predicate* (predicate goal)
                                    (relation-arity goal))
                  ,trail
                  ,@(mapcar (lambda (arg)
                              (compile-arg arg bindings))
                            (args goal))
                  ,(if (null (rest body))
                       cont
                       `(lambda ()
                          ,(compile-body trail
                                         (rest body) cont
                                         (bind-new-variables bindings goal))))))))))))


;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File prologcp.lisp:  Primitives for the prolog compiler
;;;; needed to actually run some functions.

;;; Bug fix by Adam Farquhar, farquhar@cs.utexas.edu.
;;; Trivia: Farquhar is Norvig's cousin.

(in-package "ALLEGRO-MODERATO")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun name/arity (symbol)
  (let* ((name/arity (string symbol))
         (split-at (position #\/ name/arity)))
    (cons (subseq name/arity 0 split-at)
          (parse-integer (subseq name/arity (1+ split-at)))))))


(defmacro defpred (name (&rest args) &body body)
  (let* ((name/arity (name/arity name))
         (functor (ensure-functor-locf* (find-symbol (car name/arity)
                                                     (symbol-package name))
                                        (cdr name/arity))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
     #|(setf (fdefinition ',(cdr functor))
             (fdefinition
              (defun ,name (,@args)
                ,@body)))|#
     (defun ,name (,@args)
       ,@body)
     ',(cdr functor))))


(defpred fail/0 (trail cont)
  "7.8.1"
  (declare (ignore trail cont))
  nil)


(defpred true/0 (trail cont)
  "7.8.2"
  (declare (ignore trail))
  (funcall cont))


(defpred call/1 (trail ?g cont)
  "7.8.3: Try to prove goal by calling it."
  (deref ?g)
  (cond
    ((or (goal-and-p ?g) (goal-or-p ?g) (goal-if-p ?g))
     ;; FIXME: this use of a temporary predicate name is ugly,
     ;; non-threadsafe and basically evil bad and wrong.
     (funcall (compile-predicate 'call/1-tmp-fun 0 (list ?g)) trail cont))
    (t (apply (make-predicate* (predicate ?g) (length (args ?g)))
              trail
              (append (args ?g) (list cont))))))


(defpred !/0 (trail cont)
  "7.8.4"
  (declare (ignore trail))
  (funcall cont))


(define-condition prolog-throw ()
  ((ball :initarg :ball :reader ball)))


(defpred catch/3 (trail ?goal ?catch ?recover cont)
  "7.8.9"
  (let ((trail-ndx (trail-ndx trail)))
    (handler-bind ((prolog-throw (lambda (c)
				   (let ((old-trail (trail-ndx trail)))
				     (when (unify! trail ?catch (ball c))
				       (undo-bindings! trail trail-ndx)
				       (return-from catch/3
					 (call/1 trail ?recover cont)))
				     (undo-bindings! trail old-trail)
				     (signal c)))))
      (call/1 trail ?goal cont))))


(defpred throw/1 (trail ?ball cont)
  "7.8.10"
  (declare (ignore trail cont))
  (signal (make-condition 'prolog-throw :ball (deref ?ball)))
  ;; FIXME: make this a throw eventually, and have a catch around
  ;; top-level-query.
  (error "system error"))


;;; 8.2 term unification

(defpred =/2 (trail ?arg1 ?arg2 cont)
  "8.2.1"
  (when (unify! trail ?arg1 ?arg2)
    (funcall cont)))


(defpred unify-with-occurs-check/2 (trail ?arg1 ?arg2 cont)
  "8.2.2"
  (let ((*occurs-check* t))
    (when (unify! trail ?arg1 ?arg2)
      (funcall cont))))


(defpred \\=/2 (trail ?arg1 ?arg2 cont)
  "8.2.3"
  (unless (unify! trail ?arg1 ?arg2)
    (funcall cont)))


;;; 8.3 type testing

(defun unbound-var-p (exp)
  "Is EXP an unbound var?"
  (and (var-p exp) (not (bound-p exp))))


(define-compiler-macro unbound-var-p (exp)
  "Is EXP an unbound var?"
  `(and (var-p ,exp) (not (bound-p ,exp))))


(defun nonvarp (exp)
  (not (unbound-var-p exp)))


(defun atomicp (exp)
  ;; not ATOM, because we might be implementing unbound VARs as
  ;; something returning true to lisp's ATOM.
  (or (symbolp exp)
      (integerp exp)
      (floatp exp)))


(macrolet ((define-type-testing-predicate (name docstring fun)
             `(defpred ,name (trail x cont)
                ,docstring
                (declare (ignore trail))
                (when (,fun (deref x))
                  (funcall cont)))))
  (define-type-testing-predicate var/1 "8.3.1" unbound-var-p)
  (define-type-testing-predicate atom/1 "8.3.2" symbolp)
  (define-type-testing-predicate integer/1 "8.3.3" integerp)
  (define-type-testing-predicate real/1 "8.3.4" floatp)
  (define-type-testing-predicate atomic/1 "8.3.5" atomicp)
  ;;(assert (not (consp (?))))
  (define-type-testing-predicate compound/1 "8.3.6" consp)
  (define-type-testing-predicate nonvar/1 "8.3.7" nonvarp)
  ;; strictly, this should be (OR INTEGERP FLOATP).
  (define-type-testing-predicate number/1 "8.3.8" numberp))


;;; 8.4 term comparison

(defun deref-equal (x y)
  "Are the two arguments EQUAL with no unification,
  but with dereferencing?"
  (or (eql (deref x) (deref y))
      (and (consp x) (consp y)
           (deref-equal (car x) (car y))
           (deref-equal (cdr x) (cdr y)))))


(defpred ==/2 (trail ?x ?y cont)
  "8.4.1: Are the two arguments EQUAL with no unification,
  but with dereferencing?  If so, succeed."
  (declare (ignore trail))
  (when (deref-equal ?x ?y)
    (funcall cont)))


(defpred \\==/2 (trail ?x ?y cont)
  "8.4.2"
  (declare (ignore trail))
  (unless (deref-equal ?x ?y)
    (funcall cont)))


(defun term-precedes (x y)
  (and (not (eql (deref x) (deref y)))
       (typecase x
	 (var (or (not (var-p y))
		  ;; FIXME
		  (evenp (random 2))))
	 ((float) (if (var-p y)
		      nil
		      (or (not (floatp y))
			  (< x y))))
	 ((integer) (if (or (var-p y) (floatp y))
			nil
			(or (not (integerp y))
			    (< x y))))
	 (cl:symbol (if (or (var-p y) (floatp y) (integerp y))
			nil
			(or (not (symbolp y))
			    (string< (string x) (string y)))))
	 ;; "compound term": i.e. not lists, really
	 ((cons) (when (consp y)
		   (or (< (length x) (length y))
		       (and (= (length x) (length y))
			    (or (term-precedes (car x) (car y))
				(and (eql (car x) (car y))
				     (do* ((xis (cdr x) (cdr xis))
					   (xi (car xis) (car xis))
					   (yis (cdr y) (cdr yis))
					   (yi (car yis) (car yis)))
					  ((null xis) nil)
				       (when (term-precedes xi yi)
					 (return t))))))))))))


(defpred @</2 (trail ?x ?y cont)
  "8.4.3"
  (declare (ignore trail))
  (when (term-precedes ?x ?y)
    (funcall cont)))


(defpred @=</2 (trail ?x ?y cont)
  "8.4.4"
  (declare (ignore trail))
  (when (or (deref-equal ?x ?y)
	    (term-precedes ?x ?y))
    (funcall cont)))


(defpred @>/2 (trail ?x ?y cont)
  (declare (ignore trail))
  (when (term-precedes ?y ?x)
    (funcall cont)))


(defpred @>=/2 (trail ?x ?y cont)
  (declare (ignore trail))         
  (when (or (deref-equal ?y ?x)
	    (term-precedes ?y ?x))
    (funcall cont)))


;;; 8.5 term creation and decomposition

(defpred functor/3 (trail ?term ?name ?arity cont)
  "8.5.1"
  (cond ((unbound-var-p ?term)
         (assert (not (unbound-var-p ?name)))
         (assert (not (unbound-var-p ?arity)))
         (when (unify! trail ?term (list* (deref ?name)
                                          (loop repeat (deref ?arity) collect (?))))
           (funcall cont)))
        (t (if (atomicp (deref ?term))
               (when (and (unify! trail ?arity 0)
                          (unify! trail ?name ?term))
                 (funcall cont))
               (when (and (unify! trail ?arity (length (cdr ?term)))
                          (unify! trail ?name (or (and(car ?term)))))
                 (funcall cont))))))


(defpred arg/3 (trail ?n ?term ?arg cont)
  "8.5.2"
  (when (unify! trail (nth (deref ?n) (deref ?term)) ?arg)
    (funcall cont)))


(defun =.. (trail ?term ?list cont)
  "8.5.3"
  ;; FIXME: have to decide how to represent Prolog lists
  (declare (ignore trail ?term ?list cont))
  )


(defun make-renamed-copy (term)
  (cond
    ((unbound-var-p term) (?))
    ((var-p term) term)
    ((atom term) term)
    (t (cons (make-renamed-copy (car term))
             (make-renamed-copy (cdr term))))))


(defpred copy-term/2 (trail ?term1 ?term2 cont)
  "8.5.4"
  (when (unify! trail (make-renamed-copy (deref ?term1)) (deref ?term2))
    (funcall cont)))


;;; 8.6 arithmetic evaluation

(defpred is/2 (trail var exp cont)
  "8.6.1"
  ;; Example: (is ?x (+ 3 (* ?y (+ ?z 4))))
  ;; Or even: (is (?x ?y ?x) (cons (first ?z) ?l))
  (when ;; (and (not (find-if-anywhere #'unbound-var-p exp))
      ;;      (unify! var (eval (deref-exp exp))))
      (or (and (not (find-if-anywhere #'unbound-var-p exp))
               (unify! trail var (eval (deref-exp exp))))
          (let ((var exp)
                (exp var))
            (and (not (find-if-anywhere #'unbound-var-p exp))
                 (unify! trail var (eval (deref-exp exp))))))
    (funcall cont)))


;;; 8.7 arithmetic comparison

(macrolet ((define-arithmetic-comparison-predicate (name op)
	     `(defpred ,name (trail ?e1 ?e2 cont)
		"8.7.3"
                (declare (ignore trail))
		(when (and (not (find-if-anywhere #'unbound-var-p ?e1))
			   (not (find-if-anywhere #'unbound-var-p ?e1)))
		  ;; FIXME: CL specifies comparison on (float,integer)
		  ;; by coercing the float to a rational, and
		  ;; comparing.  Prolog doesn't have rationals, and
		  ;; the coercion goes the other way.
		  (when (,op (eval (deref-exp ?e1)) (eval (deref-exp ?e2)))
		    (funcall cont))))))
  (define-arithmetic-comparison-predicate |=:=|/2 =)
  (define-arithmetic-comparison-predicate =\\=/2 /=)
  (define-arithmetic-comparison-predicate </2 <)
  (define-arithmetic-comparison-predicate =</2 <=)
  (define-arithmetic-comparison-predicate >/2 >)
  (define-arithmetic-comparison-predicate >=/2 >=))


;;; FIXME: 8.8 clause retrieval and information

(defpred clause/2 (trail ?head ?body cont)
  "8.8.1"
  (let ((clauses (get-clauses (predicate (deref ?head)))))
    (let ((old-trail (trail-ndx trail)))
      (dolist (clause clauses)
	(when (unify! trail `(,?head . ,?body) clause)
	  (funcall cont))
        (undo-bindings! trail old-trail)))))


(defpred current-predicate/1 (trail ?pi cont)
  "8.8.2"
  ;; FIXME: need to refactor *DB-PREDICATES* so that it contains arity
  ;; information
  (declare (ignore trail ?pi cont))
  )


;;; 8.9 clause creation and destruction

(defpred asserta/1 (trail ?clause cont)
  "8.9.1"
  (let* ((old-trail (trail-ndx trail))
	 (?head (?))
	 (?body (?)))
    (unless (unify! trail (deref ?clause) `(<- ,?head . ,?body))
      (undo-bindings! trail old-trail)
      (unify! trail `(,?head . ,?body) `(,?clause (true))))
    (add-clause (cons (deref ?head) (deref ?body)) :asserta t)
    (funcall cont)))


(defpred assertz/1 (trail ?clause cont)
  "8.9.2"
  (let* ((old-trail (trail-ndx trail))
	 (?head (?))
	 (?body (?)))
    (unless (unify! trail (deref ?clause) `(<- ,?head . ,?body))
      (undo-bindings! trail old-trail)
      (unify! trail `(,?head . ,?body) `(,?clause (true))))
    (add-clause (cons (deref ?head) (deref ?body)))
    (funcall cont)))


(defpred retract/1 (trail ?clause cont)
  "8.9.3"
  (let* ((old-trail (trail-ndx trail))
	 (?head (?))
	 (?body (?)))
    (unless (unify! trail (deref ?clause) `(<- ,?head . ,?body))
      (undo-bindings! trail old-trail)
      (unify! trail `(,?head . ,?body) `(,?clause (true))))
    (retract-clause (cons (deref ?head) (deref ?body)))
    (funcall cont)))


(defpred abolish/1 (trail ?pi cont)
  "8.9.4"
  ;; FIXME: implement this
  (declare (ignore trail cont ?pi))
  )


;;; 8.10 all solutions

;;; FIXME: I think this is right for FINDALL/3.  BAGOF/3 and SETOF/3
;;; have extra complicated stuff to do with witnesses.
(defpred findall/3 (trail term goal bag cont)
  "8.10.1: Find all solutions to GOAL, and for each solution,
  collect the value of TERM into the list BAG."
  ;; Ex: Assume (p 1) (p 2) (p 3).  Then:
  ;;     (bagof ?x (p ?x) ?l) ==> ?l = (1 2 3)
  (let ((answers nil))
    (call/1 trail
            goal (lambda ()
		   ;; Bug fix by mdf0%shemesh@gte.com (Mark Feblowitz)
		   ;; on 25 Jan 1996; was deref-COPY
                   (push (deref-exp term) answers))) 
    (if (and (not (null answers))
             (unify! trail bag (nreverse answers)))
        (funcall cont))))


(defpred bagof/3 (trail exp goal result cont)
  "8.10.2: Find all solutions to GOAL, and for each solution,
  collect the value of EXP into the list RESULT."
  ;; Ex: Assume (p 1) (p 2) (p 3).  Then:
  ;;     (bagof ?x (p ?x) ?l) ==> ?l = (1 2 3)
  (let ((answers nil))
    (call/1 trail
            goal (lambda ()
		   ;; Bug fix by mdf0%shemesh@gte.com (Mark Feblowitz)
		   ;; on 25 Jan 1996; was deref-COPY
                   (push (deref-EXP exp) answers))) 
    (if (and (not (null answers))
             (unify! trail result (nreverse answers)))
        (funcall cont))))


(defun deref-copy (exp)
  "Copy the expression, replacing variables with new ones.
  The part without variables can be returned as is."
  ;; Bug fix by farquhar and norvig, 12/12/92.  Forgot to deref var.
  (sublis (mapcar (lambda (var) (cons (deref var) (?)))
                  (unique-find-anywhere-if #'var-p exp))
          exp))


(defpred setof/3 (trail exp goal result cont)
  "8.10.3: Find all unique solutions to GOAL, and for each solution,
  collect the value of EXP into the list RESULT."
  ;; Ex: Assume (p 1) (p 2) (p 3).  Then:
  ;;     (setof ?x (p ?x) ?l) ==> ?l = (1 2 3)
  (let ((answers nil))
    (call/1 trail
            goal
            (lambda ()
              (push (deref-exp exp) answers)))
    (if (and (not (null answers))
             (unify! trail result (delete-duplicates answers
                                                     :test #'deref-equal)))
        (funcall cont))))


;;; FIXME: findall/3

;;; 8.11 stream selection and control

(defmacro with-stream ((s-var s) &body body)
  (let ((n (gensym)))
    `(let ((,n ,s))
       (let ((,s-var (etypecase ,n
		       ;; Lisp SYMBOL -> Prolog atom -> alias
		       (cl:symbol (get ,n 'stream-alias))
		       (stream ,n))))
	 ,@body))))


;;; FIXME: we probably want *prolog-standard-input* and
;;; *prolog-standard-output*, else things are likely to get confused.

(defpred current-input/1 (trail ?stream cont)
  "8.11.2"
  (when (unify! trail (deref ?stream) *standard-input*)
    (funcall cont)))


(defpred current-output/1 (trail ?stream cont)
  "8.11.3"
  (when (unify! trail (deref ?stream) *standard-output*)
    (funcall cont)))


(defpred set-input/1 (trail ?stream-or-alias cont)
  "8.11.4"
  (declare (ignore trail))
  (with-stream (s (deref ?stream-or-alias))
    (setf *standard-input* s)
    (funcall cont)))


(defpred set-output/1 (trail ?stream-or-alias cont)
  "8.11.5"
  (declare (ignore trail))
  (with-stream (s (deref ?stream-or-alias))
    (setf *standard-output* s)
    (funcall cont)))


(defpred open/4 (trail ?source/sink ?mode ?stream ?options cont)
  "8.11.6"
  (let ((element-type 'character)
	(name (deref ?source/sink))
	(args (case (deref ?mode)
		((read) '(:direction :input))
		((write) '(:direction :output))
		((append) '(:direction :output :if-exists :append))))
	(aliases nil))
    (dolist (option (deref ?options))
      (case (car option)
	;; FIXME: 7.10.2.11 specifies also reposition(Bool) and
	;; eof_action(Action).
	(type (setf element-type (ecase (cadr option)
				   (text 'character)
				   (binary '(unsigned-byte 8)))))
	(alias (push (cadr option) aliases))))
    (let ((stream (apply #'open name :element-type element-type args)))
      (when (unify! trail ?stream stream)
	(dolist (alias aliases)
	  (setf (get alias 'stream-alias) stream)
	  (push alias (get 'stream-aliases stream)))
	(funcall cont)))))


(defpred close/1 (trail ?stream-or-alias cont)
  "8.11.7"
  ;; FIXME: this will fail if we change the representation of the
  ;; empty list.
  (close/2 trail ?stream-or-alias nil cont))


(defpred close/2 (trail ?stream-or-alias ?options cont)
  "8.11.8"
  (declare (ignore trail ?options))
  (with-stream (s (deref ?stream-or-alias))
    ;; FIXME: actually there's all the business about going back to
    ;; user_input, and also handling 7.10.2.12 force(Bool).
    (close s))
  (funcall cont))


(defpred flush-output/0 (trail cont)
  "8.11.9"
  (declare (ignore trail))
  ;; FIXME: check to see if FORCE-OUTPUT is more appropriate
  (finish-output *standard-output*)
  (funcall cont))


(defpred flush-output/1 (trail ?stream-or-alias cont)
  "8.11.10"
  (declare (ignore trail))
  (with-stream (s (deref ?stream-or-alias))
    (finish-output s))
  (funcall cont))


;;; FIXME: 8.11.11 stream-property/2.  Need a registry of all open
;;; streams.

(defpred at-end-of-stream/0 (trail cont)
  "8.11.12"
  ;; FIXME: we can fake this for character streams by doing peek-char
  ;; and handling the END-OF-FILE condition, but what about binary streams?
  (declare (ignore trail cont))
  )


(defpred at-end-of-stream/1 (trail ?stream-or-alias cont)
  "8.11.13"
  ;;; FIXME (see at-end-of-stream/0)
  (declare (ignore trail cont ?stream-or-alias))
  )


(defpred set-stream-position/2 (trail ?stream-or-alias ?position cont)
  "8.11.14"
  (declare (ignore trail))
  (with-stream (s (deref ?stream-or-alias))
    (file-position s (deref ?position))
    (funcall cont)))


;;; FIXME: 8.12 character input/output

;;; FIXME: in general, these (8.12 and 8.13) things need to work both
;;; on characters (i.e. length-one symbols) and on character codes
;;; ("integers").  For now, character input/output works on characters
;;; and character-code input/output works on integers.

(defpred get-char/1 (trail ?char cont)
  "8.12.1"
  (when (unify! trail (deref ?char) (intern (string (read-char *standard-input*))))
    (funcall cont)))


(defpred get-char/2 (trail ?stream-or-alias ?char cont)
  "8.12.2"
  (with-stream (s (deref ?stream-or-alias))
    (declare (ignore s))
    (when (unify! trail (deref ?char) (intern (string (read-char ?stream-or-alias))))
      (funcall cont))))


(defpred put-char/1 (trail ?char cont)
  "8.12.3"
  (declare (ignore trail))
  (write-char (character (deref ?char)) *standard-output*)
  (funcall cont))


(defpred put-char/2 (trail ?stream-or-alias ?char cont)
  "8.12.4"
  (declare (ignore trail))
  (with-stream (s (deref ?stream-or-alias))
    (write-char (character (deref ?char)) s)
    (funcall cont)))


(defpred nl/0 (trail cont)
  "8.12.5"
  (declare (ignore trail))
  (terpri *standard-output*)
  (funcall cont))


(defpred nl/1 (trail ?stream-or-alias cont)
  "8.12.6"
  (declare (ignore trail))
  (with-stream (s (deref ?stream-or-alias))
    (terpri s)
    (funcall cont)))


;;; 8.13 character code input/output

;;; FIXME: see comment about element type for section 8.12

(defpred get-code/1 (trail ?code cont)
  (when (unify! trail (deref ?code) (read-byte *standard-output*))
    (funcall cont)))


(defpred get-code/2 (trail ?stream-or-alias ?code cont)
  (with-stream (s (deref ?stream-or-alias))
    (when (unify! trail (deref ?code) (read-byte s))
      (funcall cont))))


(defpred put-code/1 (trail ?code cont)
  (declare (ignore trail))
  (write-byte (deref ?code) *standard-output*)
  (funcall cont))


(defpred put-code/2 (trail ?stream-or-alias ?code cont)
  (declare (ignore trail))
  (with-stream (s (deref ?stream-or-alias))
    (write-byte (deref ?code) s)
    (funcall cont)))


;;; FIXME: 8.14 Term input/output

;;; these would probably be OK for lispy prolog; they're probably not
;;; for ISO prolog.
(defpred read/1 (trail exp cont)
  (and (unify! trail exp (read))
       (funcall cont)))


(defpred write/1 (trail exp cont)
  (declare (ignore trail))         
  (write (deref-exp exp) :pretty t)
  (funcall cont))


;;; 8.15 logic and control

(defmacro with-undo-bindings ((trail) &body body)
  "Undo bindings after each expression in body except the last."
  (if (length=1 body)
      (first body)
      `(let ((old-trail (trail-ndx ,trail)))
         ,(first body)
         ,@(loop for exp in (rest body)
                 collect `(undo-bindings! ,trail old-trail)
                 collect exp))))


(defpred fail-if/1 (trail relation cont)
  "8.15.1: Negation by failure: If you can't prove G, then (not G) true."
  ;; Either way, undo the bindings.
  (with-undo-bindings (trail)
    (call/1 trail relation (lambda () (return-from fail-if/1 nil)))
    (funcall cont)))


(defpred once/1 (trail thing cont)
  "8.15.2"
  (with-undo-bindings (trail)
    (call/1 trail thing cont)
    nil))


(defpred repeat/0 (trail cont)
  "8.15.3"
  (declare (ignore trail))
  (loop (funcall cont)))


;;; 8.16 constant processing

(defpred atom-length/2 (trail ?atom ?length cont)
  "8.16.1"
  (when (unify! trail (length (string (deref ?atom))) ?length)
    (funcall cont)))


(defpred atom-concat/3 (trail ?atom1 ?atom2 ?atom12 cont)
  "8.16.2"
  (if (unbound-var-p (deref ?atom12))
      (when (unify! trail
                    ?atom12
                    (intern (concatenate 'string
                                         (string (deref ?atom1))
                                         (string (deref ?atom2)))))
        (funcall cont))
      (let* ((string (string ?atom12))
             (length (length string)))
        (let ((old-trail (trail-ndx trail)))
          (deref ?atom1)
          (deref ?atom2)
          (dotimes (i length)
            (when (unify! trail `(,?atom1 . ,?atom2)
                          (cons (intern (subseq string 0 i))
                                (intern (subseq string i))))
              (funcall cont))
            (undo-bindings! trail old-trail))))))


(defpred sub-atom/4 (trail ?atom ?start ?length ?sub-atom cont)
  "8.16.3"
  (let* ((string (string (deref ?atom)))
         (length (length string)))
    (let ((old-trail (trail-ndx trail)))
      (dotimes (s (1+ length))
        (dotimes (l (1+ (- length s)))
          (when (and (unify! trail ?start (1+ s))
                     (unify! trail ?length l)
                     (unify! trail ?sub-atom (intern (subseq string s (+ s l)))))
            (funcall cont))
          (undo-bindings! trail old-trail))))))


(defun implode (list)
  (intern
   (with-output-to-string (s)
     (dolist (c list)
       (write-char (character c) s)))))


(defun explode (symbol)
  (loop for x across (string symbol) collect (intern (string x))))


(defpred atom-chars/2 (trail ?atom ?list cont)
  "8.16.4"
  (if (unbound-var-p (deref ?atom))
      (when (unify! trail ?atom (implode (deref-exp ?list)))
        (funcall cont))
      (when (unify! trail (explode ?atom) (deref ?list))
        (funcall cont))))


(defpred atom-codes/2 (trail ?atom ?codes cont)
  "8.16.5"
  (when (if (unbound-var-p (deref ?atom))
            (unify! trail ?atom
                    (intern (coerce (loop for i in (deref-exp ?codes)
                                          collect (code-char i))
                                    'string)))
            (unify! trail (loop for i across (string ?atom)
                                collect (char-code i))
                    (deref ?codes)))
    (funcall cont)))


(defpred atom-characters/2 (trail ?atom ?characters cont)
  "for Common Lisp"
  (when (if (unbound-var-p (deref ?atom))
            (unify! trail ?atom
                    (intern (coerce (deref-exp ?characters) 'string)))
            (unify! trail (loop for i across (string ?atom)
                                collect i)
                    (deref ?characters)))
    (funcall cont)))


(defpred string-atom/2 (trail ?string ?atom cont)
  "for Common Lisp"
  (when (if (unbound-var-p (deref ?string))
            (unify! trail ?string (string (deref ?atom)))
            (unify! trail (intern ?string) (deref ?atom)))
    (funcall cont)))


(defpred string-list/2 (trail ?string ?list cont)
  (when (if (unbound-var-p (deref ?string))
            (unify! trail ?string (coerce (deref-exp ?list) 'string))
            (unify! trail (coerce ?string 'list) (deref ?list)))
    (funcall cont)))


(defpred char-code/2 (trail ?char ?code cont)
  "8.16.6"
  (if (unbound-var-p (deref ?char))
      (when (unify! trail ?char (intern (string (code-char (deref ?code)))))
        (funcall cont))
      (when (unify! trail (char-code (character ?char)) (deref ?code))
        (funcall cont))))


(defpred number-chars/2 (trail ?number ?list cont)
  "8.16.7"
  (if (unbound-var-p (deref ?number))
      (when (unify! trail ?number
                    (read-from-string (map 'string 'character (deref-exp ?list))))
        (funcall cont))
      (when (unify! trail (explode (intern (princ-to-string ?number))) (deref ?list))
        (funcall cont))))


(defpred number-codes/2 (trail ?number ?list cont)
  "8.16.8"
  (if (unbound-var-p (deref ?number))
      (when (unify! trail ?number
                    (read-from-string (map 'string 'code-char (deref-exp ?list))))
        (funcall cont))
      (when (unify! trail (map 'list 'char-code (princ-to-string ?number))
                    (deref ?list))
        (funcall cont))))


;;; 8.17 implementation-defined hooks

;;(defpred lisp/2 (?result exp cont)
;;  "Apply (first exp) to (rest exp), and return the result."
;;  (if (and (consp (deref exp))
;;           (unify! ?result (apply (first exp) (rest exp))))
;;      (funcall cont)))

(def-prolog-compiler-macro lisp (trail goal body cont bindings)
  "lisp/1 and lisp/2"
  (let ((args (args goal)))
    (case (length args)
      (1                                ; lisp/1
         (let* ((lisp-exp (first args))
                (lisp-args (variables-in lisp-exp)))
           `(progn
              (apply (lambda ,lisp-args ,(insert-deref lisp-exp))
                     ,(compile-arg lisp-args bindings))
              ,(compile-body trail body cont bindings))))
      (2                                ; lisp/2
         (let* ((var (first args))
                (lisp-exp (second args))
                (lisp-args (variables-in lisp-exp)))
           (compile-if `(unify! ,trail ,(compile-arg var bindings)
                                (apply (lambda ,lisp-args ,(insert-deref lisp-exp))
                                       ,(compile-arg lisp-args bindings)))
                       (compile-body trail body cont (bind-new-variables bindings goal)))))
      (t :pass))))


(def-prolog-compiler-macro lisp* (trail goal body cont bindings)
  "lisp*/1 and lisp*/2"
  (let ((args (args goal)))
    (case (length args)
      (1                                ; lisp*/1
       (let* ((lisp-exp (first args))
              (lisp-args (variables-in lisp-exp)))
         `(progn
            (catch 'deref-fail
              (apply (lambda ,lisp-args ,(insert-deref-fail lisp-exp))
                     ,(compile-arg lisp-args bindings)))
            ,(compile-body trail body cont bindings))))
      (2                                ; lisp*/2
       (let* ((var (first args))
              (lisp-exp (second args))
              (lisp-args (variables-in lisp-exp)))
         (compile-if `(catch 'deref-fail
                        (unify! ,trail ,(compile-arg var bindings)
                                (apply (lambda ,lisp-args ,(insert-deref-fail lisp-exp))
                                       ,(compile-arg lisp-args bindings))))
                     (compile-body trail body cont (bind-new-variables bindings goal)))))
      (t :pass))))


(def-prolog-compiler-macro lispp* (trail goal body cont bindings)
  (let ((args (args goal)))
    (let* ((lisp-exp (first args))
           (lisp-args (variables-in lisp-exp)))
      `(and (apply (lambda ,lisp-args ,(insert-deref lisp-exp))
                   ,(compile-arg lisp-args bindings))
            ,(compile-body trail body cont bindings)))))


(def-prolog-compiler-macro lispp (trail goal body cont bindings)
  (let ((args (args goal)))
    (let* ((lisp-exp (first args))
           (lisp-args (variables-in lisp-exp)))
      `(and (catch 'deref-fail
              (apply (lambda ,lisp-args ,(insert-deref lisp-exp))
                     ,(compile-arg lisp-args bindings)))
            ,(compile-body trail body cont bindings)))))


(in-package "ALLEGRO-MODERATO")


(defun retract-same-arity-clause (clause)
  "Retract a clause from the data base"
  (let* ((head (clause-head clause))
         (pred (predicate head))
         (arity (1-  (length head))))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *uncompiled*)
    (setf (get pred 'clauses)
	  (delete-if (lambda (x)
                       (let* ((h (clause-head x))
                              (p (predicate h))
                              (a (1- (length h))))
                         (and (eq p pred)
                              (= a arity))))
                     (get-clauses pred)))
    pred))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-predicate-ftype (clause-head)
    (let ((name (predicate clause-head))
          (arity (relation-arity clause-head)))
      `(ftype (function (list ,@(loop :repeat arity :collect T) function) T)
              ,(make-predicate* name arity)))))


(defmacro <-- (&rest clause)
  "Retract a same arity clause from the data base,
and add a clause to the data base."
  (let ((clause (replace-?-vars clause)))
    `(progn
       (declaim ,(make-predicate-ftype (clause-head clause)))
       (retract-same-arity-clause ',clause)
       (add-clause ',clause))))


(defun insert-deref (exp)
  (if (atom exp)
      (if (variable-p exp)
          `(deref-exp ,exp)
          exp)
      (cons (insert-deref (car exp))
            (insert-deref (cdr exp)))))


(defun insert-deref-fail (exp)
  (if (atom exp)
      (if (variable-p exp)
          `(deref-exp-fail ,exp)
          exp)
      (cons (insert-deref-fail (car exp))
            (insert-deref-fail (cdr exp)))))


(defun prolog-translate-goals (goals)
  (mapcar (lambda (goal)
            (if (starts-with goal 'lisp)
                (let ((vars (variables-in (last goal))))
                  ``(,@',(butlast goal)
                         (apply ,(lambda (,@vars)
                                   ,@(insert-deref (last goal)))
                                (list ,@',vars))))
                `',goal))
          goals))


(defun interpreted-de-wrapper (cont &rest conses)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (dynamic-extent cont conses))
  (apply cont conses))


(defmacro de-consify (pred trail cont &rest args)
  `(apply #'interpreted-de-wrapper
          (lambda (&rest conses)
            (declare (ignorable conses))
            (,(eval pred) ,trail ,cont ,@args))
          (make-list 0)))


(declaim (inline let-de-1))


(defun let-de-1 (setter cont)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (function setter cont))
  (let ((var (?)))
    (declare (var var))
    (funcall setter var)
    (funcall cont)))


(defmacro let-de ((&rest bindspec) &body body)
  (if bindspec
      `(let (,(caar bindspec))
         (flet ((stuffit (new) (setq ,(caar bindspec) new)))
           (declare (dynamic-extent #'stuffit))
           (let-de-1 #'stuffit
                     (lambda ()
                       (let-de ,(cdr bindspec)
                         ,@body)))))
      `(progn
         ,@body)))


(defun compile-body* (trail body cont bindings)
  "Compile the body of a clause."
  (if (null body)
      `(funcall ,cont)
      (let ((goal (first body)))
	(cond
	  ((goal-cut-p goal)
	   `(progn
	      ,(compile-body* trail (rest body) cont bindings)
              (return-from ,*predicate* nil)))
	  ((goal-conjunction-p goal)
	   (compile-body* trail (append (cdr goal) (rest body)) cont bindings))
	  ((goal-disjunction-p goal)
	   (let ((bindings (bind-new-variables bindings goal)))
	     `(let ((old-trail (trail-ndx ,trail))
		    (cont (lambda () ,(compile-body* trail (rest body) cont bindings))))
	        ,(compile-body* trail (list (cadr goal)) 'cont bindings)
	        (undo-bindings! ,trail old-trail)
	        ,(compile-body* trail (list (caddr goal)) 'cont bindings))))
	  ((goal-if-then-p goal)
	   (let ((bindings (bind-new-variables bindings goal)))
	     `(let ((cont (lambda ()
                            ,(compile-body* trail (cons (caddr goal) (rest body)) cont bindings))))
	        (block nil
		  ,(compile-body* trail (list (cadr goal)) '(lambda () (funcall cont) (return nil)) bindings)))))
	  ((goal-if-then-else-p goal)
	   (let ((bindings (bind-new-variables bindings goal)))
	     (multiple-value-bind (if then else)
                 (destructure-if-then-else goal)
	       `(let ((old-trail (trail-ndx ,trail))
		      (cont (lambda ()
			      ,(compile-body* trail (rest body)
				              cont bindings))))
		  (block nil
		    ,(compile-body* trail (list if)
                                    `(lambda ()
                                       ,(compile-body* trail (list then) cont bindings)
                                       (return nil))
                                    bindings)
		    (undo-bindings! ,trail old-trail)
		    ,(compile-body* trail (list else) 'cont bindings))))))
	  (t
	   (let* ((macro (prolog-compiler-macro (predicate goal)))
		  (macro-val (and macro (funcall macro trail goal (rest body) cont bindings))))
	     (if (and macro (not (eq macro-val :pass)))
		 macro-val
		 `(let ((de-cont
                          ,(if (null (rest body))
			       cont
			       `(lambda ()
			          ,(compile-body* trail (rest body)
                                                  cont
			                          (bind-new-variables bindings goal))))))
                    (declare (dynamic-extent de-cont))
                    (de-consify (make-predicate* ',(predicate goal)
				                 ',(relation-arity goal))
                                ,trail
		                ,@(mapcar (lambda (arg)
			                    (compile-arg arg bindings))
			                  (args goal))
		                de-cont)))))))))


(defun generator/2 (trail ?item ?generator cont)
  (declare (ignore))
  (deref ?generator)
  (loop (multiple-value-bind (item next?)
                             (funcall ?generator)
          (declare (ignore next?))
          (cond ((and item (unify! trail ?item item))
                 (funcall cont)
                 (setf (var-binding ?item) unbound))
                (T (return nil))))))


(defun generator*/2 (trail ?item ?generator cont)
  (declare (ignore))
  (deref ?generator)
  (loop (multiple-value-bind (item next?)
                             (funcall ?generator)
          (cond ((and next? (unify! trail ?item item))
                 (funcall cont)
                 (setf (var-binding ?item) unbound))
                (T (return nil))))))


(def-prolog-compiler-macro generating (trail goal body cont bindings)
  (let ((generator (gensym "?generator")))
    `(let ((,generator (?)))
       ,(funcall (prolog-compiler-macro 'lisp)
              trail
              `(lisp ,generator ,(second (args goal)))
              `((generator ,(first (args goal))
                           ,generator)
                ,@body)
              cont
              bindings))))


(def-prolog-compiler-macro generating* (trail goal body cont bindings)
  (let ((generator (gensym "?generator")))
    `(let ((,generator (?)))
       ,(funcall (prolog-compiler-macro 'lisp)
                 trail
                 `(lisp ,generator ,(second (args goal)))
                 `((generator* ,(first (args goal))
                               ,generator)
                   ,@body)
                 cont
                 bindings))))


(<-- (slot= ?instance ?slot-name ?slot-value)
     (lisp ?slot-value (slot-value ?instance ?slot-name)))


(<-- (slot=* ?instance ?slot-name ?slot-value)
     (lispp* (and (slot-exists-p ?instance ?slot-name)
                  (slot-boundp ?instance ?slot-name)))
     (lisp ?slot-value (slot-value ?instance ?slot-name)))


(def-prolog-compiler-macro = (trail goal body cont bindings)
  "Compile a goal which is a call to =."
  (let ((args (args goal)))
    (if (/= (length args) 2)
        :pass ;; decline to handle this goal
        (multiple-value-bind (code1 bindings1)
                             (compile-unify trail (first args) (second args) bindings)
          (compile-if code1
                      (compile-body trail body cont bindings1))))))


(def-prolog-compiler-macro let (trail goal body cont bindings)
  (destructuring-bind (op var val &optional decl)
                      goal
    (declare (ignore op))
    `(let ((,var ,val))
       ,decl
       ,(compile-body trail body cont bindings))))


(def-prolog-compiler-macro let* (trail goal body cont bindings)
  (destructuring-bind (op var val &optional decl)
                      goal
    (declare (ignore op))
    `(let* ((,var ,val))
       (declare (special ,var))
       ,decl
       ,(compile-body trail body cont bindings))))


(def-prolog-compiler-macro unwind-protect (trail goal body cont bindings)
  (destructuring-bind (op &body cleanup-forms)
                      goal
    (declare (ignore op))
    `(unwind-protect (catch 'deref-fail
                       ,(compile-body trail body cont bindings))
       ,@cleanup-forms)))


;;; *EOF*
