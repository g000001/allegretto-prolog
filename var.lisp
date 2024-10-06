;;; -*- mode: Lisp; coding: utf-8  -*-

(declaim (optimize (speed 3) (safety 0) (compilation-speed 0) (debug 0)))

(cl:in-package allegretto-prolog)

#+cons-var
(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant var-tag
  (if (boundp 'var-tag)
      (symbol-value 'var-tag)
      "PrologVar")))

(deftype adim ()
  `(integer 0 ,(1- array-total-size-limit)))


(defconstant unbound (if (boundp 'unbound)
                         (symbol-value 'unbound)
                         "Unbound"))


(declaim (type fixnum *var-counter*))


(defvar *var-counter* 0)

#-cons-var
(defstruct (var (:constructor ? ())
                (:print-function print-var))
  (name (incf *var-counter*) :type adim)
  (binding unbound))

#+cons-var
(deftype var ()
  `(cons (eql ,var-tag) T))

#+cons-var
(defun var-p (obj)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (and (consp obj)
       (eq var-tag (car obj))))

#+cons-var
(defun ? ()
  (cons var-tag unbound))

#+cons-var
(define-compiler-macro ? ()
  `(cons (load-time-value var-tag)
         (load-time-value unbound)))

#+cons-var
(defmacro var-binding (var)
  `(cdr ,var))

#+cons-var
(defun var-name (var)
  (cdr var))


(defun bound-p (var) (not (eq (var-binding var) unbound)))


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


(defun unbound-var-p (exp)
  "Is EXP an unbound var?"
  (and (var-p exp) (not (bound-p exp))))


(defun nonvarp (exp)
  (not (unbound-var-p exp)))

#-cons-var
(defun print-var (var stream depth)
  (if (or (and *print-level*
               (>= depth *print-level*))
          (var-p (deref var)))
      (format stream "?~A" (var-name var))
      (write var :stream stream)))


;;; *EOF*
