;;; -*- mode: Lisp; coding: utf-8  -*-

(cl:in-package allegretto-prolog)


(deftype adim ()
  `(integer 0 ,(1- array-total-size-limit)))


(defconstant unbound (if (boundp 'unbound)
                         (symbol-value 'unbound)
                         "Unbound"))


(declaim (type fixnum *var-counter*))


(defvar *var-counter* 0)


(defstruct (var (:constructor ? ())
                (:print-function print-var))
  (name (incf *var-counter*) :type adim)
  (binding unbound))


(defun bound-p (var) (not (eq (var-binding var) unbound)))


(defmacro deref (exp)
  "Follow pointers for bound variables."
  `(progn (loop while (and (var-p ,exp) (bound-p ,exp))
                do (setf ,exp (var-binding ,exp)))
          ,exp))


(defun unbound-var-p (exp)
  "Is EXP an unbound var?"
  (and (var-p exp) (not (bound-p exp))))


(defun nonvarp (exp)
  (not (unbound-var-p exp)))


(defun print-var (var stream depth)
  (if (or (and *print-level*
               (>= depth *print-level*))
          (var-p (deref var)))
      (format stream "?~A" (var-name var))
      (write var :stream stream)))


;;; *EOF*
