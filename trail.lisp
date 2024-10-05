;;; -*- mode: Lisp; coding: utf-8  -*-

(cl:in-package allegretto-prolog)


(declaim (type list *trail*))


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


;;; *EOF*
