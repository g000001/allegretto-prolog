;;;; -*- Mode: common-lisp; Syntax: Common-Lisp -*-
(declaim (optimize (speed 3) (safety 0) (compilation-speed 0)
                   (debug 0)))


(cl:in-package :cl-user)


(defpackage :zebra-3 (:use :cl :allegretto-prolog-3))


(in-package :zebra-3)


#+allegro (eval-when (compile) (setf excl:*load-xref-info* nil))


(<-- (nextto ?x ?y ?list) (iright ?x ?y ?list))


(<-  (nextto ?x ?y ?list) (iright ?y ?x ?list))


(<-- (iright ?left ?right (?left ?right . ?rest)))


(<-  (iright ?left ?right (?x . ?rest))
     (iright ?left ?right ?rest))


(<-- (zebra ?h ?w ?z)
     ;; Each house is of the form:
     ;; (house nationality pet cigarette drink house-color)
     (= ?h ((house norwegian ? ? ? ?)   ;1,10
            ?
            (house ? ? ? milk ?) ? ?))  ; 9
     (member (house englishman ? ? ? red) ?h) ; 2
     (member (house spaniard dog ? ? ?) ?h) ; 3
     (member (house ? ? ? coffee green) ?h) ; 4
     (member (house ukrainian ? ? tea ?) ?h) ; 5
     (iright (house ? ? ? ? ivory)      ; 6
             (house ? ? ? ? green) ?h)
     (member (house ? snails winston ? ?) ?h) ; 7
     (member (house ? ? kools ? yellow) ?h) ; 8
     (nextto (house ? ? chesterfield ? ?) ;11
             (house ? fox ? ? ?) ?h)
     (nextto (house ? ? kools ? ?)      ;12
             (house ? horse ? ? ?) ?h)
     (member (house ? ? luckystrike oj ?) ?h) ;13
     (member (house japanese ? parliaments ? ?) ?h) ;14
     (nextto (house norwegian ? ? ? ?)  ;15
             (house ? ? ? ? blue) ?h)
     (member (house ?w ? ? water ?) ?h) ;Q1
     (member (house ?z zebra ? ? ?) ?h) ;Q2
     )


;; This runs the query:

;; (?- (zebra ?houses ?water-drinker ?zebra-owner))

;; These are benchmarking and profiling functions.  
;; It is believed that solving zebra a
;; single time requires 12825 inferences.

(defun zebra-benchmark (&optional (n 1000))
  (declare (optimize (speed 3) (safety 0)))
  (let (rt0 rt1)
    (prolog-compile-symbols)
    (time (loop initially (setf rt0 (get-internal-run-time))
                repeat n do (prolog (zebra ?houses ?water-drinker ?zebra-owner)
                              !      ; Stop once answer is found.  
                                        ; This appears to be
                                        ; what other implementations do, 
                                        ; e.g. time/1 in
                                        ; SWI Prolog.
                              )
                finally (setf rt1 (get-internal-run-time))))
    (let (zebra-owner water-drinker houses)
      (prolog (zebra ?houses ?water-drinker ?zebra-owner)
        ;; Nearly any cons structure created by Prolog 
        ;; unification will be consed with
        ;; dynamic extent.  It isn't safe to return such 
        ;; structure outside the contour
        ;; that created it.  Prolog doesn't need to worry, 
        ;; since unification always
        ;; has dynamic extent, but arbitrary Lisp 
        ;; code needs to be careful.  The first
        ;; two values this function will return are 
        ;; symbols, but the third is a cons
        ;; tree created by Prolog unification.  In order 
        ;; to return it, the tree needs
        ;; to be copied with indefinite extent.
        (lisp (setf zebra-owner ?zebra-owner
                    water-drinker ?water-drinker
                    houses (copy-tree ?houses)))
        !)
      (pprint (list
               (format nil
                       "~:D-LIPS"
                       (floor (/ (* n 12825) (/ (- rt1 rt0) internal-time-units-per-second))))
               zebra-owner water-drinker houses)))))


;; (zebra-benchmark 1000)

;;; *EOF*


