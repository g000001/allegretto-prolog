;;;; -*- Mode: common-lisp; Syntax: Common-Lisp -*-
(declaim (optimize (speed 3) (safety 0) (compilation-speed 0)
                   (debug 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "prolog"))

(defpackage :zebra-clog (:use :cl :clog))

(in-package :zebra-clog)

(defvar *zebra-result* "")

;;(rqp)

(defrel nextto
  ((nextto ?x ?y ?list) (iright ?x ?y ?list))
  ((nextto ?x ?y ?list) (iright ?y ?x ?list)))

(defrel iright
  ((iright ?left ?right (?left ?right . ?rest)))
  ((iright ?left ?right (?x . ?rest))
   (iright ?left ?right ?rest)))

(defrel zebra
  ((zebra ?h ?w ?z)
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
   ))

;; (logic '(zebra ?h ?w ?z) :return-type :fill)


;; type 1
(defun zebra-benchmark (&optional (n 1000))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let (rt0 rt1)
    (time (loop initially (setf rt0 (get-internal-run-time))
                repeat n do (logic '(zebra ?h ?w ?z) :return-type :fill)
                finally (setf rt1 (get-internal-run-time))))
    (destructuring-bind (houses water-drinker zebra-owner)
                        (logic '(zebra ?houses ?water-drinker ?zebra-owner)
                               :return-type :bag
                               :bag-exp '(?houses ?water-drinker ?zebra-owner))
      (setq *zebra-result*
            (list (floor (/ (* n 12825) (/ (- rt1 rt0) internal-time-units-per-second)))
                  zebra-owner water-drinker houses))
      (pprint *zebra-result*))))

;; type 2
(defun zebra-benchmark/ (&optional (n 1000))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let (rt0 rt1)
    (time (loop initially (setf rt0 (get-internal-run-time))
                repeat n do (logic '(zebra ?h ?w ?z) :return-type :fill)
                finally (setf rt1 (get-internal-run-time))))
    (let (zebra-owner water-drinker houses)
      (with-prolog 
       (zebra ?houses ?water-drinker ?zebra-owner)
       ((setf ?.zebra-owner ?zebra-owner
              ?.water-drinker ?water-drinker
              ?.houses (copy-tree ?houses))))
      (setq *zebra-result*
            (list (floor (/ (* n 12825) (/ (- rt1 rt0) internal-time-units-per-second)))
                  zebra-owner water-drinker houses))
      (pprint *zebra-result*))))

#||

(zebra-clog::zebra-benchmark)

(zebra-clog::zebra-benchmark/)

||# 

;;(halt) 




