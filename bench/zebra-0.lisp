;;;; -*- Mode: common-lisp; Syntax: Common-Lisp -*-
#-ccl
(declaim (optimize (speed 3) (safety 0) (compilation-speed 0)
                   (debug 0)))

(cl:in-package :cl-user)

(defpackage :zebra-0 (:use :cl :allegretto-prolog-0))

(in-package :zebra-0)

#+allegro (eval-when (compile) (setf excl:*load-xref-info* nil))

(declaim (ftype function
                allegretto-prolog-0::nextto/3
                allegretto-prolog-0::iright/3
                allegretto-prolog-0::member/2))

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
                repeat n do (prolog* (zebra ?houses ?water-drinker ?zebra-owner)
                                     !  ; Stop once answer is found.  
                                        ; This appears to be
                                        ; what other implementations do, 
                                        ; e.g. time/1 in
                                        ; SWI Prolog.
                                     )
                finally (setf rt1 (get-internal-run-time))))
    (let (zebra-owner water-drinker houses)
      (prolog* (zebra ?houses ?water-drinker ?zebra-owner)
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

#||

(prolog-collect (?houses ?water-drinker ?zebra-owner)
  (zebra ?houses ?water-drinker ?zebra-owner)
                                  !     ; Stop once answer is found.  
                                        ; This appears to be
                                        ; what other implementations do, 
                                        ; e.g. time/1 in
                                        ; SWI Prolog.
)
((((HOUSE NORWEGIAN FOX KOOLS WATER YELLOW)
   (HOUSE UKRAINIAN HORSE CHESTERFIELD TEA BLUE)
   (HOUSE ENGLISHMAN SNAILS WINSTON MILK RED)
   (HOUSE SPANIARD DOG LUCKYSTRIKE OJ IVORY)
   (HOUSE JAPANESE ZEBRA PARLIAMENTS COFFEE GREEN))
  NORWEGIAN JAPANESE))

(flet ((run (name)
         (with-standard-io-syntax
           (format T "~&==== ~S ====~%" name))
         (funcall name)))
(run 'allegretto-prolog-zebra::zebra-benchmark))

==== ALLEGRETTO-PROLOG-ZEBRA::ZEBRA-BENCHMARK ====
(LOOP INITIALLY (SETF RT0 (GET-INTERNAL-RUN-TIME)) REPEAT N DO (PROLOG* (ZEBRA ?HOUSES ?WATER-DRINKER ?ZEBRA-OWNER) !) FINALLY (SETF RT1 (GET-INTERNAL-RUN-TIME)))
took 6,370,057 microseconds (6.370057 seconds) to run.
       151,265 microseconds (0.151265 seconds, 2.37%) of which was spent in GC.
During that period, and with 12 available CPU cores,
     5,796,202 microseconds (5.796202 seconds) were spent in user mode
       712,646 microseconds (0.712646 seconds) were spent in system mode
 1,753,975,328 bytes of memory allocated.
 3 minor page faults, 0 major page faults, 0 swaps.


took 7,636,172 microseconds (7.636172 seconds) to run.
       475,761 microseconds (0.475761 seconds, 6.23%) of which was spent in GC.
During that period, and with 12 available CPU cores,
     6,887,217 microseconds (6.887217 seconds) were spent in user mode
       937,176 microseconds (0.937176 seconds) were spent in system mode
 2,346,020,608 bytes of memory allocated.
 587 minor page faults, 0 major page faults, 0 swaps.


var:cons
==== ALLEGRETTO-PROLOG-ZEBRA::ZEBRA-BENCHMARK ====
; cpu time (non-gc) 5.200938 sec user, 0.011981 sec system
; cpu time (gc)     0.423439 sec user, 0.002041 sec system
; cpu time (total)  5.624377 sec user, 0.014022 sec system
; real time  5.539054 sec (101.8%)
; space allocation:
;  85,285,242 cons cells, 219,241,184 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 227), minor: 499 (gc: 227)

2282028.2 

==== ALLEGRETTO-PROLOG-ZEBRA::ZEBRA-BENCHMARK ====
; cpu time (non-gc) 6.725866 sec user, 0.045534 sec system
; cpu time (gc)     0.921930 sec user, 0.007520 sec system
; cpu time (total)  7.647796 sec user, 0.053054 sec system
; real time  7.565958 sec (101.8%)
; space allocation:
;  48,210,506 cons cells, 2,013,834,560 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 457), minor: 1993 (gc: 457)

1676470.5 


==== ALLEGRETTO-PROLOG-ZEBRA::ZEBRA-BENCHMARK ====
; cpu time (non-gc) 8.869127 sec user, 0.051529 sec system
; cpu time (gc)     0.857259 sec user, 0.010354 sec system
; cpu time (total)  9.726386 sec user, 0.061883 sec system
; real time  9.748628 sec (100.4%)
; space allocation:
;  48,210,644 cons cells, 2,029,432,288 other bytes, 0 static bytes
; Page Faults: major: 3 (gc: 3287), minor: 6610 (gc: 3287)

1319444.4 


==== ALLEGRETTO-PROLOG-ZEBRA::ZEBRA-BENCHMARK ====
Evaluation took:
  2.719 seconds of real time
  2.773505 seconds of total run time (2.759997 user, 0.013508 system)
  [ Real times consist of 0.028 seconds GC time, and 2.691 seconds non-GC time. ]
  [ Run times consist of 0.083 seconds GC time, and 2.691 seconds non-GC time. ]
  102.02% CPU
  3,000 lambdas converted
  8,679,670,659 processor cycles
  2,445,931,008 bytes consed
  

4624.126

==== ALLEGRETTO-PROLOG-ZEBRA::ZEBRA-BENCHMARK ====
Evaluation took:
  3.021 seconds of real time
  3.108387 seconds of total run time (2.992234 user, 0.116153 system)
  [ Real times consist of 0.090 seconds GC time, and 2.931 seconds non-GC time. ]
  [ Run times consist of 0.180 seconds GC time, and 2.929 seconds non-GC time. ]
  102.88% CPU
  3,000 lambdas converted
  9,643,533,173 processor cycles
  2,446,513,152 bytes consed
  

4125.9434


==== ALLEGRETTO-PROLOG-ZEBRA::ZEBRA-BENCHMARK ====
Timing the evaluation of (loop initially (setf rt0 (get-internal-run-time))
                               repeat n
                               do (prolog (zebra ?houses ?water-drinker ?zebra-owner) !)
                               finally (setf rt1 (get-internal-run-time)))

User time    =       12.443
System time  =        0.081
Elapsed time =       12.435
Allocation   = 2074830536 bytes
7111 Page faults
Calls to %EVAL    6000
GC time      =        0.069
1023952.1 
1023952.1

;; use (fast)
==== ALLEGRETTO-PROLOG-ZEBRA::ZEBRA-BENCHMARK ====
; cpu time (non-gc) 6.361157 sec user, 0.014437 sec system
; cpu time (gc)     0.891580 sec user, 0.003483 sec system
; cpu time (total)  7.252737 sec user, 0.017920 sec system
; real time  7.174760 sec (101.3%)
; space allocation:
;  48,210,480 cons cells, 2,084,971,120 other bytes, 0 static bytes
; Page Faults: major: 1 (gc: 286), minor: 1055 (gc: 286)

1766528.7 

==== ALLEGRETTO-PROLOG-ZEBRA::ZEBRA-BENCHMARK ====
; cpu time (non-gc) 7.222903 sec user, 0.051332 sec system
; cpu time (gc)     1.141527 sec user, 0.009809 sec system
; cpu time (total)  8.364430 sec user, 0.061141 sec system
; real time  8.207557 sec (102.7%)
; space allocation:
;  48,210,487 cons cells, 2,074,996,288 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 585), minor: 949 (gc: 585)

1534090.7 

==== PAIPROLOG-ZEBRA::ZEBRA-BENCHMARK ====
Timing the evaluation of (LOOP PAIPROLOG-ZEBRA::INITIALLY (SETF PAIPROLOG-ZEBRA::RT0 (GET-INTERNAL-RUN-TIME)) PAIPROLOG:REPEAT PAIPROLOG-ZEBRA::N DO (PAIPROLOG:PROLOG (PAIPROLOG-ZEBRA::ZEBRA PAIPROLOG-ZEBRA::?HOUSES PAIPROLOG-ZEBRA::?WATER-DRINKER PAIPROLOG-ZEBRA::?ZEBRA-OWNER) PAIPROLOG:!) PAIPROLOG-ZEBRA::FINALLY (SETF PAIPROLOG-ZEBRA::RT1 (GET-INTERNAL-RUN-TIME)))

User time = 10.053 System time = 0.000 Elapsed time = 10.037
Allocation = 2010241232 bytes 0 Page faults Calls to %EVAL 6000 ====
CLOG-ZEBRA::ZEBRA-BENCHMARK ==== Timing the evaluation of (LOOP
CLOG-ZEBRA::INITIALLY (SETF CLOG-ZEBRA::RT0 (GET-INTERNAL-RUN-TIME))
COMMON-PROLOG:REPEAT CLOG-ZEBRA::N DO (COMMON-PROLOG:LOGIC (QUOTE
(CLOG-ZEBRA::ZEBRA CLOG-ZEBRA::?H CLOG-ZEBRA::?W CLOG-ZEBRA::?Z))
:RETURN-TYPE :FILL) CLOG-ZEBRA::FINALLY (SETF CLOG-ZEBRA::RT1
(GET-INTERNAL-RUN-TIME)))

User time = 2.850 System time = 0.000 Elapsed time = 2.848 Allocation
= 447907880 bytes 0 Page faults ==== CLOG-ZEBRA::ZEBRA-BENCHMARK/ ====
Timing the evaluation of (LOOP CLOG-ZEBRA::INITIALLY (SETF
CLOG-ZEBRA::RT0 (GET-INTERNAL-RUN-TIME)) COMMON-PROLOG:REPEAT
CLOG-ZEBRA::N DO (COMMON-PROLOG:LOGIC (QUOTE (CLOG-ZEBRA::ZEBRA
CLOG-ZEBRA::?H CLOG-ZEBRA::?W CLOG-ZEBRA::?Z)) :RETURN-TYPE :FILL)
CLOG-ZEBRA::FINALLY (SETF CLOG-ZEBRA::RT1 (GET-INTERNAL-RUN-TIME)))

User time = 2.850 System time = 0.000 Elapsed time = 2.848 Allocation
= 447896736 bytes 0 Page faults 4498421.5 NIL NIL
NIL
||#
;;;  zebra.cl end


