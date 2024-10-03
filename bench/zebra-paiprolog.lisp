;;;; -*- Mode: common-lisp; Syntax: Common-Lisp -*-
(declaim (optimize (speed 3) (safety 0) (compilation-speed 0)
                   (debug 0)))

(cl:in-package :cl-user)

(defpackage :zebra-paiprolog (:use :cl :paiprolog))

(in-package :zebra-paiprolog)

#+allegro (eval-when (compile) (setf excl:*load-xref-info* nil))

(declaim (ftype function paiprolog::nextto/3 paiprolog::iright/3 paiprolog::member/2))

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
    (paiprolog::prolog-compile-symbols)
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
(flet ((run (name)
         (with-standard-io-syntax
           (format T "~&==== ~S ====~%" name))
         (funcall name)))
  (run 'paiprolog-zebra::zebra-benchmark)
)

==== PAIPROLOG-ZEBRA::ZEBRA-BENCHMARK ====
(LOOP INITIALLY (SETF RT0 (GET-INTERNAL-RUN-TIME)) REPEAT N DO (PROLOG (ZEBRA ?HOUSES ?WATER-DRINKER ?ZEBRA-OWNER) !) FINALLY (SETF RT1 (GET-INTERNAL-RUN-TIME)))
took 9,856,560 microseconds (9.856560 seconds) to run.
       278,122 microseconds (0.278122 seconds, 2.82%) of which was spent in GC.
During that period, and with 12 available CPU cores,
     9,065,370 microseconds (9.065370 seconds) were spent in user mode
       976,387 microseconds (0.976387 seconds) were spent in system mode
 2,347,636,736 bytes of memory allocated.
 132 minor page faults, 0 major page faults, 0 swaps.

==== PAIPROLOG-ZEBRA::ZEBRA-BENCHMARK ====
Warning: While compiling these undefined functions were referenced:
         PAIPROLOG::NEXTTO/3, PAIPROLOG::IRIGHT/3, PAIPROLOG::MEMBER/2.
; cpu time (non-gc) 8.994256 sec user, 0.054237 sec system
; cpu time (gc)     0.934203 sec user, 0.006855 sec system
; cpu time (total)  9.928459 sec user, 0.061092 sec system
; real time  9.898965 sec (100.9%)
; space allocation:
;  48,210,503 cons cells, 2,013,742,480 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 207), minor: 692 (gc: 207) ;


==== PAIPROLOG-ZEBRA::ZEBRA-BENCHMARK ====
Evaluation took:
  3.839 seconds of real time
  3.872145 seconds of total run time (3.852251 user, 0.019894 system)
  [ Real times consist of 0.018 seconds GC time, and 3.821 seconds non-GC time. ]
  [ Run times consist of 0.053 seconds GC time, and 3.820 seconds non-GC time. ]
  100.86% CPU
  3,000 lambdas converted
  12,256,968,067 processor cycles
  2,445,388,416 bytes consed


==== PAIPROLOG-ZEBRA::ZEBRA-BENCHMARK ====
Timing the evaluation of (loop initially (setf rt0 (get-internal-run-time))
                               repeat n
                               do (prolog (zebra ?houses ?water-drinker ?zebra-owner) !)
                               finally (setf rt1 (get-internal-run-time)))

User time    =       11.446
System time  =        0.074
Elapsed time =       11.433
Allocation   = 2074991840 bytes
1704 Page faults
Calls to %EVAL    6000
GC time      =        0.037
1113184.6
japanese
norwegian
((house norwegian fox kools water yellow)
 (house ukrainian horse chesterfield tea blue)
 (house englishman snails winston milk red)
 (house spaniard dog luckystrike oj ivory)
 (house japanese zebra parliaments coffee green))


==== PAIPROLOG-ZEBRA::ZEBRA-BENCHMARK ====
; cpu time (non-gc) 9.112027 sec user, 0.054290 sec system
; cpu time (gc)     0.963237 sec user, 0.008083 sec system
; cpu time (total)  10.075264 sec user, 0.062373 sec system
; real time  10.028476 sec (101.1%)
; space allocation:
;  48,210,486 cons cells, 2,090,715,616 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 285), minor: 375 (gc: 285)

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


