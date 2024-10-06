;;; -*- mode: Lisp; coding: utf-8  -*-

(cl:in-package allegretto-prolog)


(<-- (append () ?x ?x))
(<-  (append (?x . ?xs) ?ys (?x . ?zs))
     (append ?xs ?ys ?zs))

(<-- (reverse () ()))
(<-  (reverse (?x . ?xs) ?ys)
     (reverse ?xs ?zs)
     (append ?zs (?x) ?ys))

(let (beg end (times 10000))
  (prolog-compile-symbols)
  (setq beg (get-internal-real-time))
  (dotimes (i times)
    (progn;let (ans)
      (prolog
       (reverse (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29) ?ans))))
  (setq end (get-internal-real-time))
  (format nil
          "~A ~A: ~:D-LIPS"
          (lisp-implementation-type)
          (lisp-implementation-version)
          (floor (* 496 times)
                 (/ (- end beg) internal-time-units-per-second))))


(prolog
 (let x 42)
 (= ?x 43)
 (lisp (= x ?x))
 (lisp (print 'yes)))

