(in-package "CL-USER")

#+allegro (require "prolog")
#+lispworks (ignore-errors (require "prolog"))

(setf (logical-pathname-translations "ALLEGRETTO-PROLOG")
      `(("**;*.*.*"
         ,(make-pathname :name :wild :type :wild
                         :directory
                         #+(or lispworks ccl clisp) (pathname-directory "/Users/mc/l/src/rw/allegretto-prolog/**/")
                         #-(or lispworks ccl clisp)
                         (append (pathname-directory (or *compile-file-pathname*
                                                         *load-pathname*)
                                                     :case :common)
                                 '(:wild-inferiors))
                         :case :common))))

(defun xit ()
  #+allegro (cl-user::exit)
  #+(or sbcl lispworks ccl ecl clisp) (cl-user::quit))


(defun bench-gc ()
  #+allegro (gc t)
  #+sbcl (sb-ext:gc #+gencgc :full #+gencgc t)
  #+ecl (si:gc t)
  #+lispworks (hcl:gc-generation t)
  #+ccl (ccl:gc))

(defun run ()
  ;;#-lispworks (dribble "/tmp/log.lisp")
  #+allegro (load (compile-file "allegretto-prolog:allegro-prolog"))
  #+allegro (load (compile-file "allegretto-prolog:allegro-moderato"))
  #+allegro (load (compile-file "allegretto-prolog:allegro-moderato2"))
  (load (compile-file "allegretto-prolog:paiprolog"))
  (load (compile-file "allegretto-prolog:0"))
  (load (compile-file "allegretto-prolog:1"))
  (load (compile-file "allegretto-prolog:2"))
  (load (compile-file "allegretto-prolog:3"))
  (load (compile-file "allegretto-prolog:4"))
  (load (compile-file "allegretto-prolog:5"))
  (load (compile-file "allegretto-prolog:6"))
  #+allegro (load (compile-file "allegretto-prolog:bench;zebra-allegro-prolog"))
  #+allegro (load (compile-file "allegretto-prolog:bench;zebra-allegro-moderato"))
  #+lispworks (and (ignore-errors (require "prolog"))
                   (load (compile-file "allegretto-prolog:bench;zebra-clog")))
  (load (compile-file "allegretto-prolog:bench;zebra-paiprolog"))
  (load (compile-file "allegretto-prolog:bench;zebra-0"))
  (load (compile-file "allegretto-prolog:bench;zebra-1"))
  (load (compile-file "allegretto-prolog:bench;zebra-2"))
  (load (compile-file "allegretto-prolog:bench;zebra-3"))
  (load (compile-file "allegretto-prolog:bench;zebra-4"))
  (load (compile-file "allegretto-prolog:bench;zebra-5"))
  (load (compile-file "allegretto-prolog:bench;zebra-6"))
  ;;
  (bench-gc)
  #+lispworks (and (ignore-errors (require "prolog"))
                   (funcall (print (find-symbol "ZEBRA-BENCHMARK" "ZEBRA-CLOG"))))
  (bench-gc)
  (funcall (print (find-symbol "ZEBRA-BENCHMARK" "ZEBRA-6")))
  (bench-gc)
  (funcall (print (find-symbol "ZEBRA-BENCHMARK" "ZEBRA-5")))
  (bench-gc)
  (funcall (print (find-symbol "ZEBRA-BENCHMARK" "ZEBRA-4")))
  (bench-gc)
  (funcall (print (find-symbol "ZEBRA-BENCHMARK" "ZEBRA-3")))
  (bench-gc)
  (funcall (print (find-symbol "ZEBRA-BENCHMARK" "ZEBRA-2")))
  (bench-gc)
  (funcall (print (find-symbol "ZEBRA-BENCHMARK" "ZEBRA-1")))
  (bench-gc)
  (funcall (print (find-symbol "ZEBRA-BENCHMARK" "ZEBRA-0")) #+(or ecl clisp) 10)
  (bench-gc)
  (funcall (print (find-symbol "ZEBRA-BENCHMARK" "ZEBRA-PAIPROLOG")) #+(or ecl clisp) 10)
  (bench-gc)
  #+allegro (funcall (print (find-symbol "ZEBRA-BENCHMARK" "ZEBRA-ALLEGRO-MODERATO")))
  (bench-gc)
  #+allegro (funcall (print (find-symbol "ZEBRA-BENCHMARK" "ZEBRA-ALLEGRO-PROLOG")))

  (let ((lisp-name (format nil "~A ~A" (lisp-implementation-type) (lisp-implementation-version)))
        (result ""))
    (setq result
          (with-output-to-string (out)
            (format out "~&~80@{;~}~%" T)
            (format out "~&;; ~A~%" lisp-name)
            (let ((pp-lips (car (eval (find-symbol "*ZEBRA-RESULT*" "ZEBRA-PAIPROLOG")))))
              (mapc (lambda (res)
                      (destructuring-bind (sym lips zebra-owner water-drinker houses) res
                        (format out
                                "~&;; ~25@A: ~13:DLIPS owner: ~A ~,2,,,FPP~%"
                                (package-name (symbol-package sym))
                                lips
                                zebra-owner
                                (/ lips pp-lips))))
                    (sort
                     (mapcar (lambda (sym)
                               (cons sym (eval sym)))
                             (remove nil
                                     (list
                                      (find-symbol "*ZEBRA-RESULT*" "ZEBRA-6")
                                      (find-symbol "*ZEBRA-RESULT*" "ZEBRA-5")
                                      (find-symbol "*ZEBRA-RESULT*" "ZEBRA-4")
                                      (find-symbol "*ZEBRA-RESULT*" "ZEBRA-3")
                                      (find-symbol "*ZEBRA-RESULT*" "ZEBRA-2")
                                      (find-symbol "*ZEBRA-RESULT*" "ZEBRA-1")
                                      (find-symbol "*ZEBRA-RESULT*" "ZEBRA-0")
                                      #+lispworks (and (ignore-errors (require "prolog"))
                                                       (find-symbol "*ZEBRA-RESULT*" "ZEBRA-CLOG"))
                                      (find-symbol "*ZEBRA-RESULT*" "ZEBRA-PAIPROLOG")
                                      #+allegro (find-symbol "*ZEBRA-RESULT*" "ZEBRA-ALLEGRO-MODERATO")
                                      #+allegro (find-symbol "*ZEBRA-RESULT*" "ZEBRA-ALLEGRO-PROLOG")
                                      )))
                     #'>
                     :key #'second)))
            (format out "~&~80@{;~}~%" T)))
    (with-open-file (outfile (format nil "/tmp/zebra-~A.txt" (sxhash lisp-name))
                             :direction :output
                             :if-does-not-exist :create
                             :if-exists :supersede)
      (write-string result outfile))
    (format T "~&~A~%" result))
  
  ;;#-lispworks (dribble)
  (xit))

(progn
  ;; sbcl --no-userinit --load zebra.lisp --eval "(run)"
  ;; /Applications/AllegroCL64express.app/Contents/Resources/alisp -L zebra.lisp -e "(run)"
  ;; lw80-console -init - -load zebra.lisp -eval "(run)"
  ;; ccl64 -l zebra.lisp -e "(run)"
  ;; ecl --norc --load zebra.lisp --eval "(run)"
  ;; swi-prolog LIPS is 12852844 / 1.869.  LIPS = 6876856.072766185.
  ;;swipl -l zebra.pl -g 'benchmark,halt.'
  ;;% 12,877,724 inferences, 1.703 CPU in 1.729 seconds (98% CPU, 7,562,920 Lips)
  
  )
;;; *EOF*



;; sbcl(6,445,494-LIPS JAPANESE NORWEGIAN
;; ap (9,031,690-LIPS JAPANESE NORWEGIAN
;; atto (2,666,320-LIPS JAPANESE NORWEGIAN
;; lw (2,826,757-LIPS JAPANESE NORWEGIAN ((HOUSE NORWEGIAN FOX KOOLS WATER YELLOW) (HOUSE UKRAINIAN HORSE CHESTERFIELD TEA BLUE) (HOUSE ENGLISHMAN SNAILS WINSTON MILK RED) (HOUSE SPANIARD DOG LUCKYSTRIKE OJ IVORY) (HOUSE JAPANESE ZEBRA PARLIAMENTS COFFEE GREEN)))


#||
allegro
("9,031,690-LIPS" ZEBRA-ALLEGRO-PROLOG::JAPANESE
("1,099,914-LIPS" ZEBRA-PAIPROLOG::JAPANESE ZEBRA-PAIPROLOG::NORWEGIAN
("1,274,850-LIPS" ZEBRA-0::JAPANESE ZEBRA-0::NORWEGIAN
("2,655,279-LIPS" ZEBRA-1::JAPANESE ZEBRA-1::NORWEGIAN
("1,819,148-LIPS" ZEBRA-2::JAPANESE ZEBRA-2::NORWEGIAN

SBCL
("3,213,605-LIPS" ZEBRA-PAIPROLOG::JAPANESE ZEBRA-PAIPROLOG::NORWEGIAN
("4,550,408-LIPS" ZEBRA-0::JAPANESE ZEBRA-0::NORWEGIAN
("3,585,047-LIPS" ZEBRA-1::JAPANESE ZEBRA-1::NORWEGIAN
("6,520,382-LIPS" ZEBRA-2::JAPANESE ZEBRA-2::NORWEGIAN

LispWorks
("1,499,298-LIPS" pp
("2,689,243-LIPS" 0
("2,995,095-LIPS" 1
("2,877,496-LIPS" 2

 
||#

