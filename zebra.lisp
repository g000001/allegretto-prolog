(in-package "CL-USER")


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
  (load (compile-file "allegretto-prolog:paiprolog"))
  (load (compile-file "allegretto-prolog:0"))
  (load (compile-file "allegretto-prolog:1"))
  (load (compile-file "allegretto-prolog:2"))
  (load (compile-file "allegretto-prolog:3"))
  (load (compile-file "allegretto-prolog:4"))
  (load (compile-file "allegretto-prolog:5"))
  #+allegro (load (compile-file "allegretto-prolog:bench;zebra-allegro-prolog"))
  (load (compile-file "allegretto-prolog:bench;zebra-paiprolog"))
  (load (compile-file "allegretto-prolog:bench;zebra-0"))
  (load (compile-file "allegretto-prolog:bench;zebra-1"))
  (load (compile-file "allegretto-prolog:bench;zebra-2"))
  (load (compile-file "allegretto-prolog:bench;zebra-3"))
  (load (compile-file "allegretto-prolog:bench;zebra-4"))
  (load (compile-file "allegretto-prolog:bench;zebra-5"))

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
  #+allegro (funcall (print (find-symbol "ZEBRA-BENCHMARK" "ZEBRA-ALLEGRO-PROLOG")))
  
  
  
  
  
  
  
  ;;#-lispworks (dribble)
  (xit))

(progn
  ;; sbcl --no-userinit --load zebra.lisp --eval "(run)"
  ;; /Applications/AllegroCL64express.app/Contents/Resources/alisp -L zebra.lisp -e "(run)"
  ;; lw80-console -init - -load zebra.lisp -eval "(run)"
  ;; ccl64 -l zebra.lisp -e "(run)"
  ;; ecl --norc --load zebra.lisp --eval "(run)"
  ;; swi-prolog LIPS is 12852844 / 1.869.  LIPS = 6876856.072766185.
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
