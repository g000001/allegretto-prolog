(in-package "CL-USER")


(setf (logical-pathname-translations "ALLEGRETTO-PROLOG")
      `(("**;*.*.*"
         ,(make-pathname :name :wild :type :wild
                         :directory
                         (append (pathname-directory (or *compile-file-pathname*
                                                         *load-pathname*)
                                                     :case :common)
                                 '(:wild-inferiors))
                         :case :common))))


(defun load-paiprolog ()
  (load (compile-file "allegretto-prolog:paiprolog")))


(defun load-0 ()
  (load (compile-file "allegretto-prolog:0")))


(defun load-1 ()
  (load (compile-file "allegretto-prolog:1")))


(defun zebra-paiprolog ()
  (load (compile-file "allegretto-prolog:bench;zebra-paiprolog"))
  (funcall (find-symbol "ZEBRA-BENCHMARK" "ZEBRA-PAIPROLOG")))


;;; *EOF*
