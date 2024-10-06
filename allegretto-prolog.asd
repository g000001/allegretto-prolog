;;;; allegretto-prolog.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)


(defsystem :allegretto-prolog
  :serial t
  :components ((:file "package")
               (:file "var")
               (:file "trail")
               (:file "allegretto-prolog")))


(setf (logical-pathname-translations "ALLEGRETTO-PROLOG")
      `(("**;*.*.*"
         ,(make-pathname :name :wild :type :wild
                         :directory
                         (append (pathname-directory (asdf:system-source-directory :allegretto-prolog))
                                 '(:wild-inferiors))
                         :case :common))))

(defun load-paiprolog ()
  (load (compile-file "allegretto-prolog:paiprolog")))


(defun load-0 ()
  (load (compile-file "allegretto-prolog:0")))


(defun load-1 ()
  (load (compile-file "allegretto-prolog:1")))


(defun cl-user::zebra-paiprolog ()
  (load (compile-file "allegretto-prolog:bench;zebra-paiprolog"))
  (funcall (find-symbol "ZEBRA-BENCHMARK" "ZEBRA-PAIPROLOG")))


(defun cl-user::zebra-allegretto-prolog ()
  (ignore-errors (mapc #'delete-package (package-used-by-list :allegretto-prolog)))
  (ignore-errors (delete-package :allegretto-prolog))
  (setq *features* (delete :cons-var *features*))
  (load-system :allegretto-prolog :force T)
  (load (compile-file "allegretto-prolog:bench;zebra-allegretto-prolog"))
  (funcall (find-symbol "ZEBRA-BENCHMARK" "ZEBRA-ALLEGRETTO-PROLOG")))


(defun cl-user::zebra-allegretto-prolog/cons-var ()
  (ignore-errors (mapc #'delete-package (package-used-by-list :allegretto-prolog)))
  (ignore-errors (delete-package :allegretto-prolog))
  (pushnew :cons-var *features*)
  (load-system :allegretto-prolog :force T)
  (load (compile-file "allegretto-prolog:bench;zebra-allegretto-prolog"))
  (funcall (find-symbol "ZEBRA-BENCHMARK" "ZEBRA-ALLEGRETTO-PROLOG")))




#||||


(defmethod operate ((operation compile-op)
                    (compo (eql (find-component :allegretto-prolog nil)))
                    &key)
  (dolist (file '("allegretto-prolog:paiprolog"
                  "allegretto-prolog:0"
                  "allegretto-prolog:1"
                  "allegretto-prolog:2"
                  "allegretto-prolog:3"))
    (compile-file file)))


(defmethod operate ((operation load-op)
                    (compo (eql (find-component :allegretto-prolog nil)))
                    &key)
  (dolist (file '("allegretto-prolog:paiprolog"
                  "allegretto-prolog:0"
                  "allegretto-prolog:1"
                  "allegretto-prolog:2"
                  "allegretto-prolog:3"))
    (load file)))

||||#


()


;;; *EOF*
