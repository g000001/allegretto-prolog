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

;;; *EOF*
