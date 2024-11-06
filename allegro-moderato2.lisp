(in-package "ALLEGRO-MODERATO")

(defmacro prolog (&rest goals)
  "Run Prolog in the surrounding Lisp environment
which is accessed from lisp functor.
"
  (let ((*predicate* 'prolog-clauses))
    `(block prolog
       (flet ((,*predicate* (trail cont)
                (declare (ignorable trail cont))
                ,(let* ((vars (variables-in goals)))
                   `(let-de (,@(mapcar (lambda (v) `(,v (?))) vars))
                      ,(compile-body* 'trail goals 'cont nil)))))
         (declare (dynamic-extent #',*predicate*))
         (let ((trail (fast *trail*)))
           (if trail
               (,*predicate* trail #'ignorer)
               (let ((trail (allocate-trail default-trail-size)))
                 (let ((*trail* trail))
                   (,*predicate* trail #'ignorer)))))))))


(defmacro prolog-collect ((&rest vars) &body body)
  "collect all bindings of vars"
  (when (null vars)
    (error "must specify vars."))
  (let ((result (gensym "result")))
    `(let (,result)
       (prolog
        ,@body
        ,@(when vars
            `((lisp (push ,(if (length=1 vars)
                               (car vars)
                               `(list ,@vars))
                          ,result)))))
       ,result)))


(defmacro prolog-first ((&rest vars) &body body)
  "return first bindding of vars"
  (when (null vars)
    (error "must specify vars."))
  `(prolog
     ,@body
     (lisp (return-from prolog ,(if (length=1 vars)
                                    (car vars)
                                    `(values ,@vars))))))


(progn
  ;;(<-- (member ?item (?item . ?rest)))
  ;;(<-  (member ?item (?x . ?rest)) (member ?item ?rest))

  ;;#++
  (tail-recursive-defun member/2 (trail ?arg1 ?arg2 cont)
    (let ((old-trail (trail-ndx trail)))
      (let ((?rest (?)))
        (if (unify! trail ?arg2 (cons ?arg1 ?rest))
            (funcall cont)))
      (undo-bindings! trail old-trail)
      (let ((?rest (?)))
        (if (unify! trail ?arg2 (cons (?) ?rest))
            (member/2 trail ?arg1 ?rest cont))))))

(progn
  (<-- (length () 0))
  (<-  (length (?x . ?y) ?n)
       (length ?y ?n1)
       (is ?n (1+ ?n1))))


(prolog-collect (?item)
  (member ?item (1 2 3)))

