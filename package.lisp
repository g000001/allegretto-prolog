;;; -*- mode: Lisp; coding: utf-8  -*-

(cl:in-package "BCL-USER")


(defpackage allegretto-prolog
  (:use cl)
  (:export
   #:! #:*leash-limit* #:*prolog-leash-indent-wrap*
   #:*prolog-stack-limit* #:<- #:<-- #:== #:? #:?! #:?* #:?*! #:?- #:??
   #:abolish #:arg #:asserta #:assertz #:atomic #:bagof #:call #:consult
   #:copy-term #:cut #:define-prolog-compiler-macro #:erase #:fail #:functor
   #:generating #:generating* #:generator #:generator* #:ground #:is
   #:leash #:leash-1 #:lisp #:lisp! #:lisp* #:lisp*! #:lispp #:lispp!
   #:lispp!* #:lispp* #:listing #:memberp #:nl #:nonvar #:optional
   #:prolog #:prolog-compile #:prolog-compile-symbols
   #:prolog-compiler-macro #:prolog-stack-overflow
   #:prolog-stack-overflow-length #:recorda #:recorded #:recordz #:repeat
   #:retract #:retractall #:rev #:setof #:slot-value! #:slot= #:slot=*
   #:true #:unleash #:unleash-1 #:var))


'(:pred
  #:=/2 #:==/2 #:abolish/2 #:and/* #:append/3 #:arg/3 #:assert/1
  #:asserta/1 #:assertz/1 #:atom/1 #:atomic/1 #:bagof/3 #:call/1
  #:consult/1 #:copy-term/2 #:erase/1 #:fail/0 #:first/2
  #:functor/3 #:generator*/2 #:generator/2 #:ground/1 #:if*/2
  #:if*/3 #:if/2 #:if/3 #:irev/2 #:irev3/3 #:is/2 #:last/2
  #:length/2 #:lisp/1 #:lisp/2 #:lispp!/1 #:lispp/1 #:listing/1
  #:member/2 #:memberp/2 #:nl/0 #:nl/1 #:nonvar/1 #:not/1
  #:number/1 #:or/* #:princ/1 #:princ/2 #:read/1 #:read/2 #:read/3
  #:read/4 #:recorda/1 #:recorded/2 #:recordz/1 #:repeat/0
  #:rest/2 #:retract/1 #:retractall/1 #:rev/2 #:setof/3 #:setof/4
  #:show-prolog-vars/2 #:slot-value!/3 #:slot-value/3 #:slot=*/3
  #:slot=/3 #:true/0 #:var/1 #:write/1 #:write/2)


'(:prolog-compiler-macro
  #:lispp*!  #:weird-if-semantics #:lispp* #:lisp*!  #:or #:and
  #:if #:true #:not #:lisp* #:is #:= #:if* #:lisp #:call #:lispp!
  #:setof #:bagof #:var #:lisp!  #:fail #:lispp)


'(:prolog-macro
  #:generating
  #:optional
  #:generating*)


;;; *EOF*
