;;;; package.lisp

(defpackage #:lisphys
  (:use #:cl)
  (:shadowing-import-from #:clunit 
			  #:defsuite 
			  #:deftest
			  #:run-suite
			  #:assert-equal
			  #:assert-true)
  (:shadowing-import-from #:alexandria 
			  #:symbolicate)
  (:shadowing-import-from #:infpre 
			  #:!!
			  #:infix->prefix))
