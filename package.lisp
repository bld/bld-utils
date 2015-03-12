(defpackage :bld-utils
  (:use :common-lisp)
  (:import-from :alexandria :make-keyword :maphash-values :plist-hash-table)
  #|(:import-from :let-over-lambda :plambda)|#
  (:export :for 
	   :build-symbol 
	   :remove-nth 
	   :make-hash 
	   :make-hash* 
	   :with-keys 
	   :maphash2 
	   :maphash-values2
	   :qsort 
	   :qsort-swaps 
	   :isort 
	   :isorta 
	   :bubble-sort 
	   :printhash 
	   :print-hash-key-or-val 
	   :lethash 
	   :maptree
	   :diff
	   :nested-slot
	   :bind-nested-slots
	   :slot-ref
	   :slot-ref-set
	   :hash-ref
	   :hash-ref-set
	   #|:defpfun|#))
