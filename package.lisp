(defpackage :bld-utils
  (:use :common-lisp)
  (:import-from :alexandria :make-keyword)
  (:export :for :build-symbol :remove-nth :make-hash :make-hash* :with-keys :maphash2 :qsort :qsort-swaps :isort :isorta :bubble-sort :printhash :print-hash-key-or-val :lethash))
