(in-package :bld-utils)

(defmacro for (listspec exp)
  "From the Common Lisp Cookbook - http://cl-cookbook.sourceforge.net/macros.html"
   (cond ((and (= (length listspec) 3)
               (symbolp (car listspec))
               (eq (cadr listspec) ':in))
          `(mapcar (lambda (,(car listspec))
                      ,exp)
                   ,(caddr listspec)))
         (t (error "Ill-formed: %s" `(for ,listspec ,exp)))))

(defun symstuff (l)
  "From the Common Lisp Cookbook - http://cl-cookbook.sourceforge.net/macros.html
Helper function to (build-symbol)"   
  `(concatenate 'string
    ,@(for (x :in l)
           (cond ((stringp x)
                  `',x)
                 ((atom x)
                  `',(format nil "~a" x))
                 ((eq (car x) ':<)
                  `(format nil "~a" ,(cadr x)))
                 ((eq (car x) ':++)
                  `(format nil "~a" (incf ,(cadr x))))
                 (t
                  `(format nil "~a" ,x))))))

(defmacro build-symbol (&rest l)
  "From the Common Lisp Cookbook - http://cl-cookbook.sourceforge.net/macros.html"
  (let ((p (find-if (lambda (x) (and (consp x) (eq (car x) ':package)))
		    l)))
    (cond (p
	   (setq l (remove p l))))
    (let ((pkg (cond ((eq (cadr p) 'nil)
		      nil)
		     (t `(find-package ',(cadr p))))))
      (cond (p
	     (cond (pkg
		    `(values (intern ,(symstuff l) ,pkg)))
		   (t
		    `(make-symbol ,(symstuff l)))))
	    (t
	     `(values (intern ,(symstuff l))))))))

(defun remove-nth (n seq)
  "Remove nth element from sequence"
  (remove-if (constantly t) seq :start n :count 1))

(defun make-hash (&rest keyvals)
  "Create a hash table given keys and values"
  (let ((h (make-hash-table :test #'equal)))
    (loop while keyvals
       for k = (pop keyvals)
       for v = (pop keyvals)
       when v do (setf (gethash k h) v))
    h))

(defmacro make-hash* (&rest keyvals)
  "Make a hash table given key/value pairs, allowing use of prior key/val pairs in later definitions"
  (loop while keyvals
     for k = (intern (symbol-name (pop keyvals)))
     for v = (pop keyvals)
     collect `(,k ,v) into letargs
     collect (make-keyword k) into objargs
     collect k into objargs
     finally (return
	       `(let* (,@letargs)
		  (make-hash ,@objargs)))))
(defun maphash2 (fn ht)
  "Returns a hash-table with the results of the function of key & value as values"
  (let ((ht-out (make-hash-table)))
    (maphash #'(lambda (k v)
		 (setf (gethash k ht-out) (funcall fn k v)))
	     ht)
    ht-out))

(defun maphash-values2  (fn ht)
  "Returns a hash-table with the results of the function of value as values"
  (let ((ht-out (make-hash-table)))
    (maphash #'(lambda (k v) (setf (gethash k ht-out) (funcall fn v))) ht)
    ht-out))

(defun qsort (lst &key (key #'identity) (predicate #'<))
  "Quicksort a list - in 7 lines. Optional KEY and PREDICATE functions like SORT."
  (when lst
    (let* ((x (car lst))
           (xs (cdr lst))
           (lt (loop for y in xs when (funcall predicate (funcall key y) (funcall key x)) collect y))
           (gte (loop for y in xs when (not (funcall predicate (funcall key y) (funcall key x))) collect y)))
      (append (qsort lt :key key :predicate predicate) (list x) (qsort gte :key key :predicate predicate)))))

#+null(defun qsort-swaps (lst)
  "Quicksort a list, counting the number of swaps a bubble-sort would have performed"
  (let ((swaps 0) ; initialize # swaps and result
        (res nil))
    (when lst ; if non-nil...
      (let* ((x (car lst)) ; 1st element
             (xs (cdr lst)) ; rest of list
             (lt (loop for y in xs ; xs < x
                       for i = 0 then (1+ i) ; list counter
                       with lti = 0 ; # of elements collected
                       when (< y x) collect y ; collect if <
                       and do ; and increment swaps & lti
                       (incf swaps (1+ (- i lti)))
                       (incf lti)))
             (gte (loop for y in xs
                        when (>= y x) collect y))) ; xs >= x. No swaps.
        ;; Sort lt & gte, and increments swaps by the #'s returned
        (multiple-value-bind (ltsort ltswaps) (qsort-swaps lt)
          (multiple-value-bind (gtesort gteswaps) (qsort-swaps gte)
            (incf swaps (+ ltswaps gteswaps))
            ;; append sorted lt, x, and sorted gte
            (setq res (append ltsort (list x) gtesort))))))
    (values res swaps))) ; return result and # of swaps

(defun qsort-swaps (lst &key (key #'identity) (predicate #'<))
  "Quicksort a list, counting the number of swaps a bubble-sort would have performed"
  (let ((swaps 0) ; initialize # swaps and result
        (res nil))
    (when lst ; if non-nil...
      (let* ((x (car lst)) ; 1st element
             (xs (cdr lst)) ; rest of list
             (lt (loop for y in xs ; xs < x
                       for i = 0 then (1+ i) ; list counter
                       with lti = 0 ; # of elements collected
                       when (funcall predicate (funcall key y) (funcall key x)) collect y ; collect if <
                       and do ; and increment swaps & lti
                       (incf swaps (1+ (- i lti)))
                       (incf lti)))
             (gte (loop for y in xs
                        when (not (funcall predicate (funcall key y) (funcall key x))) collect y))) ; xs >= x. No swaps.
        ;; Sort lt & gte, and increments swaps by the #'s returned
        (multiple-value-bind (ltsort ltswaps) (qsort-swaps lt :key key :predicate predicate)
          (multiple-value-bind (gtesort gteswaps) (qsort-swaps gte :key key :predicate predicate)
            (incf swaps (+ ltswaps gteswaps))
            ;; append sorted lt, x, and sorted gte
            (setq res (append ltsort (list x) gtesort))))))
    (values res swaps))) ; return result and # of swaps


(defun isorta (a &optional (pred #'<=) (l 0) (r (length a)))
  "Destructive Insertion sort 1D array. Also returns number of swaps a bubble sort would have done.
A: 1D array
PRED: predicate function
L: left index
R: right index"
  (loop for i from l upto r
        for ai across a
        with swaps = 0
        do (loop for j = (1- i) then (1- j)
                 while (and (>= j 0) (not (funcall pred (aref a j) ai)))
                 do (setf (aref a (1+ j)) (aref a j))
                 (incf swaps)
                 finally (setf (aref a (1+ j)) ai))
        finally (return (values a swaps))))

(defun isort (a &optional (pred #'<=) (l 0) (r (length a)))
  "Destructive insertion sort a list. Also returns number of swaps a bubble sort would have done.
A: list
PRED: predicate
L: left index
R: right index"
    (loop for i from l upto r
          for ai in a
          with swaps = 0
          do (loop for j = (1- i) then (1- j)
                   while (and (>= j 0) (not (funcall pred (elt a j) ai)))
                   do (setf (elt a (1+ j)) (elt a j))
                   (incf swaps)
                   finally (setf (elt a (1+ j)) ai))
          finally (return (values a swaps))))

;; Array quicksort -- WORK IN PROGRESS

(defmacro swap (pl1 pl2)
  "Macro to swap two places"
  (let ((temp1-name (gensym)) ; don't clobber existing names
        (temp2-name (gensym)))
    `(let ((,temp1-name ,pl1)
           (,temp2-name ,pl2))
      (setf ,pl1 ,temp2-name)
      (setf ,pl2 ,temp1-name))))
(defun partition (a l r pred)
  (loop while (< l r)
    do
    (loop while (< l r)
      when (not (funcall pred (aref a l) (aref a r)))
      do (swap (aref a l) (aref a r))
      do (decf r))
    (loop while (< l r)
      when (funcall pred (aref a l) (aref a r))
      do (swap (aref a l) (aref a r))
      do (incf l)))
  l)
(defun qsorta (a &optional (pred #'<=) (l 0) (r (length a)))
  "Quicksort 1D array
A: 1D array to sort
PRED: predicate function to sort with
L: left index
R: right index"
  (when (< l r)
    (if (< (- r l) 50)
        (isorta a pred l r)
        (let ((i (partition a l r pred)))
          (qsorta a pred l i)
          (qsorta a pred (1+ i) r)))))
(defun partition-swaps (a pred l r)
  "Counts bubble-sort equivalent number of swaps while partitioning"
  (let ((swaps 0))
    (loop while (< l r)
      do
      (loop while (< l r)
        when (not (funcall pred (aref a l) (aref a r)))
        do (swap (aref a l) (aref a r)) (incf swaps (- r l))
        do (decf r))
      (loop while (< l r)
        when (funcall pred (aref a l) (aref a r))
        do (swap (aref a l) (aref a r))
        do (incf l)))
    (values l swaps)))
(defun qsorta-swaps (a &optional (pred #'<=) (l 0) (r (length a)))
  "Quicksort 1D array and count the number of bubble-sort swaps
A: 1d array
PRED: predicate function
L: left index
R: right index"
  (let ((swaps 0))
    (when (< l r)
      (multiple-value-bind (i swapsp) (partition-swaps a pred l r)
        (incf swaps swapsp)
        (incf swaps (qsorta-swaps a pred l i))
        (incf swaps (qsorta-swaps a pred (1+ i) r))))
    swaps))

(defun bubble-sort (l &optional (ls (length l)))
  "Non-destructive bubble-sort given a list of numbers and optional length"
  (let ((l (copy-list l)))
    (loop with num-swaps = 0
          for has-swapped = nil
          do
          (loop for i upto (- ls 2)
                do
                (when (> (elt l i) (elt l (1+ i)))
                  (swap (elt l i) (elt l (1+ i)))
                  (setq has-swapped t)
                  (incf num-swaps)))
          while has-swapped
          finally (return (values l num-swaps)))))

(defun print-hash-key-or-val (kv stream)
  (format stream (typecase kv
		   (keyword " :~a")
		   (string " \"~a\"")
		   (symbol " '~a")
		   (list " '~a")
		   (t " ~a")) kv))

(defun printhash (h &optional (stream t))
  (format stream "#<HASH-TABLE~{~a~a~}>"
	  (loop for k being the hash-keys in h using (hash-value v)
	     collect (print-hash-key-or-val k nil)
	     collect (print-hash-key-or-val v nil))))


(defmacro lethash (keys h &body body)
  "Let form binding hash table entries to let variables names"
  (let ((ht (gensym)))
    `(let ((,ht ,h))
       (let ,(loop for key in keys
		collect `(,key (gethash ,(make-keyword key) ,ht)))
	 ,@body))))

(defmacro with-keys (keys h &body body)
  "Make keys of hash table available to body for use & changable via setf"
  (let ((ht (gensym)))
    (loop for key in keys
       for newbody = (subst `(gethash ,(make-keyword key) ,ht) key body) 
       then (subst `(gethash ,(make-keyword key) ,ht) key newbody)
       finally (return `(let ((,ht ,h))
			  ,@newbody)))))

(defun linear-interpolation (ys xs x)
  "Linear interpolation: calculate y(x) at x given table of ys and xs. Also returns index of lookup table interval. Works from first x to less than last x."
  (let* ((i (position x xs :test #'>= :from-end t))
	 (x0 (elt xs i))
	 (x1 (elt xs (1+ i)))
	 (y0 (elt ys i))
	 (y1 (elt ys (1+ i))))
    (+ y0 (* (- y1 y0) (- x x0) (/ (- x1 x0))))))

(defun maptree (f tree)
  "Map a function on the leaves of a tree"
  (cond
    ((null tree) nil)
    ((atom tree) (funcall f tree))
    (t (cons (maptree f (car tree))
	     (maptree f (cdr tree))))))

(defmethod diff ((l list))
  "Return list of the 1st differences of given list: l(1)-l(0),...,l(n)-l(n-1)"
  (loop for i below (1- (length l))
     for li in l
     collect (- (elt l (1+ i)) li)))

(defmethod diff ((v vector))
  "Return vector of the 1st differences of given vector: v(1)-v(0),...,v(n)-v(n-1)"
  (let* ((n (length v))
	 (v2 (make-array (1- n))))
    (dotimes (i (1- n))
      (setf (aref v2 i) (- (aref v (1+ i)) (aref v i))))
    v2))

(defun slot-ref (obj slots)
  "Reference nested objects by a list of successive slot names. For example, (slot-ref o 'foo 'bar 'baz) should return (slot-value (slot-value (slot-value o 'foo) 'bar) 'baz)"
  (cond
    ((atom slots) (slot-value obj slots))
    ((null (cdr slots)) (slot-value obj (car slots)))
    (t (slot-ref (slot-value obj (first slots)) (rest slots)))))

(defun slot-ref-set (obj slots val)
  "Set nested object slot reference to new value"
  (cond
    ((atom slots) (setf (slot-value obj slots) val))
    ((null (cdr slots)) (setf (slot-value obj (car slots)) val))
    (t (slot-ref-set (slot-value obj (first slots)) (rest slots) val))))

(defsetf slot-ref slot-ref-set)

(defmacro bind-nested-slots (forms obj &body body)
  "For each form of (VAR SLOT1 SLOT2 ...) bind VAR to (NESTED-SLOT OBJ SLOT1 SLOT2 ...)"
  `(let ,(loop for form in forms
	    collect `(,(first form) (slot-ref ,obj ',(rest form))))
     ,@body))
