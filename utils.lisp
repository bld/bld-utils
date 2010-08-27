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

(defun make-keyword (name)
  "Make a keyword with given name. Attempts to respect the current
readtable case."
  (intern (case (readtable-case *readtable*)
            (:upcase (string-upcase name))
            (:downcase (string-downcase name))
            (t name))
          :keyword))

(defun remove-nth (n seq)
  "Remove nth element from sequence"
  (remove-if (constantly t) seq :start n :count 1))

(defun make-hash (&rest keyvals)
  "Create a hash table given keys and values"
  (let ((h (make-hash-table)))
    (loop while keyvals
       for k = (pop keyvals)
       for v = (pop keyvals)
       when v do (setf (gethash k h) v))
    h))

(defmacro make-hash* (&rest keyvals)
  "Make a hash table given key/value pairs, allowing use of prior key/val pairs in later definitions"
  (loop while keyvals
     for k = (pop keyvals)
     for v = (pop keyvals)
     collect `(,k ,v) into letargs
     collect (make-keyword k) into objargs
     collect k into objargs
     finally (return
	       `(let* (,@letargs)
		  (make-hash ,@objargs)))))

(defmacro with-keys (keys hash &body body)
  `(let (,@(loop for key in keys
	      collect `(,key (gethash ,(make-keyword key) ,hash))))
     ,@body))

(defun maphash2 (fn ht)
  "Returns a hash-table with the results of the function of key & value as values"
  (let ((ht-out (make-hash-table)))
    (maphash #'(lambda (k v)
		 (setf (gethash k ht-out) (funcall fn k v)))
	     ht)
    ht-out))

(defmacro with-keys (keys hash-table &body body)
  "Similar to WITH-SLOTS for classes, this macro replaces references to the keys with GETHASH forms"
  (loop for key in keys
     for newbody = (subst `(gethash ,(make-keyword key) ,hash-table) key body)
     then (subst `(gethash ,(make-keyword key) ,hash-table) key newbody)
     finally (return `(progn ,@newbody))))
