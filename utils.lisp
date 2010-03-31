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
