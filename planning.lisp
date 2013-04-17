;; choose and fail code 
(defparameter *paths* nil)
(defconstant failsym '@)
(defun failed? (ans) (equalp ans failsym))
(defmacro choose (&rest choices)
    (if choices
	`(let ((*paths* nil))
	   (call/cc (lambda (cc) (progn
				   ,@(mapcar #'(lambda (c)
						 `(push #'(lambda () (funcall cc ,c)) *paths*))
					     (reverse (cdr choices)))
				   (funcall cc ,(car choices))))))
	'(fail)))

(defmacro let= ( &body all)
  `(with-call/cc (let ,@all)))

(defun set= (s1 s2)
  (null (set-exclusive-or s1 s2)))
(defun fail ()
    (if *paths*
           (funcall (pop *paths*))
           failsym))
 (defmacro choose-bind (var choices &body body)
    `(cb #'(lambda (,var) ,@body) ,choices))

(defun cb (fn choices)
  (if choices
      (let ((*paths* nil))  
	(progn
	  (if (cdr choices)
	      (push #'(lambda () (cb fn (cdr choices)))
		    *paths*))
	  (funcall fn (car choices))))
      (fail)))

;;; planning code

;;; Action class
(defclass action ()
  ((name :accessor action-name :initarg :name)
   (preconds :accessor action-preconds :initarg :preconds)
   (adds :accessor action-adds :initarg :adds)
   (dels :accessor action-dels :initarg :dels)))
(defmethod action= ((a1 action) (a2 action)) 
  (and (set= (action-preconds a1) (action-preconds a2))
       (set= (action-adds a1) (action-adds a2))
       (set= (action-dels a1) (action-dels a2))))
(defmethod print-object ((obj action) out)
  (print-unreadable-object (obj out :type t)
    (format out "~s " (action-name obj))
    (format out "[~s] " (action-preconds obj))
    (format out "+[~s] " (action-adds obj))
    (format out "-[~s] " (action-dels obj))))

;;;; State class
(defclass state ()
  ((fluents :accessor state-fluents :initarg :sf)))

(defmethod state= ((s1 state) (s2 state)) (set= (state-fluents s1) (state-fluents s2)))

(defmethod print-object ((obj state) out)
  (print-unreadable-object (obj out :type t)
    (format out "~s" (state-fluents obj))))

;;; Plan class
(defclass plan ()
  ((actions :accessor plan-actions :initarg :actions)))

(defgeneric plan-cons(a p))

(defmethod plan-cons ((a action) (p plan))
  (plan (cons a (plan-actions p))))


(defgeneric plan-reverse (p))

(defmethod plan-reverse ((p plan))
  (plan (reverse (plan-actions p))))

(defmethod print-object ((obj plan) out)
  (print-unreadable-object (obj out :type t)
    (mapcar (lambda (a)
		(format out "[~s] " (action-name a)))
	    (plan-actions obj))))

(defgeneric action-allowed? (a s))
(defmethod action-allowed? ((a action) (s state))
  (subsetp (action-preconds a) (state-fluents s) :test #'equalp))



(defgeneric action-useful? (a s))
(defmethod action-useful? ((a action) (s state))
  (and (subsetp (action-adds a) (state-fluents s)  :test #'equalp)
       (not (intersection (state-fluents s) (action-dels a) :test #'equalp))))

(defgeneric resultant-forward (a s))
(defmethod resultant-forward ((a action) (s state))
  (make-instance 'state :sf (union (action-adds a) (set-difference (state-fluents s) (action-dels a) :test #'equalp)
				   :test #'equalp)))
(defgeneric resultant-backward (a s))
(defmethod resultant-backward ((a action) (s state))
  (make-instance 'state :sf (union
			     (action-preconds a)
			     (set-difference (state-fluents s) (action-adds a) 
					     :test #'equalp))))
(defparameter *past* 2)
(defgeneric seen? (a s current-path))
(defmethod seen? ((a action) (s state) current-path)
  (member (list a s) current-path :test 
	  (lambda (x y) 
	    (and  (action= (first x) (first y))
		  (state= (second x) (second y))))))

(defgeneric reached? (f c))
(defmethod reached? ((final state) (current state))
  (subsetp (state-fluents final) (state-fluents current) :test #'equalp))

(defun action (name preconds adds dels)
  (make-instance 'action :name name :preconds preconds :adds adds :dels dels))

(defun state (&rest fluents)
  (apply #'make-instance (list 'state :sf fluents)))

(defun plan (actions)
  (make-instance 'plan :actions actions))

(defun make-plan-inner-forward (current actions final &optional (current-path ()))
	     (if (reached? final current) 
		 (plan ())
		 (choose-bind an-action actions
		   (if (and (not (seen? an-action current current-path)) (action-allowed? an-action current))
		       (let ((reduced-plan
			       (make-plan-inner-forward (resultant-forward an-action current) actions final
						(cons (list an-action current) current-path))))
			 (if (failed? reduced-plan)
			     (fail)
			     (plan-cons an-action reduced-plan)))
		       (fail)))))

(defun make-plan-inner-backward (current actions final &optional (current-path ()))
  (if (reached? final current) 
      (plan ())
      (choose-bind an-action actions
	(if (and (not (seen? an-action final current-path)) (action-useful? an-action final))
	    (let ((reduced-plan
		   (make-plan-inner-backward current actions (resultant-backward an-action final) 
				    (cons (list an-action final) current-path))))
	      (if (failed? reduced-plan)
		  (fail)
		   (plan-cons an-action reduced-plan)))
	    (fail)))))



(defun make-plan (current actions final &optional (backward? t))
  (if backward?
      (plan-reverse (make-plan-inner-backward current actions final))
      (make-plan-inner-forward current actions final)))


