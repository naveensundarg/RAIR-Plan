;; choose and fail code 

;(load "/Users/Naveen/snark-20120808r022/snark-system")
(in-package :snark-user)
(defun snark-deverbose ()
  (snark:print-options-when-starting  nil)
  (snark:print-agenda-when-finished nil)
  (snark:print-clocks-when-finished nil)
  (snark:print-final-rows nil)
  (snark:print-symbol-table-warnings nil)
  (snark:print-summary-when-finished nil)
  (snark:print-row-answers nil)
  (snark:print-row-goals nil) 
  (snark:print-rows-when-derived nil)
  (snark:print-row-reasons nil)
  (snark:print-row-partitions nil)
  (snark:print-rows-prettily nil)
  (snark:print-rows :min 0 :max 0))

(defun setup-snark (&optional  (verbose nil))
  (snark:initialize :verbose  verbose)
  (if (not verbose) (snark-deverbose))
  (snark:run-time-limit  5)
  (snark:assert-supported t)
  (snark:assume-supported t)
  (snark:prove-supported t)
  (snark:use-hyperresolution t)
  (snark:use-paramodulation t)
  (snark:allow-skolem-symbols-in-answers t)
  (snark::declare-code-for-numbers))


(defparameter *paths* nil)
(defconstant failsym '@)
(defun failed? (ans) (equalp ans failsym))

(defun set= (s1 s2)
  (null (set-exclusive-or s1 s2)))

(defun skolem-sym? (x) (if (and (>= (length (symbol-name x)) 5) (string= "SKOLEM" (subseq (symbol-name x) 0 6))) x))
(defun skolems (form)
  (if  (atom form)
       (skolem-sym? form)
       (mapcar #'skolems (rest form))))
(defun substitute-var (value var form)
  (if (equalp var form)
      value
      (if  (atom form)
	   form
	   (cons (substitute-var value var (first form))
		 (substitute-var value var (rest form))))))
(defun substitute-vars-list (values vars form)
  (if (or (null values) (atom values))
      form
      (substitute-vars-list 
       (rest values) (rest vars) 
       (substitute-var (first values) (first vars) form))))
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
   (params :accessor action-params :initarg :params)
   (preconds :accessor action-preconds :initarg :preconds)
   (adds :accessor action-adds :initarg :adds)
   (dels :accessor action-dels :initarg :dels)))
(defmethod action= ((a1 action) (a2 action)) 
  (and (set= (action-preconds a1) (action-preconds a2))
       (set= (action-params a1) (action-params a2))
       (set= (action-adds a1) (action-adds a2))
       (set= (action-dels a1) (action-dels a2))))
(defmethod print-object ((obj action) out)
  (print-unreadable-object (obj out :type t)
    (format out "~s" (action-name obj))
    (format out "~a" (action-params obj))
    (format out " [~s] " (action-preconds obj))
    (format out "+[~s] " (action-adds obj))
    (format out "-[~s] " (action-dels obj))))

;;;; State class
(defclass state ()
  ((fluents :accessor state-fluents :initarg :sf)))

(defmethod state= ((s1 state) (s2 state)) 
  (or
   (set= (state-fluents s1) (state-fluents s2))
   (and (in-closure? s1 s2)
	(in-closure? s2 s2))))

(defmethod print-object ((obj state) out)
  (print-unreadable-object (obj out :type t)
    (format out "~s" (state-fluents obj))))

;;; Plan class
(defclass plan ()
  ((actions :accessor plan-actions :initarg :actions)))

(defgeneric plan-cons(a p))
(defmethod plan-cons (a (p plan))
  (plan (cons a (plan-actions p))))

(defgeneric plan-reverse (p))
(defmethod plan-reverse ((p plan))
  (plan (reverse (plan-actions p))))

(defmethod print-object ((obj plan) out)
  (print-unreadable-object (obj out :type t)
    (mapcar (lambda (a)
	      (format out "[~s(~a)] " (action-name (first a)) (if (atom (second a))
								""
								 (second a))))
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

(defun state-form= (x y)
  (setup-snark)
  (if (eq :PROOF-FOUND (prove `(iff ,x ,y )))
      t nil))

(defgeneric resultant-backward-vars (a s params))
(defmethod resultant-backward-vars ((a action) (s state) params)
  (make-instance 'state :sf (union
			     (substitute-vars-list params (action-params a) (action-preconds a))
			     (set-difference  (state-fluents s)
					      (mapcar (lambda (f) (if (atom params)
								      f
								      `(exists ,(skolems f) ,f)))
						      (substitute-vars-list params (action-params a) (action-adds a))) 
					      :test #'state-form=))))
(defparameter *past* 2)
(defgeneric seen? (a s current-path))
(defmethod seen? ((a action) (s state) current-path)
  (member (list a s) current-path :test 
	  (lambda (x y) 
	    (and  (action= (first x) (first y))
		  (state= (second x) (second y))))))

(defgeneric reached? (f c))
(defmethod reached? ((final state) (current state))
  (or (subsetp (state-fluents final) (state-fluents current) :test #'equalp)
      (in-closure? final current)))

(defun action (name params preconds adds dels)
  (make-instance 'action :name name :params params :preconds preconds :adds adds :dels dels))

(defun state (&rest fluents)
  (apply #'make-instance (list 'state :sf fluents)))

(defun plan (actions)
  (make-instance 'plan :actions actions))


;;;
(defun provable (p) 
  (if (eq :PROOF-FOUND (snark:prove p))
      t nil))

(defgeneric in-closure? (s1 s2))
(defmethod in-closure? ((s1 state) (s2 state))
  (setup-snark)
  (mapcar #'snark::assert (state-fluents s2))
  (every (lambda (x) x) (mapcar #'provable (state-fluents s1)))) 

;;;; Returns bindings for params if executable or T if no params.
;;;; Returns nil if the action is not exectuable in the state. 
(defmethod action-executable-backward?  ((a action) (s state)) 
  (let ((params (action-params a)))
    (setup-snark)
    (mapcar #'snark::assert (state-fluents s))
    (snark::assert `(forall ,params
			    (snark::implies (and ,@(action-adds a))
					    (action ,@params))))
    (let* ((result (prove `(action ,@(action-params a)) :answer `(params ,@params)))
	   (bindings (rest (snark:answer result))))
      (if (and (eq result :PROOF-FOUND) (null params))
	  t
	  (if (null bindings)
	      nil bindings)))))
;;;
(defun make-plan-inner-forward (current actions final &optional (current-path ()))
	     (if (reached? final current) 
		 (plan ())
		 (choose-bind an-action actions
		   (if (and (not (seen? an-action current current-path)) (action-allowed? an-action current))
		       (let ((reduced-plan
			       (make-plan-inner-forward (resultant an-action current) actions final
						(cons (list an-action current) current-path))))
			 (if (failed? reduced-plan)
			     (fail)
			     (plan-cons an-action reduced-plan)))
		       (fail)))))

(defun make-plan-inner-backward-vars (current actions final &optional (current-path ()))
  (if (reached? final current) 
      (plan ())
      (choose-bind an-action actions
	(let ((params (action-executable-backward? an-action final)))
	  (if (and (not (seen? an-action final current-path)) params)
	      (let ((reduced-plan
		     (make-plan-inner-backward-vars current actions (resultant-backward-vars an-action final params) 
					       (cons (list an-action final) current-path))))
		(if (failed? reduced-plan)
		    (fail)
		    (plan-cons (list an-action params) reduced-plan)))
	      (fail))))))



(defun make-plan-vars (current actions final &optional (backward? t))
  (if backward?
       (make-plan-inner-backward-vars current actions final)
      (make-plan-inner-forward current actions final)))


