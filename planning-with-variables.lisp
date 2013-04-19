;; choose and fail code 

;(load "/Users/Naveen/snark-20120808r022/snark-system")
(in-package :snark-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; SNARK SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *background-knowledge* nil)
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
  (mapcar #'snark::assert *background-knowledge*)
  (snark:run-time-limit   0.5)
  (snark:assert-supported t)
  (snark:assume-supported t)
  (snark:prove-supported t)
  (snark:use-hyperresolution t)
  (declare-variable '?number :sort 'integer)
  (declare-variable '?number1 :sort 'integer)
  (declare-variable '?number2 :sort 'integer)

  (snark:use-paramodulation t)
  (snark:allow-skolem-symbols-in-answers t)
  ;; (snark:declare-constant '0 :sort 'integer)
  ;; (snark:declare-constant '1 :sort 'integer)
  ;; (snark:declare-constant '2 :sort 'integer)
  ;; (snark:declare-constant '3 :sort 'integer)

  (snark::declare-code-for-numbers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; Backtracking Code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *paths* nil)
(defconstant failsym '@)
(defun failed? (ans) (equalp ans failsym))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; Util Code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun set= (s1 s2)
  (null (set-exclusive-or s1 s2)))

(defun skolem-sym? (x) 
  (if (and (symbolp x) 
	   (>= (length (symbol-name x)) 6)
	   (string= "SKOLEM" 
		    (subseq (symbol-name x) 0 6))) x))

(defun skolems (form)
  (if  (atom form)
       (skolem-sym? form)
       (reduce #'append (mapcar #'skolems (rest form)))))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; Action Class ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(defun action (name params preconds adds dels)
  (make-instance 'action 
		 :name name
		 :params params 
		 :preconds preconds 
		 :adds adds
		 :dels dels))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; Action Instance Class ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass action-instance ()
  ((action-schema :accessor action-schema :initarg :schema :type 'action)
   (action-params :accessor action-params :initarg :params)))

(defmethod print-object ((obj action-instance) out)
    (format out "[~s~a] " 
	    (action-name (action-schema obj)) 
	    (if (atom (action-params obj))
		"()"
		(action-params obj))))

(defun action-instance (schema params)
  (make-instance 'action-instance :schema schema :params params))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; State  Class ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defun state (&rest fluents)
  (apply #'make-instance (list 'state :sf fluents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; Plan  Class ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass plan ()
  ((actions :accessor plan-actions :initarg :actions)))

(defgeneric plan-cons(a p))
(defmethod plan-cons (a (p plan))
  (plan (cons a (plan-actions p))))

(defgeneric plan-first-action(p))
(defmethod plan-first-action ((p plan))
  (first (plan-actions p)))

(defgeneric plan-rest(p))
(defmethod plan-rest ((p plan))
  (plan (rest (plan-actions p))))

(defgeneric plan-reverse (p))
(defmethod plan-reverse ((p plan))
  (plan (reverse (plan-actions p))))

(defmethod print-object ((obj plan) out)
  (print-unreadable-object (obj out :type t)
    (mapcar (lambda (action-instance)
	      (print-object action-instance out))
	    (plan-actions obj))))

(defun plan (actions)
  (make-instance 'plan :actions actions))



;######################################################################
;;;;;;;;;;;;;;;;;;;;;;;; Core Logic  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; Resultant Backward  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun state-form= (x y)
  (setup-snark)
  (if (eq :PROOF-FOUND (prove `(iff ,x ,y )))
      t nil))

(defgeneric resultant-backward-vars (a s params))
(defmethod resultant-backward-vars ((a action) (s state) params)
  (apply #'state 
   (union
    (substitute-vars-list params (action-params a) (action-preconds a))
    (set-difference 
     (state-fluents s)
     (mapcar (lambda (f) 
	       (if (atom params)
		   f
		   `(exists ,(skolems f) ,f)))
	     (substitute-vars-list params (action-params a) (action-adds a))) 
     :test #'state-form=))))

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

(defun provable (p) 
  (if (eq :PROOF-FOUND (snark:prove p))
      t nil))

(defgeneric in-closure? (s1 s2))
(defmethod in-closure? ((s1 state) (s2 state))
  (setup-snark)
  (mapcar #'snark::assert (state-fluents s2))
  (every (lambda (x) x) (mapcar #'provable (state-fluents s1)))) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; Action-Executable-Backward ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Returns bindings for params if executable or T if no params.;;;;;;;
;;;; Returns nil if the action is not exectuable in the state.   ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod action-executable-backward?  ((a action) (s state)) 
  (let ((params (action-params a)))
    (setup-snark)
    (mapcar #'snark::assert (state-fluents s))
    (snark::assert      `(forall ,params
      (snark::implies (and ,@(action-adds a))
		       (action ,@params))))
     (let* ((result (prove `(action ,@(action-params a)) :answer `(params ,@params)))
	    (bindings (rest (snark:answer result))))
       (if (and (eq result :PROOF-FOUND) (null params))
	   t
	   (if (null bindings)
	       nil bindings)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; Planning Backward Code ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-plan-inner-backward-vars 
    (current actions final &optional (current-path ()))
  (if (reached? final current) 
      (plan ())
      (choose-bind an-action actions
	(let ((params (action-executable-backward? an-action final)))
	  (if (and (not (seen? an-action final current-path)) params)
	      (let 
		  ((reduced-plan
		    (make-plan-inner-backward-vars 
		     current actions (resultant-backward-vars an-action final params) 
		     (cons (list an-action final) current-path))))
		(if (failed? reduced-plan)
		    (fail)
		    (plan-cons 
		     (action-instance an-action params) 
		     reduced-plan)))
	      (fail))))))



(defun make-plan-vars (current actions final &optional (background nil))
  (let ((*background-knowledge* background))
      (plan-reverse (make-plan-inner-backward-vars current actions final))))


;(defun action-schema (a) (first a))
;(defun action-params (a) (second a))
(defmethod execute-action ((a action-instance) (s state))
  (state  'a))