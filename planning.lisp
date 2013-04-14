;; choose and fail code 
(defparameter *paths* nil)
(defconstant failsym '@)
(defun failed? (ans) (equalp ans failsym))

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
          (progn
            (if (cdr choices)
                (push #'(lambda () (cb fn (cdr choices)))
                        *paths*))
            (funcall fn (car choices)))
          (fail)))
(defun make-plan-inner (current actions final &optional (current-path ()))
	     (if (reached? final current) 
		 (plan ())
		 (choose-bind an-action actions
		   (if (and (not (seen? an-action current current-path)) (action-allowed? an-action current))
		       (let ((reduced-plan (let ((*paths* nil))
					     (make-plan-inner (resultant an-action current) actions final
							      (cons (list an-action current) current-path)))))
			 (if (failed? reduced-plan)
			     (fail)
			     (plan-cons an-action reduced-plan)))
		       (fail)))))
;;; planning code
(defun make-plan (current actions final)
  (let ((*paths* nil))
    (make-plan-inner current actions final)))

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

(defclass state ()
  ((fluents :accessor state-fluents :initarg :sf)))
(defmethod state= ((s1 state) (s2 state)) (set= (state-fluents s1) (state-fluents s2)))

(defmethod print-object ((obj state) out)
  (print-unreadable-object (obj out :type t)
    (format out "~s" (state-fluents obj))))

(defclass plan ()
  ((actions :accessor plan-actions :initarg :actions)))
(defgeneric plan-cons(a p))
(defmethod plan-cons ((a action) (p plan))
  (plan (cons a (plan-actions p))))

(defmethod print-object ((obj plan) out)
  (print-unreadable-object (obj out :type t)
    (mapcar (lambda (a)
		(format out "[~s] " (action-name a)))
	    (plan-actions obj))))

(defgeneric action-allowed? (a s))

(defmethod action-allowed? ((a action) (s state))
  (subsetp (action-preconds a) (state-fluents s) :test #'equalp))

(defgeneric resultant (a s))
(defmethod resultant ((a action) (s state))
  (make-instance 'state :sf (union (action-adds a) (set-difference (state-fluents s) (action-dels a) :test #'equalp)
				   :test #'equalp)))

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

;;;;;; Testing follows

(defparameter *drink-action* (action "drink-water"  '(thirsty) '(not-thirsty) '(thirsty)))
(defparameter *eat-action* (action "eat-food" '(hungry) '(not-hungry) '(hungry)))


(defparameter *test-1*
  (list (state '(not-thirsty))
	()
	(state '(not-thirsty))))
(reached? (state '(not-thirsty)) (state '(not-thirsty)))
(defparameter *test-2*
  (list (state 'thirsty)
	(list (action "drink-water" '(thirsty) '(not-thirsty) '(thirsty)))
	(state 'not-thirsty)))

(defparameter *test-3*
  (list (state 'thirsty )
	(list (action "drink-water" '(thirsty)  '(not-thirsty) '(thirsty))
	      (action "eat-food" '(hungry)  '(not-hungry) '(hungry)))
	(state 'not-thirsty )))

(defparameter *test-4*
  (list (state 'hungry )
	(list (action "drink-water" '(thirsty)  '(not-thirsty) '(thirsty))
	      (action "eat-food" '(hungry)  '(not-hungry) '(hungry)))
	(state 'not-hungry )))

(defparameter *test-5*
  (list (state 'hungry 'thirsty)
	(list (action "drink-water" '(thirsty)  '(not-thirsty) '(thirsty))
	      (action "eat-food" '(hungry)  '(not-hungry) '(hungry)))
	(state 'not-hungry 'not-hungry)))


(defparameter *test-6*
  (list (state 'hungry 'thirsty 'work-unfinished)
	(list 
	 (action "work" '(not-hungry not-thirsty work-unfinished) '(work-finished) '(work-unfinished))
	 (action "drink-water" '(thirsty)  '(not-thirsty) '(thirsty))
	 (action "eat-food" '(hungry)  '(not-hungry) '(hungry)))
	(state 'not-hungry 'not-hungry 'work-finished)))


(defparameter *test-7*
  (list (state 'hungry 'thirsty 'work-unfinished)
	(list 
	 (action "work" '(not-hungry not-thirsty work-unfinished) '(work-finished) '(work-unfinished))
	 (action "drink-water" '(thirsty)  '(not-thirsty) '(thirsty))
	 (action "drink-water" '(thirsty)  '(not-thirsty) '(thirsty))
	 (action "eat-food" '(hungry)  '(not-hungry) '(hungry)))
	(state 'work-finished)))

(defparameter *test-8*
  (list (state 'hungry 'thirsty 'work-unfinished 'work-not-submitted)
	(list 
	 (action "work" '(not-hungry not-thirsty work-unfinished) '(work-finished) '(work-unfinished))
	 (action "submit" '(work-finished) '(work-submitted) '(work-not-submitted))
	 (action "drink-water" '(thirsty)  '(not-thirsty) '(thirsty))
	 (action "drink-water" '(thirsty)  '(not-thirsty) '(thirsty))
	 (action "eat-food" '(hungry)  '(not-hungry) '(hungry)))
	(state  'work-submitted)))

(defparameter *test-9*
  (list (state 'hungry 'thirsty 'work-unfinished 'work-not-submitted 'poor)
	(list 
	 (action "sell-work" '(work-submitted) '(rich) '(poor))
	 (action "work" '(not-hungry not-thirsty work-unfinished) '(work-finished) '(work-unfinished))
	 (action "submit" '(work-finished) '(work-submitted) '(work-not-submitted))
	 (action "drink-water" '(thirsty)  '(not-thirsty) '(thirsty))
	 (action "eat-food" '(hungry)  '(not-hungry) '(hungry)))
	(state  'rich)))

(defparameter *test-9*
  (list (state 'hungry 'thirsty 'work-unfinished 'work-not-submitted 'poor)
	(list 
	 (action "sell-work" '(work-submitted) '(rich) '(poor))
	 (action "work" '(not-hungry not-thirsty work-unfinished) '(work-finished) '(work-unfinished))
	 (action "submit" '(work-finished) '(work-submitted) '(work-not-submitted))
	 (action "drink-water" '(thirsty)  '(not-thirsty) '(thirsty))
	 (action "eat-food" '(hungry)  '(not-hungry) '(hungry)))
	(state  'rich)))

(defparameter *test-10*
  (list (state 'hungry 'thirsty 'work-unfinished 'work-not-submitted 'poor)
	(list 
	 (action "sell-work" '(work-submitted) '(rich) '(poor))
	 (action "run" '(not-hungry not-thirsty ) '(tired) '(hungry thirsty))
	 (action "work" '(not-hungry not-thirsty work-unfinished) '(work-finished) '(work-unfinished))
	 (action "submit" '(work-finished) '(work-submitted) '(work-not-submitted))
	 (action "drink-water" '(thirsty)  '(not-thirsty) '(thirsty))
	 (action "eat-food" '(hungry)  '(not-hungry) '(hungry)))
	(state  'rich)))



; this should fail
(defparameter *test-goofoff*
  (list (state 'hungry 'thirsty 'work-unfinished 'work-not-submitted 'poor)
	(list 
	 (action "goofoff" () '(tired) ()))
	(state  'rich)))



(defparameter *test-tire-problem*
  (list (state '(at flat axle) '(at spare trunk))
	(list 
	  (action "leave overnight"
		      ()
		      ()
		      (list '(at spare ground)
			    '(at spare axle)
			    '(at spare trunk)
			    '(at flat ground)
			    '(at flat axle)))
	  (action "remove spare from trunk" 
		  (list '(at spare trunk))
		  (list  '(at spare ground))
		  (list '(at spare trunk)))
	  (action "remove flat from axle"
		  (list '(at flat axle))
		  (list '(at flat ground))
		  (list '(at flat axle)))
	  (action "put spare on axle"
		  (list '(at spare ground))
		  (list '(at spare axle))
		  (list '(at spare ground))))
	(state '(at spare axle))))

(defun range (a b) (loop for i from a to b collect i))
(defparameter *tests* 
  (let ((total-tests 10))
    (mapcar (lambda (n) (eval (read-from-string (concatenate 'string "*test-" (princ-to-string n) "*"))))
	    (range 1 total-tests))))

(defun run-tests ()
  (mapcar (lambda (test-case) (apply #'make-plan test-case)) *tests*))

(run-tests)
