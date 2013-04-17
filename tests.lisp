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
	 (action "run" '(not-hungry not-thirsty ) '(tired) '(not-hungry not-thirsty))
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
		  (list '(at spare ground) '(at flat ground))
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

(apply #'make-plan *test-tire-problem*)