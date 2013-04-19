(in-package :snark-user)
(use-package :lisp-unit)

(defmethod sound-plan?? ((p plan) (initial state) (goal state))
  (or p (reached? goal initial) 
      (let ((next-state (execute-action (plan-first-action p) initial)))
	(if next-state
	    (sound-plan?? (plan-rest p) next-state goal)
	    nil))))

(defmethod sound-plan?? ((p t) (initial state) (goal state))
  (or (not (typep p plan)) (sound-plan p)))

(defmacro planner-test (name initial-state actions goal-state)
  `(define-test ,name
     (let ((initial-state ,initial-state )
	   (actions ,actions)
	   (goal-state ,goal-state))
       (assert-true 
	(sound-plan?? (make-plan-vars initial-state actions goal-state )
		      initial-state
		      goal-state)))))
;;;; test-1
(planner-test test-1
	      (state '(forall (?x) (alive ?x))) 
	      (list (action 'kill '(?x) (list '(alive ?x)) (list '(dead ?x)) (list '(alive ?x)))
		   )
	      (state '(exists (?x) (dead ?x))))

;;;; test-2
(planner-test test-2
	      (state '(forall (?x) (alive ?x)) '(hungry me))
	      (list (action 'kill '(?x) (list '(alive ?x)) (list '(dead ?x)) (list '(alive ?x)))
		    (action 'eat '() (list ) (list '(not (hungry me))) (list '(hungry me))))
	      (state '(exists (?x) (dead ?x)) '(not (hungry me))))

;;;; simple-bidding-single-agent
(planner-test simple-bidding-single-agent
	      (state '(bid 0))
	      (list (action 'post-new-bid '(?number) 
			    (list '(bid ($$sum -1 ?number)))
			    (list '(bid ?number)) 
			    (list '(bid ($$sum -1 ?number)))))
	      (state '(bid 5)))


;;;; murder-planning :D
(planner-test murder-planning
	      (state '(forall (?x) (implies (dead ?x) (inherits (son ?x))))
		     '(alive jack) '(= me (son jack))
		     '(forall (?x) (implies (inherits ?x) (rich ?x))))
	      (list   (action 'kill '(?x) (list '(alive ?x)) (list '(dead ?x)) (list '(alive ?x))))
	      (state '(rich me))
	      )

(run-tests)

(make-plan-vars 
	     (state 
	      '(alive jack) )
	     (list (action 'kill '(?x) (list '(alive ?x)) (list '(dead ?x)) (list '(alive ?x)))
		   (action 'break '(?x) (list '(unbroken ?x)) (list '(broken ?x)) (list '(unbroken ?x))))
	     (state '(rich me)) 
	     (list '(forall (?x) 
		     (iff (dead ?x) (inherits (son ?x))))
		   '(= me (son jack))
		   '(forall (?x) (implies (inherits ?x) (rich ?x)))))


(time (make-plan-vars 
	     (state '(alive jack) )
	     (list (action 'kill '(?x) (list '(alive ?x)) (list '(dead ?x)) (list '(alive ?x)))
		   (action 'break '(?x) (list '(unbroken ?x)) (list '(broken ?x)) (list '(unbroken ?x)))
		   (action 'fly-to-moon '(?x) (list '(rich ?x)) (list '(at moon ?x)) ()))
	     (state '(at moon me)) 
	     (list '(forall (?x) 
		     (iff (dead ?x) (inherits (son ?x))))
		   '(= me (son jack))
		   '(forall (?x) (iff (inherits ?x) (rich ?x))))))

(time (make-plan-vars 
	     (state '(alive jack) )
	     (list (action 'kill '(?x) (list '(alive ?x)) (list '(dead ?x)) (list '(alive ?x)))
		   (action 'break '(?x) (list '(unbroken ?x)) (list '(broken ?x)) (list '(unbroken ?x)))
		   (action 'fly-to-moon '(?x) (list '(rich ?x)) (list '(at moon ?x)) ())
		   (action 'run () () () ()))
	     (state '(at moon me)) 
	     (list '(forall (?x) 
		     (iff (dead ?x) (inherits (son ?x))))
		   '(= me (son jack))
		   '(forall (?x) (iff (inherits ?x) (rich ?x))))))

 (time (make-plan-vars 
	     (state '(alive jack) )
	     (list (action 'kill '(?x) (list '(alive ?x)) (list '(dead ?x)) (list '(alive ?x)))
		   (action 'break '(?x) (list '(unbroken ?x)) (list '(broken ?x)) (list '(unbroken ?x)))
		  (action 'fly-to-moon '(?x) (list '(rich ?x)) (list '(at moon ?x) '(not (at earth ?x))) ()) 
		   (action 'fire-cannon-at '(?x) (list '(not (at ?x me))) (list '(destroyed ?x)) ()))
	     (state '(destroyed earth)) 
	     (list '(forall (?x) 
		     (iff (dead ?x) (inherits (son ?x))))
		   '(= me (son jack)) '(not (= moon earth)) '(or (at earth me) (at moon me))
		   '(forall (?x) (iff (inherits ?x) (rich ?x))))))