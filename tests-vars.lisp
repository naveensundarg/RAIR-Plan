(defparameter *initial-state*
  (state '(forall (?x) (alive ?x)) '(hungry me)))

(defparameter *goal-state*
  (state '(exists (?x) (dead ?x)) '(not (hungry me))))

(defparameter *kill-action*
  (action 'kill '(?x) (list '(alive ?x)) (list '(dead ?x)) (list '(alive ?x))))

(defparameter *eat-action*
  (action 'eat '() (list ) (list '(not (hungry me))) (list '(hungry me))))

(make-plan-vars *initial-state* (list *kill-action* *eat-action* ) *goal-state*)
