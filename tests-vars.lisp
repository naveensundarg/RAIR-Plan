(defparameter *initial-state*
  (state '(forall (?x) (alive ?x)) '(hungry me)))

(defparameter *goal-state*
  (state '(exists (?x) (dead ?x)) '(not (hungry me))))

(defparameter *kill-action*
  (action 'kill '(?x) (list '(alive ?x)) (list '(dead ?x)) (list '(alive ?x))))

(defparameter *eat-action*
  (action 'eat '() (list ) (list '(not (hungry me))) (list '(hungry me))))

(make-plan-vars *initial-state* (list *kill-action* *eat-action* ) *goal-state*)


(defparameter *initial-state*
  (state '(bid 0)))

(defparameter *goal-state*
  (state '(bid 5)))

(defparameter *post-new-bid-action*
  (action 'post-new-bid '(?number) (list '(bid ($$sum -1 ?number))) (list '(bid ?number)) (list )))

(make-plan-vars *initial-state* (list *post-new-bid-action* ) *goal-state*)


(in-closure? (state '(bid ($$sum 1 -1))) (state '(bid 0)))