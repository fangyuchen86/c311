(in-package "ACL2")

(include-book "ops")

(defun preserve-cond (Q A E)
  (cond
    ((equal A E) A)
    ('t (cond-c Q A E))))

(defun conjunction (es)
  (cond
    ((atom es) (quote-c 't))
    ((atom (cdr es)) (car es))
    ('t (cond-c (car es)
          (conjunction (cdr es))
          (quote-c 'nil)))))

(defun totality/meas (meas formals apps)
  (cond
    ((atom apps) '())
    ('t (cons (app-c '<
                (list2 (sub-e meas formals
                         (app.args (car apps)))
                       meas))
          (totality/meas meas formals (cdr apps))))))

(defun totality/cond (meas f formals apps e)
  (cond
    ((cond-p e)
     (preserve-cond (cond.Q e)
       (totality/cond meas f formals
         (list-union apps (expr-recs f (cond.Q e)))
         (cond.A e))
       (totality/cond meas f formals
         (list-union apps (expr-recs f (cond.Q e)))
         (cond.E e))))
    ('t (conjunction
          (totality/meas meas formals
            (list-union apps (expr-recs f e)))))))

(defun totality/claim (meas def)
  (cond-c (app-c 'natp (list1 meas))
    (totality/cond meas
      (defun.name def)
      (defun.formals def)
      '()
      (defun.body def))
    (quote-c 'nil)))

(defun implication (es e)
  (cond
    ((atom es) e)
    ('t (cond-c (car es)
          (implication (cdr es) e)
          (quote-c 't)))))

(defun induction/prems (vars claim apps)
  (cond
    ((atom apps) '())
    ('t (cons
	  (sub-e claim vars (app.args (car apps)))
          (induction/prems vars claim (cdr apps))))))

(defun induction/cond (vars claim f apps e)
  (cond
    ((cond-p e)
     (preserve-cond (cond.Q e)
       (induction/cond vars claim f
         (list-union apps (expr-recs f (cond.Q e)))
         (cond.A e))
       (induction/cond vars claim f
         (list-union apps (expr-recs f (cond.Q e)))
         (cond.E e))))
    ('t (implication
          (induction/prems vars claim
            (list-union apps (expr-recs f e)))
          claim))))

(defun induction/claim (vars claim def)
  (cond
    ((defun-p def)
     (cond
       ((arity-check vars (defun.formals def))
        (induction/cond vars claim (defun.name def)
          '()
          (sub-e (defun.body def)
            (defun.formals def)
            vars)))
       ('t claim)))
    ('t claim)))
