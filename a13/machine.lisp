(in-package "ACL2")

(include-book "focus")

(defun equality (focus a b)
  (cond
    ((equal focus a) b)
    ((equal focus b) a)
    ('t focus)))

(defun equality/equation (focus instantiated-conclusion)
  (cond
    ((equal-check instantiated-conclusion)
     (equality focus
       (1st (app.args instantiated-conclusion))
       (2nd (app.args instantiated-conclusion))))
    ('t focus)))

(defun equality/path (prems e path thm)
  (cond
    ((atom path)
     (equality/equation e (follow-prems prems thm)))
    ((step-check (car path) e)
     (rewrite-focus-at-step (car path) e
       (equality/path
	 (extend-prems (car path) e prems)
         (find-focus-at-step (car path) e)
         (cdr path)
         thm)))
    ('t e)))

(defun equality/def (claim path def name args)
  (cond
    ((op-check name args)
     (equality/path (empty-prems) claim path
       (app-c 'equal
         (list2 (app-c name args)
                (apply-op name args)))))
    ((defun-p def)
     (cond
       ((arity-check (defun.formals def) args)
	(equality/path (empty-prems) claim path
	  (sub-e
	    (app-c 'equal
              (list2 (app-c (defun.name def)
                       (defun.formals def))
                     (defun.body def)))
	    (defun.formals def)
	    args)))
       ('t claim)))
    ((dethm-p def)
     (cond
       ((arity-check (vars-e (dethm.body def)) args)
	(equality/path (empty-prems) claim path
          (sub-e (dethm.body def)
	    (vars-e (dethm.body def))
	    args)))
       ('t claim)))
    ('t claim)))

(defun rewriter/act (defs claim act)
  (cond
    ((equ-p act)
     (equality/def claim (equ.path act)
       (lookup (app.name (equ.app act)) defs)
       (app.name (equ.app act))
       (app.args (equ.app act))))
    ((ind-p act)
     (induction/claim (app.args (ind.app act)) claim
       (lookup (app.name (ind.app act)) defs)))
    ('t claim)))

(defun rewriter/act! (defs claim act)
  (print-frame defs claim act
    (rewriter/act defs claim act)))

(defun rewriter/acts (defs acts claim1 claim2)
  (cond
    ((equal claim1 claim2) claim2)
    ((atom acts) claim2)
    ('t (rewriter/acts defs (cdr acts) claim2
          (rewriter/act defs claim2 (car acts))))))

(defun rewriter/claim (defs claim acts)
  (rewriter/acts defs acts (cond-c 't claim 'nil) claim))
