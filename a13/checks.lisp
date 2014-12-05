(in-package "ACL2")

(include-book "magic")

(defun lookup (name defs)
  (cond
    ((atom defs) 'nil)
    ((equal (1st (untag (car defs))) name) (car defs))
    ('t (lookup name (cdr defs)))))

(defun app-check (def args)
  (cond
    ((defun-p def) (arity-check (defun.formals def) args))
    ('t 'nil)))

(defun exprs-check (defs vars es)
  (cond
    ((atom es) 't)
    ((atom (car es))
     (cond
       ((member-p (car es) vars)
        (exprs-check defs vars (cdr es)))
       ('t 'nil)))
    ((quote-p (car es)) (exprs-check defs vars (cdr es)))
    ((cond-p (car es))
     (cond
       ((exprs-check defs vars (cond-QAE (car es)))
        (exprs-check defs vars (cdr es)))
       ('t 'nil)))
    ((op-p (app.name (car es)))
     (cond
       ((rator-arity-check (app.name (car es))
          (app.args (car es)))
        (exprs-check defs vars (cdr es)))
       ('t 'nil)))
    ((app-check (lookup (app.name (car es)) defs)
       (app.args (car es)))
     (cond
       ((exprs-check defs vars (app.args (car es)))
        (exprs-check defs vars (cdr es)))
       ('t 'nil)))
    ('t 'nil)))

(defun expr-check (defs vars e)
  (exprs-check defs vars (list1 e)))

(defun var-check (x)
  (cond
    ((equal x 't) 'nil)
    ((equal x 'nil) 'nil)
    ((natp x) 'nil)
    ((atom x) 't)
    ('t 'nil)))

(defun vars-check (vars)
  (cond
    ((atom vars) 't)
    ((var-check (car vars))
     (cond
       ((member-p (car vars) (cdr vars)) 'nil)
       ('t (vars-check (cdr vars)))))
    ('t 'nil)))

(defun def-check (defs def name vars body)
  (cond
    ((equal (lookup name defs) 'nil)
     (cond
       ((vars-check vars)
        (expr-check (list-union defs (list1 def)) vars body))
       ('t 'nil)))
    ('t 'nil)))

(defun defun-check (defs def)
  (cond
    ((defun-p def)
     (def-check defs def
       (defun.name def)
       (defun.formals def)
       (defun.body def)))
    ('t 'nil)))

(defun dethm-check (defs def)
  (cond
    ((dethm-p def)
     (def-check defs def
       (dethm.name def)
       (vars-e (dethm.body def))
       (dethm.body def)))
    ('t 'nil)))
