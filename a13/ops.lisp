(in-package "ACL2")

(include-book "exprs")

(defun rator-value (op xs)
  (cond
    ((equal op 'equal) (equal (1st xs) (2nd xs)))
    ((equal op 'car) (car (1st xs)))
    ((equal op 'cdr) (cdr (1st xs)))
    ((equal op 'cons) (cons (1st xs) (2nd xs)))
    ((equal op 'atom) (atom (1st xs)))
    ((equal op 'acl2-count) (acl2-count (1st xs)))
    ((equal op 'natp) (natp (1st xs)))
    ((equal op '<) (< (1st xs) (2nd xs)))
    ((equal op '+) (+ (1st xs) (2nd xs)))
    ('t 'nil)))

(defun rand-values (args)
  (cond
    ((atom args) '())
    ('t (cons (quote.value (car args))
          (rand-values (cdr args))))))

(defun apply-op (op args)
  (quote-c (rator-value op (rand-values args))))

(defun rands-check (args)
  (cond
    ((atom args) 't)
    ('t (cond
          ((quote-p (car args))
           (rands-check (cdr args)))
          ('t 'nil)))))

(defun rator-arity-check (op args)
  (cond
    ((equal op 'equal) (arity-check '(x y) args))
    ((equal op 'car) (arity-check '(x) args))
    ((equal op 'cdr) (arity-check '(x) args))
    ((equal op 'cons) (arity-check '(x y) args))
    ((equal op 'atom) (arity-check '(x) args))
    ((equal op 'acl2-count) (arity-check '(x) args))
    ((equal op 'natp) (arity-check '(x) args))
    ((equal op '<) (arity-check '(x y) args))
    ((equal op '+) (arity-check '(x y) args))
    ('t 'nil)))

(defun op-check (op args)
  (cond
    ((rator-arity-check op args) (rands-check args))
    ('t 'nil)))

(defun op-p (op)
  (member-p op
    '(equal car cdr cons atom natp acl2-count < +)))
