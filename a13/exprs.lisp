(in-package "ACL2")

(include-book "lists")

(defun cond-QAE (e)
  (list3 (cond.Q e) (cond.A e) (cond.E e)))

(defun QAE-cond (es)
  (cond-c (1st es) (2nd es) (3rd es)))

(defun vars-es (es)
  (cond
    ((atom es) '())
    ((atom (car es))
     (list-union (list1 (car es)) (vars-es (cdr es))))
    ((quote-p (car es)) (vars-es (cdr es)))
    ((cond-p (car es))
     (list-union (vars-es (cond-QAE (car es)))
                 (vars-es (cdr es))))
    ('t (list-union (vars-es (app.args (car es)))
                    (vars-es (cdr es))))))

(defun vars-e (e)
  (vars-es (list1 e)))

(defun exprs-recs (f es)
  (cond
    ((atom es) '())
    ((atom (car es)) (exprs-recs f (cdr es)))
    ((quote-p (car es)) (exprs-recs f (cdr es)))
    ((cond-p (car es))
     (list-union (exprs-recs f (cond-QAE (car es)))
                 (exprs-recs f (cdr es))))
    ((equal (app.name (car es)) f)
     (list-union (list1 (car es))
                 (list-union (exprs-recs f (app.args (car es)))
                             (exprs-recs f (cdr es)))))
    ('t (list-union (exprs-recs f (app.args (car es)))
                    (exprs-recs f (cdr es))))))

(defun expr-recs (f e)
  (exprs-recs f (list1 e)))

(defun sub-var (var vars args)
  (cond
    ((atom vars) var)
    ((equal (car vars) var) (car args))
    ('t (sub-var var (cdr vars) (cdr args)))))

(defun sub-es (es vars args)
  (cond
    ((atom es) '())
    ((atom (car es))
     (cons (sub-var (car es) vars args)
       (sub-es (cdr es) vars args)))
    ((quote-p (car es))
     (cons (car es)
       (sub-es (cdr es) vars args)))
    ((cond-p (car es))
     (cons
       (QAE-cond
         (sub-es (cond-QAE (car es)) vars args))
       (sub-es (cdr es) vars args)))
    ('t (cons
          (app-c (app.name (car es))
            (sub-es (app.args (car es)) vars args))
          (sub-es (cdr es) vars args)))))

(defun sub-e (e vars args)
  (1st (sub-es (list1 e) vars args)))
