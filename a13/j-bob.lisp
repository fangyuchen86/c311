(in-package "ACL2")

(include-book "machine")

(defun J-Bob/thm (defs def acts)
  (cond
    ((dethm-check defs def)
     (rewriter/claim defs
       (dethm.body def)
       acts))
    ('t (quote-c 'nil))))

(defun J-Bob/thm! (defs def acts)
  (loudly
   (begin (print-header def)
          (print-footer (J-Bob/thm defs def acts)))))

(defun J-Bob/fun (defs def meas acts)
  (cond
    ((defun-check defs def)
     (rewriter/claim defs
       (totality/claim meas def)
       acts))
    ('t (quote-c 'nil))))

(defun J-Bob/fun! (defs def meas acts)
  (loudly
   (begin (print-header def)
          (print-footer (J-Bob/fun defs def meas acts)))))

(defun J-Bob/thm+ (defs def acts)
  (cond
    ((equal (J-Bob/thm defs def acts) (quote-c 't))
     (list-union defs (list1 def)))
    ('t defs)))

(defun J-Bob/fun+ (defs def meas acts)
  (cond
    ((equal (J-Bob/fun defs def meas acts) (quote-c 't))
     (list-union defs (list1 def)))
    ('t defs)))
