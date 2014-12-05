;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The code below makes proof-assistant.lisp work 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-syntax in-package
  (syntax-rules ()
    ((_ . args) (begin))))

(define-syntax include-book
  (syntax-rules ()
    ((_ file dir keyword) (begin))
    ((_ file) (load (string-append file ".lisp")))))

(define-syntax defun
  (syntax-rules ()
    ((_ name args decl ... body) (define name (lambda args body)))))

(define-syntax cond
  (syntax-rules ()
    ((_ (a b) ('t e))
     (if (equal? (fixup a) 'nil) e b))
    ((_ (a b) (c d) (e f) (g h) ...)
     (if (equal? (fixup a) 'nil)
       (cond (c d) (e f) (g h) ...)
       b))))

(define fixup
  (lambda (x)
    (if (pair? x)
      (cons (fixup (car x)) (fixup (cdr x)))
      (if (eq? x '()) 'nil x))))

(define scar car)
(define scdr cdr)

(define car
  (lambda (x)
    (if (pair? x) (scar x) 'nil)))

(define cdr
  (lambda (x)
    (if (pair? x) (scdr x) 'nil)))

(define s< <)

(define <
  (lambda (x y)
    (if (s< x y) 't 'nil)))

(define equal
  (lambda (x y)
    (if (equal? (fixup x) (fixup y)) 't 'nil)))

(define atom
  (lambda (x)
    (if (pair? x) 'nil 't)))

(define natp
  (lambda (x)
    (if (integer? x)
      (if (zero? x) 't (if (positive? x) 't 'nil))
      'nil)))

(define acl2-count
  (lambda (x)
    (cond
      ((atom x)
       (cond
         ((natp x) x)
         ('t 0)))
      ('t (+ 1 (+ (acl2-count (car x))
                  (acl2-count (cdr x))))))))

(define invert-cdr-nils
  (lambda (s)
    (if (pair? s)
      (cons (invert-cdr-nils (car s))
        (if (eq? (cdr s) 'nil)
          '()
          (invert-cdr-nils (cdr s))))
      s)))

(define-syntax check-expect
  (syntax-rules ()
    ((_ actual expected)
     (let ((a (fixup actual)) (e (fixup expected)))
       (if (equal a e)
           (begin
             (display "CORRECT")
             (newline)
             (pretty-print (invert-cdr-nils a))
             (newline))
           (begin
             (display "ACTUAL")
             (newline)
             (display a)
             (display "EXPECTED")
             (newline)
             (display e)
             (newline)
             (error 'check-expect
                    "ACTUAL result differs from EXPECTED result.")))))))

(load "proof-assistant.lisp")
