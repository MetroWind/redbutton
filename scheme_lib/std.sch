(define (= x . xs)
  (if (null? xs)
      #t
      (and (equal2 x (car xs))
           (apply = xs))))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (identity x) x)
(define (map f xs)
  (if (null? xs)
      '()
      (cons (f (car xs)) (map f (cdr xs)))))

(define (for-each xs f)
  (if (not (null? xs))
      (begin
        (f (car xs))
        (for-each (cdr xs) f))))

(define (list-tail xs n)
  (if (<= n 0) xs (list-tail (cdr xs) (- n 1))))

(define (length xs)
  (if (null? xs) 0 (+ (length (cdr xs)) 1)))
