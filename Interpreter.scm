(load "simpleParser.scm")

(define front car)
(define middle cadr)
(define last caddr)

(define M_value
  (lambda (expression)
    (cond
      ((number? expression) expression)
      ((eq? '+ (front expression)) (+ (M_value (middle expression)) (M_value (last expression))))
      ((eq? '- (front expression)) (- (M_value (middle expression)) (M_value (last expression))))
      ((eq? '* (front expression)) (* (M_value (middle expression)) (M_value (last expression))))
      ((eq? '/ (front expression)) (quotient (M_value (middle expression)) (M_value (last expression))))
      ((eq? '% (front expression)) (remainder (M_value (middle expression)) (M_value (last expression))))
      (else (error 'unknown "unknown expression")))))
