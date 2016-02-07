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

(define M_return
  (lambda (expression)
    (cond
      ((null? expression) '())
      ((list? expression) (M_return (car expression)))
      ((number? expression) expression)
      (else '()))))

(define M_state
  (lambda (expression)
    (cond
      ((eq? 'return (car (car expression))) (M_return (cdr (car expression)))))))

(define evaluate
  (lambda (expressions state)
    (if (null? expressions)
        state
        (M_state expressions))))

(define interpret
  (lambda (filename)
    (evaluate (parser filename) '(()()))))

