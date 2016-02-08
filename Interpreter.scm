(load "simpleParser.scm")

(define operator car)
(define leftOperand cadr)
(define rightOperand caddr)

(define M_value
  (lambda (expression)
    (cond
      ((number? expression) expression)
      ((eq? '+ (operator expression)) (+ (M_value (leftOperand expression)) (M_value (rightOperand expression))))
      ((eq? '- (operator expression)) (- (M_value (leftOperand expression)) (M_value (rightOperand expression))))
      ((eq? '* (operator expression)) (* (M_value (leftOperand expression)) (M_value (rightOperand expression))))
      ((eq? '/ (operator expression)) (quotient (M_value (leftOperand expression)) (M_value (rightOperand expression))))
      ((eq? '% (operator expression)) (remainder (M_value (leftOperand expression)) (M_value (rightOperand expression))))
      (else (error 'unknown "unknown expression")))))

(define M_return
  (lambda (expression)
    (cond
      ((null? expression) '())
      ((list? expression) (M_return (M_value (car expression))))
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

