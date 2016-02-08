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
      (else (number? expression) expression))))

(define M_declare
  (lambda (expression state)
    (cond
      ((null? expression) state)
      ((eq? (leftOperand expression) 'var)
       ))))

(define M_state
  (lambda (expression state)
    (cond
      ((eq? 'return (car (car expression))) (M_return (cdr (car expression))))
      ((eq? 
      )))))

(define searchVariable
  (lambda (var state)
    ((null? (car state)))
    (if (eq? (car (car state)) var)
        (car (cdr state))
        (searchVariable var 
    

    (else (error 'unknown "This variable doesn't exist"))))))

(define removeFirstPair
  (lambda (state)
    (cond
    ((null? (car state)) state)
    (else (cons (cdr (car state))(cons (cdr (car (cdr state))) '()))))))

;(define addVariable
  ;search
  ;add

(define evaluate
  (lambda (expressions state)
    (if (null? expressions)
        state
        (M_state expressions state))))

(define interpret
  (lambda (filename)
    (evaluate (parser filename) '(()()))))

