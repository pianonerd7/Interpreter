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

;(M_declare '(x 12) '(()())) --> ((x) (12))
;(M_declare '(x) '(()())) --> ((x) (#f))
(define M_declare
  (lambda (expression state)
    (cond
      ((and (null? (cdr expression)) (eq? (searchVariable (car expression) state) #f)) (addVariable (car expression) #f state))
      ((eq? (searchVariable (car expression) state) #f) (addVariable (car expression) (car (cdr expression)) state))
      (else (error 'unknown "unknown expression")))))

(define M_assignment
  (lambda (expression state)
    (cond
      ((null? expression) state))))


(define M_state
  (lambda (expression state)
    (cond
      ((null? expression) state)
      ((eq? 'var (car (car expression))) (M_declare (cdr (car expression)) state))
      ((eq? '= (car (car expression))) (M_assignment (cdr expression) state))
      ((eq? 'return (car (car expression))) (M_return (cdr (car expression))))
      (else (error 'unknown "unknown expression")))))

;(searchVariable 'y '((x y z) (1 2 3))) --> 2
(define searchVariable
  (lambda (var state)
    (cond
      ((null? (car state)) #f)
      ((eq? var (car (car state))) (car (car (cdr state))))
      (else (searchVariable var (removeFirstPair state))))))

;(addVariable 'x 4 '((z)(1))) --> ((z x) (1 4))
(define addVariable
  (lambda (var value state)
    (cons (append (car state) (cons var '()))(cons (append (car (cdr state)) (cons value '())) '()))))

;(removeVariable 'y '((x y z) (1 2 3))) --> ((x z) (1 3))
(define removeVariable
  (lambda (var state)
    (cond
      ((null? (car state)) #f)
      ((eq? var (car (car state))) (cons (cdr (car state)) (cons (cdr (car (cdr state))) '())))
      (else (cons (cons (car (car state)) (car (removeVariable var (removeFirstPair state)))) (cons (cons (car (car (cdr state))) (car (cdr (removeVariable var (removeFirstPair state))))) '()))))))

;(removeFirstPair '((x y z)(4 5 6))) --> ((y z) (5 6))
(define removeFirstPair
  (lambda (state)
    (if (null? (car state))
        state
        (cons (cdr (car state))(cons (cdr (car (cdr state))) '())))))

(define evaluate
  (lambda (expressions state)
    (if (null? expressions)
        state
        (M_state expressions state))))

(define interpret
  (lambda (filename)
    (evaluate (parser filename) '(()()))))

