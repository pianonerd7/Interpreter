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


(define variable car)
(define value cdr)
;(M_declare '(x 12) '(()())) --> ((x) (12))
;(M_declare '(x) '(()())) --> ((x) (#f))
(define M_declare
  (lambda (expression state)
    (cond
      ((and (null? (value expression)) (eq? (searchVariable (variable expression) state) #f)) (addVariable (variable expression) #f state))
      ((eq? (searchVariable (variable expression) state) #f) (addVariable (variable expression) (car (value expression)) state))
      (else (error 'unknown "unknown expression")))))

(define M_assignment
  (lambda (expression state)
    (cond
      ((number? (value expression)) (addVariable (variable expression) (value expression) (removeVariable (variable expression) state)))
      ((list? (value expression)) (addVariable variable (M_value value) (removeVariable (variable expression) state)))
      (else (error 'unknown "unknown expression")))))

(define condition car)
(define body cdr)

(define M_state
  (lambda (expression state)
    (cond
      ((null? expression) state)
      ((eq? 'var (condition expression)) (M_declare (body expression) state))
      ((eq? '= (condition expression)) (M_assignment (body expression) state))
      ((eq? 'return (condition expression)) (M_return (body expression)))
      (else (error 'unknown "unknown expression")))))

(define variables car)
(define vals cadr)
(define restOfVariables cdar)
(define restOfValues cdadr)
(define 1stVariable caar)
(define 1stValue caadr)
;(searchVariable 'y '((x y z) (1 2 3))) --> 2
(define searchVariable
  (lambda (var state)
    (cond
      ((null? (car state)) #f)
      ((eq? var (1stVariable state)) (1stValue state))
      (else (searchVariable var (removeFirstPair state))))))

;(addVariable 'x 4 '((z)(1))) --> ((z x) (1 4))
(define addVariable
  (lambda (var value state)
    (cons (append (variables state) (cons var '()))(cons (append (vals state) (cons value '())) '()))))

;(removeVariable 'y '((x y z) (1 2 3))) --> ((x z) (1 3))
(define removeVariable
  (lambda (var state)
    (cond
      ((null? (variables state)) #f)
      ((eq? var (1stVariable state)) (cons (restOfVariables state) (cons (cdadr state) '())))
      (else (cons (cons (1stVariable state) (variables (removeVariable var (removeFirstPair state)))) (cons (cons (1stValue state) (vals (removeVariable var (removeFirstPair state)))) '()))))))

;(removeFirstPair '((x y z)(4 5 6))) --> ((y z) (5 6))
(define removeFirstPair
  (lambda (state)
    (if (null? (car state))
        state
        (cons (restOfVariables state)(cons (restOfValues state) '())))))

(define 1stExpression car)
(define restOfExpression cdr)
(define evaluate
  (lambda (expressions state)
    (if (null? (1stExpression expressions))
        state
        (evaluate (restOfExpression expressions) (M_state (1stExpression expressions) state)))))

(define interpret
  (lambda (filename)
    (evaluate (parser filename) '(()()))))

