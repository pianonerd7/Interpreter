(load "simpleParser.scm")

(define operator car)
(define leftOperand cadr)
(define rightOperand caddr)
(define (atom? x) (not (or (pair? x) (null? x))))
(define M_value
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((and (atom? expression)(eq? (searchVariable expression state) 'empty)) (error 'unknown "using before assigning"))
      ((and (atom? expression)(eq? (searchVariable expression state) #f)) (error 'unknown "using before assigning"))
      ((atom? expression) (searchVariable expression state))
      ((eq? '+ (operator expression)) (+ (M_value (leftOperand expression) state) (M_value (rightOperand expression) state)))
      ((eq? '- (operator expression))
       (if (null? (cddr expression))
           (- 0 (M_value (leftOperand expression) state))
           (- (M_value (leftOperand expression) state) (M_value (rightOperand expression) state))))
      ((eq? '* (operator expression)) (* (M_value (leftOperand expression) state) (M_value (rightOperand expression) state)))
      ((eq? '/ (operator expression)) (quotient (M_value (leftOperand expression) state) (M_value (rightOperand expression) state)))
      ((eq? '% (operator expression)) (remainder (M_value (leftOperand expression) state) (M_value (rightOperand expression) state)))
      (else (error 'unknown "unknown expression")))))

(define M_return
  (lambda (expression state)
    (cond
      ((number? (car expression)) (car expression))
      ((list? (car expression)) (M_return (cons (M_value (car expression) state) '()) state))
      (else (searchVariable (car expression) state)))))


(define variable car)
(define value cdr)
;(M_declare '(x) '(()())) --> ((x) (#f))
;(M_declare '(x 12) '(()())) --> ((x) (12))
(define M_declare
  (lambda (expression state)
    (cond
      ((and (null? (value expression)) (eq? (searchVariable (variable expression) state) 'empty)) (addVariable (variable expression) #f state))
      ((eq? (searchVariable (variable expression) state) 'empty) (addVariable (variable expression) (M_value (car (value expression)) state) state))
      (else (error 'unknown "unknown expression")))))

(define assign_value cadr)
(define M_assignment
  (lambda (expression state)
    (cond
      ((eq? (searchVariable (variable expression) state) 'empty) (error 'unknown "using before declaring"))
      ((number? (assign_value expression)) (addVariable (variable expression) (assign_value expression) (removeVariable (variable expression) state)))
      ;((eq? (searchVariable (assign_value expression) state) 'empty) (error 'unknown "using before declaring"))
      ;((eq? (searchVariable (assign_value expression) state) #f) (error 'unknown "using before assigning"))
      ((atom? (assign_value expression)) (addVariable (variable expression) (M_value (assign_value expression) state) (removeVariable (variable expression) state)))
      ((list? (assign_value expression)) (addVariable (variable expression) (M_value (assign_value expression) state) (removeVariable (variable expression) state)))
      (else (error 'unknown "unknown expression")))))

(define boolean_operator caar)
(define leftCondition cadar)
(define rightCondition caddar)
(define M_boolean
  (lambda (expression state)
    (cond
      ((null? expression) state)
      ((atom? expression) (M_value expression state))
      ((eq? '== (boolean_operator expression)) (eq? (M_boolean (leftCondition expression) state) (M_boolean (rightCondition expression) state)))
      ((eq? '!= (boolean_operator expression)) (not (eq? (M_boolean (leftCondition expression) state) (M_boolean (rightCondition expression) state))))
      ((eq? '< (boolean_operator expression)) (< (M_boolean (leftCondition expression) state) (M_boolean (rightCondition expression) state)))
      ((eq? '> (boolean_operator expression)) (> (M_boolean (leftCondition expression) state) (M_boolean (rightCondition expression) state)))
      ((eq? '<= (boolean_operator expression)) (<= (M_boolean (leftCondition expression) state) (M_boolean (rightCondition expression) state)))
      ((eq? '>= (boolean_operator expression)) (>= (M_boolean (leftCondition expression) state) (M_boolean (rightCondition expression) state)))
      ((eq? '&& (boolean_operator expression)) (and (eq? (M_boolean (leftCondition expression) state) true) (eq? (M_boolean (rightCondition expression) state) true)))
      ((eq? '|| (boolean_operator expression)) (or (eq? (M_boolean (leftCondition expression) state) true) (eq? (M_boolean (rightCondition expression) state) true)))
      ((eq? '! (boolean_operator expression)) (not (M_boolean (cdr expression) state)))
      (else (error 'unknown "unknown expression")))))

(define condition car)
(define body cdr)
(define ifTrueExec caddr)
(define elseExec cadddr)
(define M_state
  (lambda (expression state)
    (cond
      ((null? expression) state)
      ((eq? 'var (condition expression)) (M_declare (body expression) state))
      ((eq? '= (condition expression)) (M_assignment (body expression) state))
      ((eq? 'return (condition expression)) (M_return (body expression) state))
      ((eq? 'if (condition expression))
       (if (M_boolean (body expression) state)
           (M_state (ifTrueExec expression) state)
           (if (null? (cdddr expression))
               state
               (M_state (elseExec expression) state))))
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
      ((null? (car state)) 'empty)
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
    (if (null? expressions)
        state
        (evaluate (restOfExpression expressions) (M_state (1stExpression expressions) state)))))

(define interpret
  (lambda (filename)
    (evaluate (parser filename) '(()()))))

