(load "simpleParser.scm")

;Anna He jxh604

(define operator car)
(define leftOperand cadr)
(define rightOperand caddr)
(define (atom? x) (not (or (pair? x) (null? x))))
(define M_value
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((and (atom? expression)(eq? (searchVariable expression state) 'empty)) (error 'unknown "using before declaring"))
      ((and (atom? expression)(eq? (searchVariable expression state) 'null)) (error 'unknown "using before assigning"))
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
    (con
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
      ((and (null? (value expression)) (eq? (searchVariable (variable expression) state) 'empty)) (addVariable (variable expression) 'null state))
      ((eq? (searchVariable (variable expression) state) 'empty) (addVariable (variable expression) (M_boolean (car (value expression)) state) state))
      ((or (eq? (searchVariable (variable expression) state) 'empty) (not (eq? (searchVariable (variable expression) state) 'empty))) (error 'unknown "redefining"))
      (else (error 'unknown "unknown expression")))))

(define assign_value cadr)
(define M_assignment
  (lambda (expression state)
    (cond
      ((eq? (searchVariable (variable expression) state) 'empty) (error 'unknown "using before declaring"))
      (else (addVariable (variable expression) (M_value (assign_value expression) state) (removeVariable (variable expression) state))))))

(define boolean_operator car)
(define leftCondition cadr)
(define rightCondition caddr)
(define M_boolean
  (lambda (expression state)
    (cond
      ((null? expression) state)
      ((eq? 'true expression) true)
      ((eq? 'false expression) false)
      ((eq? '#f expression) false)
      ((eq? '#t expression) true)
      ((atom? expression) (M_value expression state))
      ((null? (cdr expression)) (M_boolean (car expression) state))
      ((eq? '== (boolean_operator expression)) (eq? (M_boolean (leftCondition expression) state) (M_boolean (rightCondition expression) state)))
      ((eq? '!= (boolean_operator expression)) (not (eq? (M_boolean (leftCondition expression) state) (M_boolean (rightCondition expression) state))))
      ((eq? '< (boolean_operator expression)) (< (M_boolean (leftCondition expression) state) (M_boolean (rightCondition expression) state)))
      ((eq? '> (boolean_operator expression)) (> (M_boolean (leftCondition expression) state) (M_boolean (rightCondition expression) state)))
      ((eq? '<= (boolean_operator expression)) (<= (M_boolean (leftCondition expression) state) (M_boolean (rightCondition expression) state)))
      ((eq? '>= (boolean_operator expression)) (>= (M_boolean (leftCondition expression) state) (M_boolean (rightCondition expression) state)))
      ((eq? '&& (boolean_operator expression)) (and (eq? (M_boolean (leftCondition expression) state) true) (eq? (M_boolean (rightCondition expression) state) true)))
      ((eq? '|| (boolean_operator expression)) (or (eq? (M_boolean (leftCondition expression) state) true) (eq? (M_boolean (rightCondition expression) state) true)))
      ((eq? '! (boolean_operator expression)) (not (M_boolean (cdr expression) state)))
      (else (M_value expression state)))))

(define M_state_If
  (lambda (expression state)
    (if (M_boolean (ifBody expression) state)
        (M_state (ifTrueExec expression) state)
        (if (null? (cdddr expression))
            state
            (M_state (elseExec expression) state)))))

(define M_state_While
  (lambda (expression state)
    (if (M_boolean(ifBody expression) state)
        (M_state expression (M_state (ifTrueExec expression) state))
        state)))

(define M_state_Begin
  (lambda (expression state)
    (if (null? expression)
        (removeTopLayer state)
        (M_state_Begin '() (executeBegin expression (addLayer initialState state))))))

(define executeBegin
  (lambda (expression state)
    (if (null? expression)
        state
        (executeBegin (restExpression expression) (M_state (firstExpression expression) state)))))

(define firstExpression car)
(define restExpression cdr)
;helper function to remove the top most layer of state
(define removedTopLayer cdr)
;helper function to add another layer to the state
(define addLayer cons)

(define condition car)
(define body cdr)
(define ifBody cadr)
(define ifTrueExec caddr)
(define elseExec cadddr)
(define M_state
  (lambda (expression state)
    (cond
      ((null? expression) state)
      ((eq? 'var (condition expression)) (M_declare (body expression) state))
      ((eq? '= (condition expression)) (M_assignment (body expression) state))
      ((eq? 'return (condition expression)) (M_boolean (body expression) state))
      ((eq? 'if (condition expression)) (M_state_If expression state))
      ((eq? 'while (condition expression)) (M_state_While expression state))
      ((eq? 'begin (condition expression)) (M_state_Begin (body expression) state))
      (else (M_boolean(expression) state)))))

(define topLayer car)
(define restLayer cdr)
(define searchVariable
  (lambda (var state)
    (cond
      ((null? (car state)) 'empty)
      ((eq? (searchVariableScope var (topLayer state)) 'empty) (searchVariable var (restLayer state)))
      (else (searchVariableScope var (topLayer state))))))
                                      
(define variables car)
(define vals cadr)
(define restOfVariables cdar)
(define restOfValues cdadr)
(define 1stVariable caar)
(define 1stValue caadr)
;(searchVariable 'y '((x y z) (1 2 3))) --> 2
(define searchVariableScope
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
      ((null? (variables state)) state)
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
    (cond
      ((null? expressions) state)
      ((eq? (evaluate (restOfExpression expressions) (M_state (1stExpression expressions) state)) #t) 'true)
      ((eq? (evaluate (restOfExpression expressions) (M_state (1stExpression expressions) state)) #f) 'false)
      (else (evaluate(restOfExpression expressions) (M_state (1stExpression expressions) state))))))

(define initialState '(()()))

(define interpret
  (lambda (filename)
    (evaluate (parser filename) initialState)))