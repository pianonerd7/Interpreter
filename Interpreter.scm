(load "simpleParser.scm")

;Anna He jxh604

(define condition car)
(define body cdr)
(define ifBody cadr)
(define ifTrueExec caddr)
(define elseExec cadddr)
(define M_state
  (lambda (expression state continue break)
    (cond
      ((null? expression) state)
      ((eq? 'var (condition expression)) (M_declare (body expression) state))
      ((eq? '= (condition expression)) (M_assignment (body expression) state))
      ((eq? 'return (condition expression)) (M_boolean (body expression) state))
      ((eq? 'if (condition expression)) (M_state_If expression state))
      ((eq? 'while (condition expression)) (M_state_While expression state))
      ((eq? 'begin (condition expression)) (M_state_Begin (body expression) state continue break))
      (else (M_boolean(expression) state)))))

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

(define addLayer cons)
(define restOfStates cadr)
(define value cadr)
(define variable car)
; don't need cps
(define M_declare
  (lambda (expression state)
    (if (null? (value expression))
        (addToFrontOfState (variable expression) 'null state)
        (addToFrontOfState (variable expression) (M_value (value expression) state) state))))

;assignValue uses cps
(define M_assignment
  (lambda (expression state)
    (assignValue-cps (variable expression) (M_value (value expression) state) state (lambda (v) v))))

(define firstVar car)
(define variables car)
(define vals cadr)
(define topLayerState car)
(define restLayerState cdr)
(define assignValue-cps
  (lambda (var value state return)
    (cond
      ((null? state)(error 'unknown "using before declaring"))
      ((null? (vals (topLayerState state))) (return (assignValue-cps var value (restLayerState state) (lambda (v) (cons (topLayerState state) v)))))
      ((eq? (firstVar (variables (topLayerState state))) var) (return (addToFrontOfState var value (removeFirstPairFromState state))))
      (else (assignValue-cps var value (removeFirstPairFromState state) (lambda (v) (return (addToFrontOfState (car (variables (topLayerState state))) (car (vals (topLayerState state))) v))))))))

(define removeFirstPairFromState
  (lambda (state)
    (if (eq? state initialState)
        state
        (cons (cons (cdr (variables (topLayerState state))) (cons (cdr (vals (topLayerState state))) '())) (restLayerState state)))))

;((a) (31160))
(define addToFrontOfState
  (lambda (var value state)
    (cond 
    ((eq? state initialState) (cons (cons var (variables state)) (cons (cons value (vals state)) '())))
    ((not (list? (variables (topLayerState state)))) (cons (cons var (variables state)) (cons (cons value (vals state)) '())))
    (else (cons (cons (cons var (variables (topLayerState state))) (cons (cons value (vals (topLayerState state))) '())) (restLayerState state))))))

(define M_return
  (lambda (expression state)
    (con
     ((number? (car expression)) (car expression))
     ((list? (car expression)) (M_return (cons (M_value (car expression) state) '()) state))
     (else (searchVariable (car expression) state)))))

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

(define consEmptyListToState
  (lambda (state)
    (cons state '())))

(define removeTopLayer cadr)
(define M_state_Begin
  (lambda (expression state continue break)
    (removeTopLayer (executeBegin expression (addLayer initialState (consEmptyListToState state)) continue break))))

(define executeBegin
  (lambda (expression state continue break)
    (if (null? expression)
        state
        (executeBegin (restExpression expression) (M_state (firstExpression expression) state continue break) continue break))))

(define firstExpression car)
(define restExpression cdr)
;helper function to remove the top most layer of state
(define removedTopLayer cdr)
;helper function to add another layer to the state
(define addLayer cons)


(define topLayer car)
(define restLayer cadr)
(define isAlreadyOneLayer cdadr)
(define searchVariable
  (lambda (var state)
    (cond
      ((or (null? state) (null? (car state))) 'empty)
      ((null? (isAlreadyOneLayer state)) (searchVariableScope var state))
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
      (else (searchVariableScope var (removeFirstPair state))))))

;(addVariable 'x 4 '((z)(1))) --> ((z x) (1 4))
(define addVariable
  (lambda (var value state)
    (cond
      ((or (eq? initialState state)(null? (isAlreadyOneLayer state))) (addVariableScope var value state))
      (else (addVariableScope var value (topLayer state))))))

(define addVariableScope
  (lambda (var value state)
    (cons (append (variables state) (cons var '()))(cons (append (vals state) (cons value '())) '()))))

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
      ((eq? (evaluate (restOfExpression expressions) (M_state (1stExpression expressions) state (lambda (v) v) (lambda (v) v))) #t) 'true)
      ((eq? (evaluate (restOfExpression expressions) (M_state (1stExpression expressions) state (lambda (v) v) (lambda (v) v))) #f) 'false)
      (else (evaluate(restOfExpression expressions) (M_state (1stExpression expressions) state (lambda (v) v) (lambda (v) v)))))))

(define initialState '(()()))

(define interpret
  (lambda (filename)
    (evaluate (parser filename) initialState)))