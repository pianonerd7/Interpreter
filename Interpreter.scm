(load "simpleParser.scm")

;Anna He jxh604

(define condition car)
(define body cdr)
(define ifBody cadr)
(define ifTrueExec caddr)
(define elseExec cadddr)
(define M_state
  (lambda (expression state rtn)
    (cond
      ((null? expression) state)
      ((eq? 'var (condition expression)) (M_declare (body expression) state))
      ((eq? '= (condition expression)) (M_assignment (body expression) state))
      ((eq? 'return (condition expression)) (M_return (body expression) state rtn))
      ((eq? 'if (condition expression)) (M_state_If expression state rtn))
      ((eq? 'while (condition expression)) (M_state_While expression state rtn))
      ((eq? 'begin (condition expression)) (M_state_Begin (body expression) state rtn))
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
      ((and (atom? expression)(eq? (searchVariable expression state (lambda (v) v)) 'empty)) (error 'unknown "using before declaring"))
      ((and (atom? expression)(eq? (searchVariable expression state (lambda (v) v)) 'null)) (error 'unknown "using before assigning"))
      ((atom? expression) (searchVariable expression state (lambda (v) v)))
      ((eq? '+ (operator expression)) (+ (M_value (leftOperand expression) state) (M_value (rightOperand expression) state)))
      ((eq? '- (operator expression))
       (if (null? (cddr expression))
           (- 0 (M_value (leftOperand expression) state))
           (- (M_value (leftOperand expression) state) (M_value (rightOperand expression) state))))
      ((eq? '* (operator expression)) (* (M_value (leftOperand expression) state) (M_value (rightOperand expression) state)))
      ((eq? '/ (operator expression)) (quotient (M_value (leftOperand expression) state) (M_value (rightOperand expression) state)))
      ((eq? '% (operator expression)) (remainder (M_value (leftOperand expression) state) (M_value (rightOperand expression) state)))
      (else (error 'unknown "unknown expression")))))

;helper function to add another layer to the state
(define addLayer cons)
(define restOfStates cadr)
(define value cadr)
(define variable car)
; don't need cps
(define M_declare
  (lambda (expression state)
    (if (null? (value expression))
        (addToFrontOfState (variable expression) 'null state)
        (assignValue-cps (variable expression) (M_value (value expression) state) (addToFrontOfState(variable expression) 'null state) (lambda (v) v)))))

;assignValue uses cps
(define M_assignment
  (lambda (expression state)
    (assignValue-cps (variable expression) (M_value (value expression) state) state (lambda (v) v))))

(define firstVar car)
(define firstVal car)
(define variables car)
(define vals cadr)
(define topLayerState car)
(define restLayerState cadr)
(define assignValue-cps
  (lambda (var value state return)
    (cond
      ((null? state)(error 'unknown "using before declaring"))
      ((and (not (list? (variables (topLayerState state))))(eq? (firstVar (variables state)) var)) (return (addToFrontOfState var value (removeFirstPairFromState state))))
      ((not (list? (variables (topLayerState state)))) (return (assignValue-cps var value (removeFirstPairFromState state) (lambda (v) (return (addToFrontOfState (car (variables state)) (car (vals state)) v))))))
      ((null? (vals (topLayerState state))) (return (assignValue-cps var value (restLayerState state) (lambda (v) (cons (topLayerState state) (cons v '()))))))
      ((eq? (firstVar (variables (topLayerState state))) var) (return (addToFrontOfState var value (removeFirstPairFromState state))))
      (else (assignValue-cps var value (removeFirstPairFromState state) (lambda (v) (return (addToFrontOfState (car (variables (topLayerState state))) (car (vals (topLayerState state))) v))))))))

(define M_return
  (lambda (expression state rtn)
    (rtn (M_boolean expression state))))

(define M_state_While
  (lambda (expression state rtn)
    (whileLoop (ifBody expression) (ifTrueExec expression) state rtn)))

(define whileLoop
  (lambda (condition body state rtn)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body state)
                        (if (M_boolean condition state)
                            (loop condition body (M_state body state rtn))
                            state))))
         (loop condition body state))))))

(define M_state_If
  (lambda (expression state rtn)
    (if (M_boolean (ifBody expression) state)
        (M_state (ifTrueExec expression) state rtn)
        (if (null? (cdddr expression))
            state
            (M_state (elseExec expression) state rtn)))))

(define M_state_Continue
  (lambda (expression continue state)
    (continue state)))
           
(define removeFirstPairFromState
  (lambda (state)
    (cond
      ((eq? state initialState) state)
      ((not (list? (variables (topLayerState state)))) (cons (cdr (variables state)) (cons (cdr (vals state)) '())))
      (else (cons (cons (cdr (variables (topLayerState state))) (cons (cdr (vals (topLayerState state))) '())) (cons (restLayerState state) '()))))))

(define addToFrontOfState
  (lambda (var value state)
    (cond 
      ((equal? state initialState) (cons (cons var (variables state)) (cons (cons value (vals state)) '())))
      ((not (list? (variables (topLayerState state)))) (cons (cons var (variables state)) (cons (cons value (vals state)) '())))
      (else (cons (cons (cons var (variables (topLayerState state))) (cons (cons value (vals (topLayerState state))) '())) (cons (restLayerState state) '()))))))

(define consEmptyListToState
  (lambda (state)
    (cons state '())))

(define removeTopLayer cadr)
(define M_state_Begin
  (lambda (expression state rtn)
    (removeTopLayer (executeBegin expression (addLayer initialState (consEmptyListToState state)) rtn))))

(define firstExpression car)
(define restExpression cdr)
(define executeBegin
  (lambda (expression state rtn)
    (if (null? expression)
        state
        (executeBegin (restExpression expression) (M_state (firstExpression expression) state rtn) rtn))))

(define searchVariable
  (lambda (var state return)
    (cond
      ((or (null? state)(null? (vals state))) (return 'empty))
      ((and (not (list? (variables (topLayerState state))))(eq? (firstVar (variables state)) var)) (return (firstVal (vals state))))
      ((not (list? (variables (topLayerState state)))) (return (searchVariable var (removeFirstPairFromState state) (lambda (v)(return v)))))
      ((null? (vals (topLayerState state))) (return (searchVariable var (restLayerState state) (lambda (v) (return v)))))
      ((eq? (firstVar (variables (topLayerState state))) var) (return (firstVal (vals (topLayerState state)))))
      (else (searchVariable var (removeFirstPairFromState state) (lambda (v) (return v)))))))

(define 1stExpression car)
(define restOfExpression cdr)
(define evaluate
  (lambda (expressions state)
    (call/cc
     (lambda (rtn)
       (letrec ((loop (lambda (expressions state)
                        (cond
                          ((null? expressions) (rtn state))
                          (else (loop (restOfExpression expressions) (M_state(1stExpression expressions) state rtn)))))))
         (loop expressions state))))))

(define initialState '(()()))

(define interpret
  (lambda (filename)
    (evaluate (parser filename) initialState)))