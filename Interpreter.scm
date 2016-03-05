(load "simpleParser.scm")

;Anna He jxh604

(define condition car)
(define body cdr)
(define ifBody cadr)
(define ifTrueExec caddr)
(define elseExec cadddr)
;M_state determines where to send expressions for execution
(define M_state
  (lambda (expression state rtn break continue catch)
    (cond
      ((null? expression) state)
      ((eq? 'var (condition expression)) (M_declare (body expression) state))
      ((eq? '= (condition expression)) (M_assignment (body expression) state))
      ((eq? 'return (condition expression)) (M_return (body expression) state rtn))
      ((eq? 'if (condition expression)) (M_state_If expression state rtn break continue catch))
      ((eq? 'while (condition expression)) (M_state_While expression state rtn catch))
      ((eq? 'begin (condition expression)) (M_state_Begin (body expression) state rtn break continue catch))
      ((eq? 'continue (condition expression)) (M_state_Continue continue state))
      ((eq? 'break (condition expression)) (M_state_Break break state))
      ((eq? 'try (condition expression)) (M_state_Try (body expression) state catch))
      ((eq? 'throw (condition expression)) (M_state_Catch (car (body expression)) state catch rtn))
      (else (M_boolean(expression) state)))))

(define boolean_operator car)
(define leftCondition cadr)
(define rightCondition caddr)
;Determines the boolean value of an expression
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
;checks to see if an expression is an atom
(define (atom? x) (not (or (pair? x) (null? x))))
;Computes the value of an expression
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
(define isListNull cdr)
(define variable car)
; don't need cps, it simply adds the variable and value to the top most layer of the state
(define M_declare
  (lambda (expression state)
    (if (null? (isListNull expression))
        (addToFrontOfState (variable expression) 'null state)
        (assignValue-cps (variable expression) (M_value (value expression) state) (addToFrontOfState(variable expression) 'null state) (lambda (v) v)))))

;Calls on assignValue (which uses cps) to give a declared variable a value
(define M_assignment
  (lambda (expression state)
    (assignValue-cps (variable expression) (M_value (value expression) state) state (lambda (v) v))))

(define firstVar car)
(define firstVal car)
(define variables car)
(define vals cadr)
(define topLayerState car)
(define restLayerState cadr)
;Searches through the top most pair of variable and value of the state, if it isn't the variable we are looking for, we remove and continue searching until
;we find it, or until the list is null
(define assignValue-cps
  (lambda (var value state return)
    (cond
      ((null? state)(error 'unknown "using before declaring"))
      ((and (not (list? (variables (topLayerState state))))(eq? (firstVar (variables state)) var)) (return (addToFrontOfState var value (removeFirstPairFromState state))))
      ((not (list? (variables (topLayerState state)))) (return (assignValue-cps var value (removeFirstPairFromState state) (lambda (v) (return (addToFrontOfState (car (variables state)) (car (vals state)) v))))))
      ((null? (vals (topLayerState state))) (return (assignValue-cps var value (restLayerState state) (lambda (v) (cons (topLayerState state) (cons v '()))))))
      ((eq? (firstVar (variables (topLayerState state))) var) (return (addToFrontOfState var value (removeFirstPairFromState state))))
      (else (assignValue-cps var value (removeFirstPairFromState state) (lambda (v) (return (addToFrontOfState (car (variables (topLayerState state))) (car (vals (topLayerState state))) v))))))))

;Return the value. It call/cc back to evaluate and no other expression will be evaluated after this
(define M_return
  (lambda (expression state rtn)
    (rtn (M_boolean expression state))))

;Evaluates the condition of an if statement, if true, then it executes, exit otherwise
(define M_state_If
  (lambda (expression state rtn break continue catch)
    (if (M_boolean (ifBody expression) state)
        (M_state (ifTrueExec expression) state rtn break continue catch)
        (if (null? (cdddr expression))
            state
            (M_state (elseExec expression) state rtn break continue catch)))))

;Sends the condition, body and necessary arguments to whileLoop for call/cc
(define M_state_While
  (lambda (expression state rtn catch)
    (whileLoop (ifBody expression) (ifTrueExec expression) state rtn catch)))

;Loops until the condition becomes false, then exits, or break is called and exits, or continue is called
;and goes onto the next iteration
(define whileLoop
  (lambda (condition body state rtn catch)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body state)
                        (if (M_boolean condition state)
                            (loop condition body (M_state body state rtn break (lambda (v) v) catch))
                            state))))
         (loop condition body state))))))

(define removeTopLayer cadr)
;When a block of code is given, it creates a new layer and add to the top of the state. Then continues to send each expression M_state
(define M_state_Begin
  (lambda (expression state rtn break continue catch)
    (removeTopLayer
     (call/cc
      (lambda (continue)
        (executeBegin expression (addLayer initialState (consEmptyListToState state)) rtn break continue catch))))))

(define firstExpression car)
(define restExpression cdr)
;Helper function of M_state_begin, passes expression by expression to M_state
(define executeBegin
  (lambda (expression state rtn break continue catch)
    (if (null? expression)
        state
        (executeBegin (restExpression expression) (M_state (firstExpression expression) state rtn break continue catch) rtn break continue catch))))

;Continues to the next iteration of the loop
(define M_state_Continue
  (lambda (continue state)
    (continue state)))

;If a break is called inside the loop, it will break out of it. If not, it will display an error
(define M_state_Break
  (lambda (break state)
    (break (removeTopLayer state))))

(define tryBlock car)
(define 2ndExpression cadr)
(define 3rdExpression cddr)
(define catchBody
  (lambda (expression)
    (caddr (2ndExpression expression))))
;Try/Catch/Finally blocks are sent here, and are sent to respective functions based on whether they have try, catch, and or finally
(define M_state_Try
  (lambda (expression state catch)
    (cond
      ;case of try/catch and finally
      ((and (eq? (isEmpty (2ndExpression expression)) 'no) (eq? (isEmpty (3rdExpression expression)) 'no)) (M_state_TryCatchFinally expression state catch))
      ;case of try/catch
      ((eq? (isEmpty (2ndExpression expression)) 'no) (M_state_TryCatch expression state catch))
      ;case of try/finally
      ((eq? (isEmpty (3rdExpression expression)) 'no) (M_state_TryFinally expression (addLayer initialState (consEmptyListToState state)) catch))
      (else (error 'unknown "unknown expression")))))

;Loops through chunks of code and sends to M_state until a throw occurs and call/cc from M_state_Catch to here. Finally is then executed
(define M_state_TryCatchFinally
  (lambda (expression state catch)
    (M_state_Finally (cadar (3rdExpression expression))
                     (call/cc
                      (lambda (try)
                        (if (null? expression)
                            state
                            (M_state (cons 'begin (car expression)) state try (lambda (v) v) (lambda (v) (error "not in loop")) (caddr (2ndExpression expression)))))))))


;Loops through chunks of code and sends to M_state until a throw occurs and call/cc from M_state_Catch to here. Since it is only
;try and catch, the result will just be returned 
(define M_state_TryCatch
  (lambda (expression state catch)
    (call/cc
     (lambda (try)
       (if (null? expression)
           state
           (M_state (cons 'begin (car expression)) state try (lambda (v) v) (lambda (v) (error "not in loop")) (caddr (2ndExpression expression))))))))


;Loops through chunks of code and sends to M_state, finally is then executed
(define M_state_TryFinally
  (lambda (expression state catch)
    (M_state_Finally (cadar (3rdExpression expression))
                     (call/cc
                      (lambda (try)
                        (if (null? expression)
                            state
                            (M_state (cons 'begin (car expression)) state try (lambda (v) v) (lambda (v) (error "not in loop")) catch)))))))

;If a throw occurs, it is sent here. The thrown value and e is put ontop the top most layer of the state and evaulates the catch block. 
(define M_state_Catch
  (lambda (expression state catchExpressions break)
    (TryEvaluate catchExpressions (addToFrontOfState 'e expression (addLayer initialState (consEmptyListToState (removeTopLayer state)))) break)))

;Executes th finally block
(define M_state_Finally
  (lambda (expression state)
    (TryEvaluate expression state (lambda (v) v))))

;Loops through the catch block and executes until it is finished, then call/cc back to its respective try block
;Loops through the finally block and executes until it is finished
(define TryEvaluate
  (lambda (expression state break)
    (letrec ((loop (lambda (expression state)
                     (cond
                       ((null? expression) (break state))
                       ((and (null? (cdr expression)) (eq? (caar expression) 'throw)) (error "you don't have the right to throw"))
                       (else(loop (restExpression expression) (M_state (1stExpression expression) state break (lambda (v) (error "not in loop")) (lambda (v) (error "not in loop")) '())))))))
      (loop expression state))))

;Removes the first pair of var and value from the state
(define removeFirstPairFromState
  (lambda (state)
    (cond
      ((eq? state initialState) state)
      ((not (list? (variables (topLayerState state)))) (cons (cdr (variables state)) (cons (cdr (vals state)) '())))
      (else (cons (cons (cdr (variables (topLayerState state))) (cons (cdr (vals (topLayerState state))) '())) (cons (restLayerState state) '()))))))

;Adds a value and a pair to the top most layer of the state
(define addToFrontOfState
  (lambda (var value state)
    (cond 
      ((equal? state initialState) (cons (cons var (variables state)) (cons (cons value (vals state)) '())))
      ((not (list? (variables (topLayerState state)))) (cons (cons var (variables state)) (cons (cons value (vals state)) '())))
      (else (cons (cons (cons var (variables (topLayerState state))) (cons (cons value (vals (topLayerState state))) '())) (cons (restLayerState state) '()))))))

;Cons an empty list to the state
(define consEmptyListToState
  (lambda (state)
    (cons state '())))

;Uses CPS to search for a variable in a list. Used in M_value
(define searchVariable
  (lambda (var state return)
    (cond
      ((or (null? state)(null? (vals state))) (return 'empty))
      ((and (not (list? (variables (topLayerState state))))(eq? (firstVar (variables state)) var)) (return (firstVal (vals state))))
      ((not (list? (variables (topLayerState state)))) (return (searchVariable var (removeFirstPairFromState state) (lambda (v)(return v)))))
      ((null? (vals (topLayerState state))) (return (searchVariable var (restLayerState state) (lambda (v) (return v)))))
      ((eq? (firstVar (variables (topLayerState state))) var) (return (firstVal (vals (topLayerState state)))))
      (else (searchVariable var (removeFirstPairFromState state) (lambda (v) (return v)))))))

;Checks to see if a (possibly nested) list is empty or not
(define isEmpty
  (lambda (list)
    (cond
      ((null? list) 'yes)
      ((pair? list) (isEmpty (car list)))
      (else 'no))))

(define 1stExpression car)
(define restOfExpression cdr)
;Sends each chunk of code to M_state for computation. If a return statement is reached, it call/cc to here so that nothing else is executed
(define evaluate
  (lambda (expressions state)
    (call/cc
     (lambda (rtn)
       (letrec ((loop (lambda (expressions state)
                        (cond
                          ((null? expressions) (rtn state))
                          (else (loop (restOfExpression expressions) (M_state(1stExpression expressions) state rtn (lambda (v) (error "not in loop")) (lambda (v) (error "not in loop")) '())))))))
         (loop expressions state))))))

(define initialState '(()()))

;Parses a file and sends to the evaluate function
(define interpret
  (lambda (filename)
    (evaluate (parser filename) initialState)))