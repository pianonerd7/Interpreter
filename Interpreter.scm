(load "functionParser.scm")
;Anna He jxh604
;Interpreter 3

(define condition car)
(define body cdr)
(define ifBody cadr)
(define ifTrueExec caddr)
(define elseExec cadddr)
;M_state determines where to send expressions for execution
(define M_state
  (lambda (expression state rtn break continue throw)
    (cond
      ((null? expression) state)
      ((eq? 'var (condition expression)) (M_declare (body expression) state))
      ((eq? '= (condition expression)) (M_assignment (body expression) state))
      ((eq? 'return (condition expression)) (M_return (body expression) state rtn))
      ((eq? 'if (condition expression)) (M_state_If expression state rtn break continue throw))
      ((eq? 'while (condition expression)) (M_state_While expression state rtn throw))
      ((eq? 'begin (condition expression)) (M_state_Begin (body expression) state rtn break continue throw))
      ((eq? 'continue (condition expression)) (M_state_Continue continue state))
      ((eq? 'break (condition expression)) (M_state_Break break state))
      ((eq? 'try (condition expression)) (M_state_tryCatchFinally expression state rtn break continue throw))
      ((eq? 'throw (condition expression)) (M_state_throw expression state throw))
      ((eq? 'function (condition expression)) (M_declare_Fxn expression state rtn break continue throw))
      (else (M_boolean(expression) state)))))

(define fxn_name cadr)
(define fxn_parameter caddr)
(define fxn_body cadddr)
; (function main(a1, a2) ((body stmt1) (body stmt2)))
(define M_declare_fxn
  (lambda (expression state rtn break continue throw)
    (addToFrontOfState (fxn_name expression) (list (fxn_parameter expression) (fxn_body expression) (lambda (state) (createEnvironment (fxn_name expression) state))))))
    
(define createEnvironment
  (lambda (fxnname state)
    ()))
      
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
  (lambda (expression state rtn break continue throw)
    (if (M_boolean (ifBody expression) state)
        (M_state (ifTrueExec expression) state rtn break continue throw)
        (if (null? (cdddr expression))
            state
            (M_state (elseExec expression) state rtn break continue throw)))))

;Sends the condition, body and necessary arguments to whileLoop for call/cc
(define M_state_While
  (lambda (expression state rtn throw)
    (whileLoop (ifBody expression) (ifTrueExec expression) state rtn throw)))

;Loops until the condition becomes false, then exits, or break is called and exits, or continue is called
;and goes onto the next iteration
(define whileLoop
  (lambda (condition body state rtn throw)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body state)
                        (if (M_boolean condition state)
                            (loop condition body (M_state body state rtn break (lambda (v) v) throw))
                            state))))
         (loop condition body state))))))

(define removeTopLayer cadr)
;When a block of code is given, it creates a new layer and add to the top of the state. Then continues to send each expression M_state
(define M_state_Begin
  (lambda (expression state rtn break continue throw)
    (removeTopLayer
     (call/cc
      (lambda (continue)
        (executeBegin expression (addLayer initialState (consEmptyListToState state)) rtn break continue throw))))))

(define firstExpression car)
(define restExpression cdr)
;Helper function of M_state_begin, passes expression by expression to M_state
(define executeBegin
  (lambda (expression state rtn break continue throw)
    (if (null? expression)
        state
        (executeBegin (restExpression expression) (M_state (firstExpression expression) state rtn break continue throw) rtn break continue throw))))

;Continues to the next iteration of the loop
(define M_state_Continue
  (lambda (continue state)
    (continue state)))

;If a break is called inside the loop, it will break out of it. If not, it will display an error
(define M_state_Break
  (lambda (break state)
    (break (removeTopLayer state))))

; (try body (catch (e) body) (finally body))
(define try-body cadr)
(define catch-body (lambda (t) (if (null? (cddr (caddr t)))  '()  (car (cddr (caddr t))))))
(define catch-err (lambda (t) (car (cadr (caddr t)))))
(define finally-stmt (lambda (t) (car (cdddr t))))
(define finally-body (lambda (t) (cadr (car (cdddr t)))))

(define M_state_throw
  (lambda (statement state throw)
    (throw (except-stmt statement) state)))
(define except-stmt cadr)

(define M_state_tryCatchFinally
  (lambda (statement state prog-return break continue throw)
    (call/cc
     (lambda (try-break)
       (letrec ((finally (lambda (s)
                    (cond
                      ((null? (finally-stmt statement)) s)
                      ((list? (car (finally-body statement))) (run-state (finally-body statement) s prog-return break continue throw))
                      (else (m-state (finally-body statement) s prog-return break continue throw)))))

                (try (lambda (s try-throw)
                       (if (list? (car (try-body statement)))
                           (finally (run-state (try-body statement) s prog-return break continue try-throw))
                           (finally (m-state (try-body statement) s prog-return break continue try-throw)))))

                (catch (lambda (e s)
                         (if (list? (car (catch-body statement)))
                             (finally (run-state (replace*-cps (catch-err statement) e (catch-body statement) (lambda (v) v)) s prog-return break continue throw))
                             (finally (m-state (replace*-cps (catch-err statement) e (catch-body statement) (lambda (v) v)) s prog-return break continue throw))))))
         (try state (lambda (e s) (try-break (catch e s)))) )))))

(define replace*-cps
  (lambda (old new l return)
    (cond
      ((null? l) (return l))
      ((pair? (car l)) (replace*-cps old new (cdr l) (lambda (v) (replace*-cps old new (car l) (lambda (v2) (return (cons v2 v)))))))
      ((eq? (car l) old) (replace*-cps old new (cdr l) (lambda (v) (return (cons new v)))))
      (else (replace*-cps old new (cdr l) (lambda (v) (return (cons (car l) v))))))))

(define run-state
    (lambda (pt state prog-return break continue throw)
      (if (null? pt)
          state
          (run-state (cdr pt)
                     (M_state (car pt) state prog-return break continue throw)
                     prog-return break continue throw))))

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
  (lambda (expressions state return)
    (call/cc
     (lambda (rtn)
       (letrec ((loop (lambda (expressions state)
                        (cond
                          ((null? expressions) (rtn state))
                          (else (loop (restOfExpression expressions) (M_state(1stExpression expressions) state rtn default_break default_continue default_throw)))))))
         (loop expressions state))
       (M_state '(main main) state return default_break default_continue default_throw)))))

(define initialState '(()()))

(define default_break
  (lambda (v)
    (error "break not in loop")))

(define default_continue
  (lambda (v)
    (error "continue not in loop")))

(define default_throw
  (lambda (statment state)
    (error "throw without catch")))

;Parses a file and sends to the evaluate function
(define interpret
  (lambda (filename)
    (call/cc
     (lambda (return)
       (evaluate (parser filename) initialState return)))))