(load "classParser.scm")

;Anna He jxh604
;Leah Platt lrp39		
;Haley Eisenshtadt hne3
;Interpreter 4

(define condition car)
(define body cdr)
(define ifBody cadr)
(define ifTrueExec caddr)
(define elseExec cadddr)
;M_state determines where to send expressions for execution
(define M_state
  (lambda (expression state classState)
    (cond
      ((null? expression) state)
      ((eq? 'var (condition expression)) (M_declare (body expression) state classState))
      ((eq? '= (condition expression)) (M_assignment (body expression) state classState))
      ((eq? 'return (condition expression)) (M_return (body expression) state classState))
      ((eq? 'if (condition expression)) (M_state_If expression state classState))
      ((eq? 'while (condition expression)) (M_state_While expression state classState))
      ((eq? 'begin (condition expression)) (M_state_Begin (body expression) state classState))
      ((eq? 'continue (condition expression)) (M_state_Continue (getcontinue classState) state))
      ((eq? 'break (condition expression)) (M_state_Break (getbreak classState) state))
      ((eq? 'try (condition expression)) (M_state_tryCatchFinally expression state classState))
      ((eq? 'throw (condition expression)) (M_state_throw expression state (getthrow classState)))
      ((eq? 'function (condition expression)) (M_declare_fxn expression state classState))
      ((eq? 'funcall (condition expression)) (begin (M_value expression state classState) state))
      (else (M_boolean expression state classState)))))

(define fxn_body cadr)
(define fxn_environment caddr)
(define fxn_argVal cddr)
;when a function is called M_state_fxncall gets the closure for the function, and binds the formal parameter and the
;arguments, create a new layer and append to the front of the state with the parameters in it. Then it calls run-state
;to process the body of the function
(define M_state_fxncall
  (lambda (expression state classState)
    (call/cc
     (lambda (return)
       (run-state (cadr (car (lookupdotfxn (cadr expression) state classState)))
                  (cons (paramprocessor (car (car (lookupdotfxn (cadr expression) state classState))) (cddr expression) state classState)
                        ((caddr (car (lookupdotfxn (cadr expression) state classState))) state))
                  (setclass
                   (if (eq? 'null (cadr (lookupdotfxn (cadr expression) state classState)))
                       ((cadddr (car (lookupdotfxn (cadr expression) state classState))) state)
                       (caddr (lookupdotfxn (cadr expression) state classState)))
                   (setreturn return (setbreak default_break (setinstance (cadr (lookupdotfxn (cadr expression) state classState))
                                                                          (setcurrentclass ((cadddr (car (lookupdotfxn (cadr expression) state classState))) state)
                                                                                           (setcontinue default_continue classState)))))))))))

(define lookupvariable
  (lambda (variablename state class instance)
    (cond
      ((eq? 'this variablename) (box instance))
      ((not (eq? 'empty (searchInStateAllLayer variablename state))) (searchInStateAllLayer variablename state))
      ((not (eq? 'empty (searchInStateAllLayer variablename (list (car (getclassinstance class)) (caddr instance)))))
       (searchInStateAllLayer variablename (list (car (getclassinstance class)) (caddr instance))))
      (else (error "does not exist")))))

(define getclassinstancepair
  (lambda (variablename)
    (if (eq? (car variablename) 'class)
        (list 'null variablename)
        (list variablename (cadr variablename)))))
    
(define findfxnfromstate
  (lambda (variablename class state)
    (if (not (eq? 'empty (searchInStateAllLayer variable state)))
        (unbox (searchInStateAllLayer variable state))
        (unbox (searchreverseorder variablename (getmethods class))))))

(define searchreverseorder
  (lambda (variable state)
    (getatindex (traverse variable state) state)))

(define getatindex
  (lambda (index state)
    (cond
      ((null? state) 'empty)
      ((eq? 1 index) (caadr state))
      (else (getatindex (- index 1) (list (cdar state) (cdadr state)))))))
       
(define traverse
  (lambda (var l)
    (cond
      ((null? l) 'empty)
      ((eq? (caar l) var) (getindex (cadr l) 0))
      (else (traverse var (list (cdar l) (cdadr l)))))))

(define getindex
  (lambda (list count)
    (if (null? list)
        count
        (getindex (cdr list) (+ count 1)))))

(define lookupdotlefthandclass
  (lambda (classname state classState)
      (getclassinstancepair (unbox (lookupvariable classname state (getclass classState) (getinstance classState))))))

(define lookupdotfxn
  (lambda (expression state classState)
    (cons (findfxnfromstate (caddr expression) (cadr (lookupdotlefthandclass (cadr expression) state classState)) initialState) (lookupdotlefthandclass (cadr expression) state classState))))

(define lookupdotoperatorvariable
  (lambda (expression state classState)
    (lookupvariable (caddr expression) initialState (cadr (lookupdotlefthandclass (cadr expression) state classState)) (car (lookupdotlefthandclass (cadr expression) state classState)))))

(define paramprocessor
  (lambda (varlist vallist state classState)
    (if (and (null? varlist) (null? vallist))
        initialState
        (addToFrontOfState (car varlist) (M_value (car vallist) state classState) (paramprocessor (cdr varlist) (cdr vallist) state classState)))))
     
;bind parameters with value to the front of the state
(define fxncall_newstate
  (lambda (var val state)
    (cond
      ((and (null? var) (null? val)) state)
      (else (fxncall_newstate (cdr var) (cdr val) (addToFrontOfState (car var) (car val) state))))))

;checks to make sure that the formal parameter and the actual parameter passed in from the expression is the same length
(define checkParameterLength
  (lambda (var val)
    (cond
      ((and (null? var) (null? val)) 'pass)
      ((or (null? var) (null? val)) (error "Formal parameter and actual parameter are not the same length!"))
      (else (checkParameterLength (cdr var) (cdr val))))))

(define fxn_name cadr)
(define fxn_parameter caddr)
(define fxn_body cadddr)
; (function main(a1, a2) ((body stmt1) (body stmt2)))
;declares a function by creating a closure and placing into the state
(define M_declare_fxn
  (lambda (expression state classState)
    (addToFrontOfState (fxn_name expression) (list (fxn_parameter expression) (fxn_body expression) (lambda (state) (createEnvironment (fxn_name expression) state))) state)))

;this is a function that will be placed in the closure for each function that will get called when the
;function gets called. This function creates the environment for the function
(define createEnvironment
  (lambda (fxnname state)
    (cond
      ((null? state) 'empty)
      ((eq? 'empty (searchInStateTopLayer fxnname (getFirstLayerFromState state))) (createEnvironment fxnname (removeFirstLayerFromState state)))
      (else state))))

(define boolean_operator car)
(define leftCondition cadr)
(define rightCondition caddr)
;Determines the boolean value of an expression
(define M_boolean
  (lambda (expression state classState)
    (cond
      ((null? expression) state)
      ((eq? 'true expression) true)
      ((eq? 'false expression) false)
      ((eq? '#f expression) false)
      ((eq? '#t expression) true)
      ((atom? expression) (M_value expression state classState))
      ((null? (cdr expression)) (M_boolean (car expression) state classState))
      ((eq? '== (boolean_operator expression)) (eq? (M_boolean (leftCondition expression) state classState) (M_boolean (rightCondition expression) state classState)))
      ((eq? '!= (boolean_operator expression)) (not (eq? (M_boolean (leftCondition expression) state classState) (M_boolean (rightCondition expression) state classState))))
      ((eq? '< (boolean_operator expression)) (< (M_boolean (leftCondition expression) state classState) (M_boolean (rightCondition expression) state classState)))
      ((eq? '> (boolean_operator expression)) (> (M_boolean (leftCondition expression) state classState) (M_boolean (rightCondition expression) state classState)))
      ((eq? '<= (boolean_operator expression)) (<= (M_boolean (leftCondition expression) state classState) (M_boolean (rightCondition expression) state classState)))
      ((eq? '>= (boolean_operator expression)) (>= (M_boolean (leftCondition expression) state classState) (M_boolean (rightCondition expression) state classState)))
      ((eq? '&& (boolean_operator expression)) (and (eq? (M_boolean (leftCondition expression) state classState) true) (eq? (M_boolean (rightCondition expression) state classState) true)))
      ((eq? '|| (boolean_operator expression)) (or (eq? (M_boolean (leftCondition expression) state classState) true) (eq? (M_boolean (rightCondition expression) state classState) true)))
      ((eq? '! (boolean_operator expression)) (not (M_boolean (cdr expression) state classState)))
      (else (M_value expression state classState)))))

(define operator car)
(define leftOperand cadr)
(define rightOperand caddr)
;checks to see if an expression is an atom
(define (atom? x) (not (or (pair? x) (null? x))))
;Computes the value of an expression
(define M_value
  (lambda (expression state classState)
    (cond
      ((number? expression) expression)
      ((and (atom? expression)(eq? (searchVariable expression state (lambda (v) v)) 'empty)) (error 'unknown "using before declaring"))
      ((and (atom? expression)(eq? (searchVariable expression state (lambda (v) v)) 'null)) (error 'unknown "using before assigning"))
      ((atom? expression) (searchVariable expression state (lambda (v) v)))
      ((eq? '+ (operator expression)) (+ (M_value (leftOperand expression) state classState) (M_value (rightOperand expression) state classState)))
      ((eq? '- (operator expression))
       (if (null? (cddr expression))
           (- 0 (M_value (leftOperand expression) state classState))
           (- (M_value (leftOperand expression) state classState) (M_value (rightOperand expression) state classState))))
      ((eq? '* (operator expression)) (* (M_value (leftOperand expression) state classState) (M_value (rightOperand expression) state classState)))
      ((eq? '/ (operator expression)) (quotient (M_value (leftOperand expression) state classState) (M_value (rightOperand expression) state classState)))
      ((eq? '% (operator expression)) (remainder (M_value (leftOperand expression) state classState) (M_value (rightOperand expression) state classState)))
      ((eq? 'funcall (condition expression)) (M_state_fxncall expression state classState))
      ((eq? 'new (operator expression)) (M_state_newOperator (body expression) state classState))
      ((eq? 'dot (operator expression)) (M_state_dotOperator expression state classState))
      (else (error 'unknown "unknown expression")))))

(define M_state_newOperator
  (lambda (expression state classState)
    (list 'inst (cadr (list 'inst (unbox (searchInStateAllLayer (car expression) state)) '())) (cadr (getclassinstance (unbox (searchInStateAllLayer (car expression) state)))))))

(define M_state_dotOperator
  (lambda (expression state classState)
    (unbox (lookupdottedexpression expression state classState))))

(define lookupdottedexpression
  (lambda (expression state classState)
    (lookupvariable (caddr expression) initialState (cadr (lookupdotlefthandclass (cadr expression) state classState)) (car (lookupdotlefthandclass (cadr expression) state classState)))))

;helper function to add another layer to the state
(define addLayer cons)
(define restOfStates cadr)
(define value cadr)
(define isListNull cdr)
(define variable car)
; don't need cps, it simply adds the variable and value to the top most layer of the state
(define M_declare
  (lambda (expression state classState)
    (if (null? (isListNull expression))
        (addToFrontOfState (variable expression) 'null state)
        (assignValue (variable expression) (M_value (value expression) state classState) (addToFrontOfState(variable expression) 'null state)))))

;Calls on assignValue (which uses cps) to give a declared variable a value
(define M_assignment
  (lambda (expression state classState)
    (set-box! (lookupdotoperatorvariable (car expression) state classState) (M_value (cadr expression) state classState)) (M_value (cadr expression) state classState)))

(define firstVar car)
(define firstVal car)
(define variables car)
(define vals cadr)
(define topLayerState car)
(define restLayerState cadr)
;Searches through the top most pair of variable and value of the state, if it isn't the variable we are looking for, we remove and continue searching until
;we find it, or until the list is null
(define assignValue
  (lambda (var value state)
    (if (eq? (searchInStateAllLayer var state) 'empty)
        (error "You are not in scope to access this variable!")
        (begin (set-box! (searchInStateAllLayer var state) value) state))))

;Return the value. It call/cc back to evaluate and no other expression will be evaluated after this
(define M_return
  (lambda (expression state classState)
    ((getreturn classState) (M_boolean expression state classState))))

;Evaluates the condition of an if statement, if true, then it executes, exit otherwise
(define M_state_If
  (lambda (expression state classState)
    (if (M_boolean (ifBody expression) state classState)
        (M_state (ifTrueExec expression) state classState)
        (if (null? (cdddr expression))
            state
            (M_state (elseExec expression) state classState)))))

;Sends the condition, body and necessary arguments to whileLoop for call/cc
(define M_state_While
  (lambda (expression state classState)
    (whileLoop (ifBody expression) (ifTrueExec expression) state classState)))

;Loops until the condition becomes false, then exits, or break is called and exits, or continue is called
;and goes onto the next iteration
(define whileLoop
  (lambda (condition body state classState)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body state)
                        (if (M_boolean condition state classState)
                            (loop condition body (M_state body state (setcontinue (lambda (v) v) classState)))
                            state))))
         (loop condition body state))))))

(define removeTopLayer cadr)
;When a block of code is given, it creates a new layer and add to the top of the state. Then continues to send each expression M_state
(define M_state_Begin
  (lambda (expression state classState)
    (removeTopLayer
     (call/cc
      (lambda (continue)
        (executeBegin expression (addLayer initialState (consEmptyListToState state)) classState))))))

(define firstExpression car)
(define restExpression cdr)
;Helper function of M_state_begin, passes expression by expression to M_state
(define executeBegin
  (lambda (expression state classState)
    (if (null? expression)
        state
        (executeBegin (restExpression expression) (M_state (firstExpression expression) state classState) classState))))

;Continues to the next iteration of the loop
(define M_state_Continue
  (lambda (continue state)
    (continue state)))

;If a break is called inside the loop, it will break out of it. If not, it will display an error
(define M_state_Break
  (lambda (break state)
    (break (removeTopLayer state))))

; (try body (catch (e) body) (finally body))
(define try_body cadr)
(define catch_body (lambda (t) (if (null? (cddr (caddr t)))  '()  (car (cddr (caddr t))))))
(define catch_err (lambda (t) (car (cadr (caddr t)))))
(define finally_stmt (lambda (t) (car (cdddr t))))
(define finally_body (lambda (t) (cadr (car (cdddr t)))))

; gets called when an expression throws
(define M_state_throw
  (lambda (statement state throw)
    (throw (except-stmt statement) state)))
(define except-stmt cadr)

(define M_state_tryCatchFinally
  (lambda (statement state classState)
    (call/cc
     (lambda (try-break)
       (letrec ((finally (lambda (s)
                           (cond
                             ((null? (finally_stmt statement)) s)
                             ((list? (car (finally_body statement))) (run-state (finally_body statement) s classState))
                             (else (m-state (finally_body statement) s classState)))))
                
                (try (lambda (s try-throw)
                       (if (list? (car (try_body statement)))
                           (finally (run-state (try_body statement) s classState))
                           (finally (m-state (try_body statement) s classState)))))
                
                (catch (lambda (e s)
                         (if (list? (car (catch_body statement)))
                             (finally (run-state (replace*-cps (catch_err statement) e (catch_body statement) (lambda (v) v)) s classState))
                             (finally (m-state (replace*-cps (catch_err statement) e (catch_body statement) (lambda (v) v)) s classState))))))
         (try state (lambda (e s) (try-break (catch e s)))) )))))

(define replace*-cps
  (lambda (old new l return)
    (cond
      ((null? l) (return l))
      ((pair? (car l)) (replace*-cps old new (cdr l) (lambda (v) (replace*-cps old new (car l) (lambda (v2) (return (cons v2 v)))))))
      ((eq? (car l) old) (replace*-cps old new (cdr l) (lambda (v) (return (cons new v)))))
      (else (replace*-cps old new (cdr l) (lambda (v) (return (cons (car l) v))))))))

;passes into this function a body and it will execute each expression 
(define run-state
  (lambda (expression state classState)
    (if (null? expression)
        state
        (run-state (cdr expression)
                   (M_state (car expression) state classState)
                   classState))))

;Uses CPS to search for a variable in the state
(define searchVariable
  (lambda (var state return)
    (cond
      ((or (null? state)(null? (vals state))) (return 'empty))
      ((and (not (list? (variables (topLayerState state))))(eq? (firstVar (variables state)) var)) (return (unbox (firstVal (vals state)))))
      ((not (list? (variables (topLayerState state)))) (return (searchVariable var (removeFirstPairFromState state) (lambda (v)(return v)))))
      ((null? (vals (topLayerState state))) (return (searchVariable var (restLayerState state) (lambda (v) (return v)))))
      ((eq? (firstVar (variables (topLayerState state))) var) (return (unbox (firstVal (vals (topLayerState state))))))
      (else (searchVariable var (removeFirstPairFromState state) (lambda (v) (return v)))))))

(define topVariable caar)
(define topValue caadr)
;searches only in the top layer of the state, returns empty if the name doesn't exist
(define searchInStateTopLayer
  (lambda (name state)
    (cond
      ((or (null? state) (equal? initialState state)) 'empty)
      ((eq? (car (variables state)) name) (car (vals state)))
      (else (searchInStateTopLayer name (removeFirstPairFromState state))))))

(define ifValuesExists caar)
;returns the box of a name found in the state if it exists, empty other wise
(define searchInStateAllLayer
  (lambda (name state)
    (cond
      ((null? state) 'empty)
      ((eq? initialState state) 'empty)
      ((and (not(list? (ifValuesExists state))) (eq? 'empty (searchInStateTopLayer name state))) 'empty)
      ((not (list? (ifValuesExists state))) (searchInStateTopLayer name state))
      ((eq? 'empty (searchInStateTopLayer name (topLayerState state))) (searchInStateAllLayer name (removeFirstLayerFromState state)))
      (else (searchInStateTopLayer name (topLayerState state))))))

;returns the state with the first layer removed
(define removeFirstLayerFromState
  (lambda (state)
    (cond
      ((equal? state initialState) state)
      (else (restLayerState state)))))

;returns the first layer of the state
(define getFirstLayerFromState
  (lambda (state)
    (cond
      ((equal? state initialState) state)
      ((list? (ifValuesExists state)) (car state))
      (else state))))

;Removes the first pair of var and value from the state
(define removeFirstPairFromState
  (lambda (state)
    (cond
      ((null? (variables (topLayerState state))) (cadr state))
      ((eq? state initialState) state)
      ((not (list? (variables (topLayerState state)))) (cons (cdr (variables state)) (cons (cdr (vals state)) '())))
      (else (cons (cons (cdr (variables (topLayerState state))) (cons (cdr (vals (topLayerState state))) '())) (cons (restLayerState state) '()))))))

;Adds a value and a pair to the top most layer of the state
(define addToFrontOfState
  (lambda (var value state)
    (cond
      ((equal? state initialState) (cons (cons var (variables state)) (cons (cons (box value) (vals state)) '())))
      ((not (list? (variables (topLayerState state)))) (cons (cons var (variables state)) (cons (cons (box value) (vals state)) '())))
      (else (cons (cons (cons var (variables (topLayerState state))) (cons (cons (box value) (vals (topLayerState state))) '())) (cons (restLayerState state) '()))))))

;Cons an empty list to the state
(define consEmptyListToState
  (lambda (state)
    (cons state '())))

;Checks to see if a (possibly nested) list is empty or not
(define isEmpty
  (lambda (list)
    (cond
      ((null? list) 'yes)
      ((pair? list) (isEmpty (car list)))
      (else 'no))))

(define initialState '(()()))
(define initialClassState
  (lambda (rtn)
    (list rtn default_break default_continue default_throw '() '() '())))

;default break
(define default_break
  (lambda (v)
    (error "break not in loop")))

;default continue
(define default_continue
  (lambda (v)
    (error "continue not in loop")))

;default throw
(define default_throw
  (lambda (statment state)
    (error "throw without catch")))

(define getreturn car)
(define getbreak cadr)
(define getcontinue caddr)
(define getthrow cadddr)

(define getclass
  (lambda (classState)
    (cadr (cdddr classState))))

(define getinstance
  (lambda (classState)
    (caddr (cdddr classState))))

(define getcurrentclass
  (lambda (classState)
    (cadddr (cdddr classState))))

(define setreturn
  (lambda (newReturn classState)
    (list newReturn
     (getbreak classState) (getcontinue classState) (getthrow classState)
     (getclass classState) (getinstance classState) (getcurrentclass classState))))

(define setbreak
  (lambda (newBreak classState)
    (list (getreturn classState)
     newBreak (getcontinue classState) (getthrow classState)
     (getclass classState) (getinstance classState) (getcurrentclass classState))))

(define setcontinue
  (lambda (newContinue classState)
    (list (getreturn classState)
     (getbreak classState) newContinue (getthrow classState)
     (getclass classState) (getinstance classState) (getcurrentclass classState))))

(define setthrow
  (lambda (newThrow classState)
    (list (getreturn classState)
     (getbreak classState) (getcontinue classState) newThrow
     (getclass classState) (getinstance classState) (getcurrentclass classState))))

(define setclass
  (lambda (newClass classState)
    (list (getreturn classState)
     (getbreak classState) (getcontinue classState) (getthrow classState)
     newClass (getinstance classState) (getcurrentclass classState))))

(define setinstance
  (lambda (newInstance classState)
    (list (getreturn classState)
     (getbreak classState) (getcontinue classState) (getthrow classState)
     (getclass classState) newInstance (getcurrentclass classState))))

(define setcurrentclass
  (lambda (newCurrentClass classState)
    (list (getreturn classState)
     (getbreak classState) (getcontinue classState) (getthrow classState)
     (getclass classState) (getinstance classState) newCurrentClass)))

;a class consists of 1) name 2) who it extends 3) body
(define classname cadr)
(define extends caddr)
(define declarebody cadddr)
(define class_declaration
  (lambda (expression state classState)
    (if (null? expression)
        state
        (addToFrontOfState (classname expression)
                           (classProcessor (declarebody expression) state (setcurrentclass (createNewClass (classname expression) (extends expression)) (setclass (createNewClass (classname expression) (extends expression)) classState)))
                            state))))

(define classProcessor
  (lambda (expression state classState)
    (cond
      ((null? expression) (getclass classState))
      (else (classProcessor (restOfExpression expression) state (setcurrentclass (MClass (1stExpression expression) state classState) (setclass (MClass (1stExpression expression) state classState) classState)))))))

(define MClass
  (lambda (expression state classState)
    (cond
      ((null? expression) classState)
      ((eq? 'var (condition expression)) (MClass_declare (cdr expression) state classState))
      ((eq? 'function (condition expression)) (MClass_declarestaticfxn expression state classState))
      ((eq? 'static-function (condition expression)) (MClass_declarestaticfxn expression state classState))
      (else (getclass classState)))))

(define MClass_declare
  (lambda (expression state classState)
    (if (null? (isListNull expression))
        (setclassinstance (addToFrontOfState (variable expression) 'null (getclassinstance (getclass classState))) (getclass classState))
        (setclassinstance (assignValue (variable expression)
                                           (M_value (value expression) state classState)
                                           (addToFrontOfState (variable expression) 'null (getclassinstance (getclass classState)))) (getclass classState)))))

(define MClass_declarefxn
  (lambda (expression state classState)
    (setmethods (addToFrontOfState ))))
     
(define name cadr)
(define fxnparam caddr)
(define fxnbody cadddr)
(define MClass_declarestaticfxn
  (lambda (expression state classState)
    (setmethods (addInReverseOrder (name expression) (createFunctionEnvironment expression state classState) (getmethods (getclass classState))) (getclass classState))))

(define addInReverseOrder
  (lambda (variable value state)
    (list (cons variable (car state)) (append (cadr state) (list (box value))))))

(define createFunctionEnvironment
  (lambda (expression state classState)
    (list (fxnparam expression) (fxnbody expression)
          (lambda (state)
            (createEnvironment (getname (getclass classState)) state))
          (lambda (state)
            (unbox (searchInStateAllLayer (getname (getclass classState)) state))))))

(define createNewClass
  (lambda (classname parent)
    (list 'class parent classname  (getParentField parent) (getParentMethod parent) (getParentname parent))))

(define getParentField
  (lambda (parent)
    (if (null? parent)
        initialState
        (getparent parent))))

(define getParentMethod
  (lambda (parent)
    (if (null? parent)
        initialState
        (getmethods parent))))

(define getParentname
  (lambda (parent)
    (if (null? parent)
        initialState
        (getclassinstance classState))))

(define getparent cadr)
(define getname caddr)
(define getfields cadddr)

(define getmethods
  (lambda (class)
    (cadr (cdddr class))))

(define getclassinstance
  (lambda (class)
    (caddr (cdddr class))))

(define setfields
  (lambda (newFields class)
    (list 'class (getparent class) (getname class) newFields (getmethods class) (getclassinstance class))))

(define setmethods
  (lambda (newMethods class)
    (list 'class (getparent class) (getname class) (getfields class) newMethods (getclassinstance class))))

(define setclassinstance
  (lambda (newClassInstance class)
    (list 'class (getparent class) (getname class) (getfields class) (getmethods class) newClassInstance)))

(define 1stExpression car)
(define restOfExpression cdr)
;Sends each chunk of code to M_state for computation. If a return statement is reached, it call/cc to here so that nothing else is executed
(define evaluate
  (lambda (expressions classname state return)
    (call/cc
     (lambda (rtn)
       (letrec ((loop (lambda (expressions state classState)
                        (cond
                          ((null? expressions) (M_value (append (cons 'funcall '()) (cons '(dot A main) '())) (cons state '()) classState))
                          (else (loop (restOfExpression expressions) (class_declaration (1stExpression expressions) state classState) classState))))))
         (loop expressions state (initialClassState rtn)))))))

;Parses a file and sends to the evaluate function
(define interpret
  (lambda (filename classname)
    (checkResult (call/cc
                  (lambda (return)
                    (evaluate (parser filename) classname initialState return))))))

;converts the result to its corresponding boolean value if it exists
(define checkResult
  (lambda (result)
    (cond
      ((eq? result #t) 'true)
      ((eq? result #f) 'false)
      (else result))))