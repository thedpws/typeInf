:- begin_tests(typeInf).
:- include(typeInf). 

/* Note: when writing tests keep in mind that 
    the use of of global variable and function definitions
    define facts for gvar() predicate. Either test
    directy infer() predicate or call
    delegeGVars() predicate to clean up gvar().
*/

% tests for typeExp
test(typeExp_add) :- 
    typeExp(add(int,int), int).

test(typeExp_multiply) :- 
    typeExp(multiply(int, X), int).

test(typeExp_subtract) :- 
    typeExp(subtract(X, Y), Z).

test(typeExp_exponentiate) :-
    typeExp(exponentiate(X, Y), Z).

test(typeExp_bool_of_string) :-
    typeExp(bool_of_string(X), Y).

test(typeExp_equal) :-
    typeExp(equal(int, int), Y).

test(typeExp_print) :-
    typeExp(print(string), Y).

test(typeExp_or) :-
    typeExp(or(X, Y), Z).

% this test should fail
test(typeExp_add_F, [fail]) :-                    
    typeExp(add(int, int), bool).

test(typeExp_add_T, [true(T == int)]) :-
    typeExp(add(int, int), T).

test(greater_than) :-
    functionType(greater_than, [int, int, T]).

test(less_than_1) :-
    functionType(less_than, [int, int, T]).

% This test should fail
test(less_than_2) :-
    functionType(less_than, [float, int, bool]).

test(less_than_3) :-
    functionType(less_than, [string, string, T]).       

% This test should fail
test(string_of_int_1) :-
    functionType(string_of_int, [float, _]).

test(string_of_int_2) :-
    functionType(string_of_int, [int, T]).                    

test(typeExp_apply) :-
    typeExp(apply(Y, X), T).

test(typeExp_reverse_apply) :-
    functionType(reverse_apply, [int, add, T]).

test(if_Statement) :- 
    


% NOTE: use nondet as option to test if the test is nondeterministic

% test for statement with state cleaning
test(typeStatement_gvar, [nondet, true(T == int)]) :- % should succeed with T=int
    deleteGVars(), /* clean up variables */
    typeStatement(gvLet(v, T, add(X, Y)), unit),
    assertion(X == int), assertion( Y == int), % make sure the types are int
    gvar(v, int). % make sure the global variable is defined

% % same test as above but with infer 
test(infer_gvar, [nondet]) :-
    infer([gvLet(v, T, add(X, Y))], unit),
    assertion(T==int), assertion(X==int), assertion(Y=int),
    gvar(v,int).

% test custom function with mocked definition
test(mockedFct, [nondet]) :-
    deleteGVars(), % clean up variables since we cannot use infer
    asserta(gvar(my_fct, [int, float])), % add my_fct(int)-> float to the gloval variables
    typeExp(my_fct(X), T), % infer type of expression using or function
    assertion(X==int), assertion(T==float). % make sure the types infered are correct

:-end_tests(typeInf).
