:- begin_tests(typeInf).
:- include(typeInf). 

/* Note: when writing tests keep in mind that 
    the use of of global variable and function definitions
    define facts for gvar() predicate. Either test
    directy infer() predicate or call
    delegeGVars() predicate to clean up gvar().
*/

% tests for typeExp
test(typeExp_add_1) :- 
    functionType(add, [int, [Y, [X]]]).

test(typeExp_add_2) :- 
    functionType(apply, [ADD, int, T]), functionType(add, ADD).

test(typeExp_multiply) :- 
    functionType(multiply, [X, [Y, [int]]]).

test(typeExp_subtract) :- 
    functionType(subtract, [X, [Y, [Z]]]).

test(typeExp_exponentiate) :-
    functionType(exponentiate, [X, [Y, [Z]]]).

test(typeExp_bool_of_string) :-
    functionType(bool_of_string, [X, [Y]]).

test(typeExp_equal) :-
    functionType(equal, [int, [X, [Z]]]).

test(typeExp_print) :-
    functionType(print, [string, [Y]]).

test(typeExp_or) :-
    functionType(or, [X, [Y, [Z]]]).

test(greater_than) :-
    functionType(greater_than, [int, [Y, [T]]]).

test(less_than_1) :-
    functionType(less_than, [X, [int, [T]]]).

% This test should fail
test(less_than_2) :-
    functionType(less_than, [float, [int, [bool]]]).

test(less_than_3) :-
    functionType(less_than, [string, [string, [T]]]).       

% This test should fail
test(string_of_int_1) :-
    functionType(string_of_int, [float, [_]]).

test(string_of_int_2) :-
    functionType(string_of_int, [int, [T]]).                    

test(typeExp_apply_1) :-
    functionType(apply, [[X,Y], string, B]).

test(typeExp_apply_2) :-
    functionType(apply, [ [int, [int]], int, T]).

test(typeExp_apply_3) :-
    functionType(apply, [PRINT, string, T]), functionType(print, PRINT).

test(typeExp_reverse_apply_1) :-
    functionType(reverse_apply, [int, ADD, T]), functionType(add, ADD).

test(typeExp_reverse_apply_2) :-
    functionType(reverse_apply, [float, ADD2, T]), functionType(apply, [ADD, float, ADD2]), functionType(add, ADD).
    

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
