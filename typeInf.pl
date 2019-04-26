/* match functions by unifying with arguments 
    and infering the result
*/
typeExp(Fct, T):-
    \+ var(Fct), /* make sure Fct is not a variable */ 
    \+ atom(Fct), /* or an atom */
    functor(Fct, Fname, _Nargs), /* ensure we have a functor */
    !, /* if we make it here we do not try anything else */
    Fct =.. [Fname|Args], /* get list of arguments */
    append(Args, [T], FType), /* make it loook like a function signature */
    functionType(Fname, TArgs), /* get type of arguments from definition */
    typeExpList(FType, TArgs). /* recurisvely match types */

/* propagate types */
typeExp(T, T).

/* list version to allow function mathine */
typeExpList([], []).
typeExpList([Hin|Tin], [Hout|Tout]):-
    typeExp(Hin, Hout), /* type infer the head */
    typeExpList(Tin, Tout). /* recurse */

/* TODO: add statements types and their type checking */
/* global variable definition
    Example:
        gvLet(v, T, int) ~ let v = 3;
 */
typeStatement(gvLet(Name, T, Code), unit):-
    atom(Name), /* make sure we have a bound name */
    typeExp(Code, T), /* infer the type of Code and ensure it is T */
    bType(T), /* make sure we have an infered type */
    asserta(gvar(Name, T)). /* add definition to database */

/* Name = name of local variable
    LVarType = type of local variable
    Code = code from which we can infer type of local variable
    assert that both are the same.
*/
typeStatement(letIn(Name, LVarType, Code), Code2, T):-
    atom(Name),
    typeExp(Code, LVarType),
    bType(LVarType),
    asserta(gvar(Name, LVarType)),
    typeCode(Code2, T),
    retract(gvar(Name, LVarType)).
/* If statement */
typeStatement(if(Cond, TCode, FCode), T) :-
    typeExp(Cond, bool),
    typeCode(TCode, T),
    typeCode(FCode, T),
    bType(T).

/* Expressions are statements */
typeStatement(Expr, T) :-
    typeExp(Expr, T),
    bType(T).

/* Code is simply a list of statements. The type is 
    the type of the last statement 
*/
typeCode([], _T).
typeCode([S], T):-typeStatement(S, T).
typeCode([S, S2|Code], T):-
    typeStatement(S,unit),
    typeCode([S2|Code], T).

/* top level function */
infer(Code, T) :-
    is_list(Code), /* make sure Code is a list */
    deleteGVars(), /* delete all global definitions */
    typeCode(Code, T).

/* Basic types
    TODO: add more types if needed
 */
bType(bool).
bType(int).
bType(float).
bType(string).
bType(char).
bType(bool).

bType(unit). /* unit type for things that are not expressions */
/*  functions type.
    The type is a list, the last element is the return type
    E.g. add: int->int->int is represented as [int, int, int]
    and can be called as add(1,2)->3
 */
bType([H]):- bType(H).
bType([H|T]):- bType(H), bType(T).

/*
    TODO: as you encounter global variable definitions
    or global functions add their definitions to 
    the database using:
        asserta( gvar(Name, Type) )
    To check the types as you encounter them in the code
    use:
        gvar(Name, Type) with the Name bound to the name.
    Type will be bound to the global type
    Examples:
        g

    Call the predicate deleveGVars() to delete all global 
    variables. Best wy to do this is in your top predicate
*/

deleteGVars():-retractall(gvar), asserta(gvar(_X,_Y):-false()).

/*  builtin functions
    Each definition specifies the name and the 
    type as a function type

    TODO: add more functions
*/

/* These values allow arithmetic*/
numBType(int).
numBType(float).

fType(  add,                [X,X,X]) :- numBType(X).
fType(  subtract,           [X,X,X]) :- numBType(X).
fType(  multiply,           [X,X,X]) :- numBType(X).
fType(  divide,             [X,X,X]) :- numBType(X).
fType(  exponentiate,       [float, float, float]).
fType(  mod,                [int, int, int] ).
fType(  int_of_float,       [float,int]     ).
fType(  float_of_int,       [int,float]     ).
fType(  print,              [_X, unit]      ).
ftype(  string_of_float,    [float, string] ).
ftype(  string_of_int,      [int, string]   ).
ftype(  int_of_string,      [string, int]   ).
ftype(  float_of_string,    [string, float] ).
ftype(  string_of_bool,     [bool, string]  ).
fType(  bool_of_string,     [string, bool]  ).
fType(  string_of_char,     [char, string]  ).
fType(  char_of_string,     [string, char]  ).
fType(  negation,           [int,int]       ).
fType(  negation,           [float, float]  ).
fType(  negation,           [bool, bool]    ).
fType(  equal,              [X,X,bool]      ).
fType(  nequal,             [X,X,bool]      ).
fType(  less_than,          [X,X,bool]      ).
fType(  greater_than,       [X,X,bool]      ).
fType(  lteq,               [X,X,bool]      ).
fType(  gteq,               [X,X,bool]      ).
fType(  compare,            [X,X,int]       ).
fType(  or,                 [bool,bool,bool]).
fType(  and,                [bool,bool,bool]).
fType(  concat_string,      [string,string,string]).
fType(  concat_list,        [X,X,X]) :- is_list(X).


/* Find function signature
   A function is either buld in using fType or
   added as a user definition with gvar(fct, List)
*/

% Check the user defined functions first
functionType(Name, Args):-
    gvar(Name, Args),
    is_list(Args). % make sure we have a function not a simple variable

% Check first built in functions
functionType(Name, Args) :-
    fType(Name, Args), !. % make deterministic

% This gets wiped out but we have it here to make the linter happy
gvar(_, _) :- false().
