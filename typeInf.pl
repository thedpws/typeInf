/* match functions by unifying with arguments 
    and infering the result
*/
exprType(Fct, T):-
    \+ var(Fct), /* make sure Fct is not a variable */ 
    \+ atom(Fct), /* or an atom */
    functor(Fct, Fname, _Nargs), /* ensure we have a functor */
    !, /* if we make it here we do not try anything else */
    Fct =.. [Fname|Args], /* get list of arguments */
    append(Args, [T], FType), /* make it loook like a function signature */
    functionType(Fname, TArgs), /* get type of arguments from definition */
    exprTypeList(FType, TArgs). /* recurisvely match types */

/* propagate types */
exprType(T, T).

/* list version to allow function mathine */
exprTypeList([], []).
exprTypeList([Hin|Tin], [Hout|Tout]):-
    exprType(Hin, Hout), /* type infer the head */
    exprTypeList(Tin, Tout). /* recurse */

/* TODO: add statements types and their type checking */
/* global variable definition
    Example:
        gvLet(v, T, int) ~ let v = 3;
 */
statementType(gvLet(Name, T, Code), unit):-
    atom(Name), /* make sure we have a bound name */
    exprType(Code, T), /* infer the type of Code and ensure it is T */
    basicType(T), /* make sure we have an infered type */
    asserta(gvar(Name, T)). /* add definition to database */

/* If statement */
statementType(if(Cond, TCode, FCode), T) :-
    typeExp(Cond, bool),
    typeCode(TCode, T),
    typeCode(FCode, T),
    bType(T).

/* Expressions are statements */
statementType(Expr, T) :-
    typeExp(Expr, T),
    bType(T).

/* Code is simply a list of statements. The type is 
    the type of the last statement 
*/
codeBlockType([], _T).
codeBlockType([S], T):-statementType(S, T).
codeBlockType([S, S2|Code], T):-
    statementType(S,_T),
    codeBlockType([S2|Code], T).

/* top level function */
infer(Code, T) :-
    is_list(Code), /* make sure Code is a list */
    deleteGVars(), /* delete all global definitions */
    codeBlockType(Code, T).

/* Basic types
    TODO: add more types if needed
 */
basicType(bool).
basicType(int).
basicType(float).
basicType(string).
basicType(unit). /* unit type for things that are not expressions */
/*  functions type.
    The type is a list, the last element is the return type
    E.g. add: int->int->int is represented as [int, int, int]
    and can be called as add(1,2)->3
 */
basicType([H]):- basicType(H).
basicType([H|T]):- basicType(H), basicType(T).

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

functionType('<', [float, float, bool]).

functionType(iplus, [int,int,int]).
functionType(fplus, [float, float, float]).
functionType(fToInt, [float,int]).
functionType(iToFloat, [int,float]).
functionType(print, [_X, unit]). /* simple print */

/* Find function signature
   A function is either buld in using functionType or
   added as a user definition with gvar(fct, List)
*/

% Check the user defined functions first
functionType(Name, Args):-
    gvar(Name, Args),
    is_list(Args). % make sure we have a function not a simple variable

% Check first built in functions
functionType(Name, Args) :-
    functionType(Name, Args), !. % make deterministic

% This gets wiped out but we have it here to make the linter happy
gvar(_, _) :- false().
