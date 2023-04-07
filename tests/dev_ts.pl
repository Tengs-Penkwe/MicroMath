% :- asserta(library_directory('../src')).
:- use_module(library(plunit)).

:- use_module('../src/dev', [dv/3]).

:- begin_tests(derivation).

test(constant) :-
    dv(5, _, 0).

test(variable) :-
    dv(X, X, 1).

test(addition) :-
    dv(X + 3, X, 1).

test(subtraction) :-
    dv(3 - X, X, -1).

test(multiplication) :-
    dv(2 * X, X, 2).

test(power) :-
    dv(X^2, X, 2 * X).

:- end_tests(derivation).


% :- run_tests.

