:- use_module('../main.pl').

:- begin_tests(composed_test).

test(simplify_addition) :-
    micro_math("x+0", "x").

test(simplify_subtraction) :-
    micro_math("x-0", "x").

test(simplify_multiplication) :-
    micro_math("x*1", "x").

test(simplify_division) :-
    micro_math("x/1", "x").

test(simplify_exponentiation) :-
    micro_math("x^1", "x").

test(simplify_polynomial) :-
    micro_math("x^2+2*x+1", "x^2+2*x+1").

test(simplify_trig_functions) :-
    micro_math("sin(0)+cos(0)", "1").

test(simplify_nested_functions) :-
    micro_math("sin(cos(0))", "sin(1)").

test(simplify_complex_expression) :-
    micro_math("2*x+3*y-0", "2*x+3*y").

:- end_tests(composed_test).
