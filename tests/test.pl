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

test(simplify_add_zero) :-
    micro_math("0+x", "x").

% test(simplify_sub_zero) :-
%     micro_math("0-x", "-x").

test(simplify_mul_zero) :-
    micro_math("x*0", "0").

test(simplify_mul_zero_2) :-
    micro_math("0*x", "0").

test(simplify_div_zero) :-
    micro_math("0/x", "0").

test(simplify_exp_zero) :-
    micro_math("x^0", "1").

% test(simplify_neg_exp) :-
%     micro_math("x^(-1)", "1/x").

% test(simplify_double_neg) :-
%     micro_math("(-x)^2", "x^2").

% test(simplify_trig_identity) :-
%     micro_math("sin(x)^2+cos(x)^2", "1").

% test(simplify_exp_add) :-
%     micro_math("exp(x)*exp(y)", "exp(x+y)").

test(simplify_nested_trig) :-
    micro_math("sin(cos(sin(0)))", "sin(1)").

test(simplify_nested_pow) :-
    micro_math("(x^2)^3", "x^6").

test(simplify_nested_mul) :-
    micro_math("2*(3*x)", "6*x").

test(simplify_nested_div) :-
    micro_math("(x/2)/4", "x/8").

test(simplify_mixed_terms) :-
    micro_math("x^2+2*x*y+y^2", "x^2+2*x*y+y^2").

test(simplify_polynomial) :-
    micro_math("x^2+2*x+1", "x^2+2*x+1").

test(simplify_trig_functions) :-
    micro_math("sin(0)+cos(0)", "1").

test(simplify_nested_functions) :-
    micro_math("sin(cos(0))", "sin(1)").

test(simplify_complex_expression) :-
    micro_math("2*x+3*y-0", "2*x+3*y").

:- end_tests(composed_test).
