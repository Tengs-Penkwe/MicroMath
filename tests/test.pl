:- use_module('../main.pl').

:- begin_tests(composed_test).

test(simplify_addition) :-
    micro_math("x+0", "x"),!.

test(simplify_subtraction) :-
    micro_math("x-0", "x"),!.

test(simplify_multiplication) :-
    micro_math("x*1", "x"),!.

test(simplify_division) :-
    micro_math("x/1", "x"),!.

test(simplify_exponentiation) :-
    micro_math("x^1", "x"),!.

test(simplify_add_zero) :-
    micro_math("0+x", "x"),!.

% test(simplify_sub_zero) :-
%     micro_math("0-x", "-x"),!.

test(simplify_mul_zero) :-
    micro_math("x*0", "0"),!.

test(simplify_mul_zero_2) :-
    micro_math("0*x", "0"),!.

test(simplify_div_zero) :-
    micro_math("0/x", "0"),!.

test(simplify_exp_zero) :-
    micro_math("x^0", "1"),!.

% test(simplify_neg_exp) :-
%     micro_math("x^(-1)", "1/x"),!.

% test(simplify_double_neg) :-
%     micro_math("(-x)^2", "x^2"),!.

% test(simplify_trig_identity) :-
%     micro_math("sin(x)^2+cos(x)^2", "1"),!.

% test(simplify_exp_add) :-
%     micro_math("exp(x)*exp(y)", "exp(x+y)"),!.

test(simplify_nested_trig) :-
    micro_math("sin(cos(sin(0)))", "sin(1)"),!.

test(simplify_nested_pow) :-
    micro_math("(x^2)^3", "x^6"),!.

% test(simplify_nested_mul) :-
%     micro_math("2*(3*x)", "6*x"),!.

% test(simplify_nested_div) :-
%     micro_math("(x/2)/4", "x/8"),!.

test(simplify_mixed_terms) :-
    micro_math("x^2+2*x*y+y^2", "x^2+2*x*y+y^2"),!.

test(simplify_polynomial) :-
    micro_math("x^2+2*x+1", "x^2+2*x+1"),!.

test(simplify_trig_functions) :-
    micro_math("sin(0)+cos(0)", "1"),!.

test(simplify_nested_functions) :-
    micro_math("sin(cos(0))", "sin(1)"),!.

test(simplify_complex_expression) :-
    micro_math("2*x+3*y-0", "2*x+3*y"),!.

% Test basic differentiation
test(diff_variable) :-
    micro_math("Dx(x)", "1"),!.

test(diff_const) :-
    micro_math("Dx(3)", "0"),!.

test(diff_addition) :-
    micro_math("Dx(x+3)", "1"),!.

test(diff_subtraction) :-
    micro_math("Dx(x-3)", "1"), !.

test(diff_multiplication) :-
    micro_math("Dx(3*x)", "3"), !.

% test(diff_division) :-
%     micro_math("Dx(x/3)", "1/3"), !.

test(diff_power) :-
    micro_math("Dx(x^3)", "3*x^2").

% Test differentiation with functions
test(diff_sin) :-
    micro_math("Dx(sin(x))", "cos(x)").

test(diff_cos) :-
    micro_math("Dx(cos(x))", "0-sin(x)").

test(diff_exp) :-
    micro_math("Dx(exp(x))", "exp(x)").

% Test differentiation with composed expressions
test(diff_composed_expression_1) :-
    micro_math("Dx(sin(x)+3*x)", "cos(x)+3").

% test(diff_composed_expression_2) :-
%     micro_math("Dx(3*x^2-4*x+2)", ).

% test(diff_composed_expression_3) :-
%     micro_math("Dx(x/(2+sin(x)))", dvd(sub(sin(vrb(x)), mul(vrb(x), cos(vrb(x)))), pow(add(num(2), sin(vrb(x))), num(2)))).

:- end_tests(composed_test).
