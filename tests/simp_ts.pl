:- use_module('../src/simp.pl').


% :- begin_tests(contradicions). 

% test(zero_contradiction) :-
%     simplify(pow(num(0), pow(num(0), num(2))), num(0)).

% test(divide_by_zero_edge_cases) :-
%     simplify(dvd(num(5), num(0)), _),
%     simplify(dvd(vrb(x), num(0)), _),
%     simplify(dvd(pow(vrb(x), num(2)), num(0)), _).

% test(imaginary numbers) :- 
%     simplify(pow(num(-1), dvd(num(1), num(2))), _),

% :- end_tests(contradicions). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% Test for Simplification without integration and Differentiation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- begin_tests(simplify).

test(simple_num) :-
    simplify(num(5), num(5)),
    simplify(num(5.12), num(5.12)).

test(simple_vrb) :-
    simplify(vrb(x), vrb(x)),
    simplify(vrb(y), vrb(y)),
    simplify(vrb(h1), vrb(h1)).

test(simple_add_sub_mul_dvd) :-
    simplify(add(num(1), num(2)), num(3)),
    simplify(sub(num(5), num(2)), num(3)),
    simplify(mul(num(2), num(3)), num(6)),
    simplify(dvd(num(6), num(2)), num(3)).

test(zero) :-
    simplify(add(num(0), num(5)), num(5)),
    simplify(add(num(5), num(0)), num(5)),
    simplify(sub(num(5), num(0)), num(5)),
    simplify(mul(num(0), num(5)), num(0)),
    simplify(mul(num(5), num(0)), num(0)),
    simplify(dvd(num(0), num(5)), num(0)),
    simplify(pow(num(0), num(5)), num(0)),
    simplify(pow(num(5), num(0)), num(1)),
    simplify(add(num(0), vrb(x)), vrb(x)),
    simplify(add(vrb(x), num(0)), vrb(x)),
    simplify(sub(vrb(x), num(0)), vrb(x)),
    simplify(mul(num(0), vrb(x)), num(0)),
    simplify(mul(vrb(x), num(0)), num(0)),
    simplify(dvd(num(0), vrb(x)), num(0)),
    simplify(pow(num(0), vrb(x)), num(0)),
    simplify(pow(vrb(x), num(0)), num(1)).

test(zero_complex_expr) :-
    simplify(add(mul(num(0), vrb(x)), mul(num(0), vrb(y))), num(0)),
    simplify(add(mul(num(0), pow(vrb(x), num(2))), mul(num(0), pow(vrb(y), num(2)))), num(0)),
    simplify(sub(mul(num(0), vrb(x)), mul(num(0), vrb(y))), num(0)),
    simplify(sub(mul(num(0), pow(vrb(x), num(2))), mul(num(0), pow(vrb(y), num(2)))), num(0)),
    simplify(mul(num(0), add(vrb(x), vrb(y))), num(0)),
    simplify(mul(num(0), sub(vrb(x), vrb(y))), num(0)),
    simplify(mul(num(0), pow(vrb(x), num(2))), num(0)),
    simplify(mul(num(0), pow(vrb(x), vrb(y))), num(0)).

test(zero_pow_edge_cases) :-
    simplify(pow(num(1), pow(num(0), num(2))), num(1)),
    simplify(pow(vrb(x), pow(num(0), num(2))), num(1)),
    simplify(pow(num(1), pow(num(0), vrb(x))), num(1)),
    simplify(pow(vrb(x), pow(num(0), vrb(x))), num(1)).

test(invalid_powers) :-
    simplify(pow(vrb(x), vrb(y)), _),
    simplify(pow(pow(vrb(x), num(2)), vrb(y)), _).

test(nested_zero_edge_cases) :-
    simplify(add(mul(num(0), add(vrb(x), vrb(y))), num(5)), num(5)),
    simplify(mul(num(0), mul(vrb(x), vrb(y))), num(0)),
    simplify(sub(mul(num(0), sub(vrb(x), vrb(y))), num(6)), num(-6)),
    simplify(add(num(0), mul(num(0), vrb(x))), num(0)).

test(simle_pow) :-
    simplify(pow(num(2), num(2)), num(4)),
    simplify(pow(num(1.2), num(3.1)), T),
    T = num(1.7597941219820576),
    simplify(pow(vrb(x), num(0)), num(1)),
    simplify(pow(vrb(x), num(1)), vrb(x)),
    simplify(pow(num(0), vrb(x)), num(0)),
    simplify(pow(num(1), vrb(x)), num(1)),
    simplify(pow(num(-1), num(2)), num(1)),
    simplify(pow(num(-1), num(3)), num(-1)),
    simplify(pow(pow(vrb(x), num(2)), num(3)), pow(vrb(x), num(6))).

test(add_variable) :-
    simplify(dvd(num(6), vrb(x)), T1),
    T1 = dvd(num(6), vrb(x)),
    simplify(add(vrb(x), num(2)), T2),
    T2 = add(vrb(x), num(2)),
    simplify(mul(vrb(z), num(3)), mul(vrb(z), num(3))),
    simplify(dvd(vrb(h1), num(2)), dvd(vrb(h1), num(2))),
    simplify(sub(vrb(k), num(2)), sub(vrb(k), num(2))).

test(multi_variable) :-
    simplify(add(vrb(k), vrb(x)), add(vrb(k), vrb(x))),
    simplify(add(vrb(x), add(num(2), vrb(p))), T),
    T = add(vrb(x), add(num(2), vrb(p))).

test(nested_arith) :-
    simplify(add(num(1), add(num(2), num(3))), num(6)),
    simplify(dvd(num(6), mul(num(2), num(3))), num(1)).

test(nested_complex) :-
    simplify(add(num(1), mul(num(2), add(num(3), num(4)))), T),
    T = num(15).

test(mul_with_zero) :-
    simplify(mul(num(0), vrb(x)), num(0)).

% Test differentiation simplification
test(diff_simple) :-
    simplify(diff(vrb(x), vrb(x)), R),
    R = num(1),
    !.

% Test integration simplification
test(integ_simple) :-
    simplify(integ(num(5), vrb(x)), R),
    R = mul(num(5), vrb(x)),
    !.

:- end_tests(simplify).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% Test for Integration 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :- begin_tests(integration).

% test(constant) :-
%     simplify(integ(num(3), vrb(x)), R),
%     R = mul(num(3), vrb(x)).

% test(variable) :-
%     simplify(integ(vrb(x), vrb(x)), R),
%     R = mul(num(0.5), pow(vrb(x), num(2))).

% test(power) :-
%     simplify(integ(pow(vrb(x), num(3)), vrb(x)), R),
%     R = mul(num(0.25), pow(vrb(x), num(4))).

% test(exponential) :-
%     simplify(integ(exp(vrb(x)), vrb(x)), R),
%     R = exp(vrb(x)).

% test(logarithmic) :-
%     simplify(integ(ln(vrb(x)), vrb(x)), R),
%     R = sub(mul(vrb(x), ln(vrb(x))), vrb(x)).

% test(constant_times_function) :-
%     simplify(integ(mul(num(2), pow(vrb(x), num(2))), vrb(x)), R),
%     R = mul(num(2), mul(num(1/3), pow(vrb(x), num(3)))).

% test(sum) :-
%     simplify(integ(add(pow(vrb(x), num(2)), pow(vrb(x), num(3))), vrb(x)), R),
%     R = add(mul(num(1/3), pow(vrb(x), num(3))), mul(num(1/4), pow(vrb(x), num(4)))).

% test(difference) :-
%     simplify(integ(sub(pow(vrb(x), num(3)), pow(vrb(x), num(2))), vrb(x)), R),
%     R = sub(mul(num(1/4), pow(vrb(x), num(4))), mul(num(1/3), pow(vrb(x), num(3)))).

% % Additional tests
% test(multiple_terms) :-
%     simplify(integ(add(mul(num(3), vrb(x)), mul(num(4), pow(vrb(x), num(2)))), vrb(x)), R),
%     R = add(mul(num(3/2), pow(vrb(x), num(2))), mul(num(4/3), pow(vrb(x), num(3)))).

% test(sin) :-
%     simplify(integ(sin(vrb(x)), vrb(x)), R),
%     R = sub(num(0), cos(vrb(x))).

% test(cos) :-
%     simplify(integ(cos(vrb(x)), vrb(x)), R),
%     R = sin(vrb(x)).

% :- end_tests(integration).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% Test for Differentiation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- begin_tests(differentiation).

test(diff_constant) :-
    simplify(diff(num(3), vrb(x)), R),
    R = num(0).

test(diff_variable) :-
    simplify(diff(vrb(x), vrb(x)), R),
    R = num(1).

test(diff_add_rule) :-
    simplify(diff(add(num(8), vrb(x)), vrb(x)), R),
    R = num(1).

test(diff_mul_rule) :-
    simplify(diff(mul(num(8), vrb(x)), vrb(x)), R),
    R = num(8).

test(constant) :-
    simplify(diff(num(5), vrb(x)), num(0)),
    simplify(diff(num(-5), vrb(x)), num(0)),
    simplify(diff(num(0), vrb(x)), num(0)).

test(linear) :-
    simplify(diff(vrb(x), vrb(x)), num(1)),
    simplify(diff(mul(num(3), vrb(x)), vrb(x)), num(3)),
    simplify(diff(mul(num(-3), vrb(x)), vrb(x)), num(-3)),
    simplify(diff(mul(num(0), vrb(x)), vrb(x)), num(0)).

test(power) :-
    simplify(diff(pow(vrb(x), num(2)), vrb(x)), mul(num(2), vrb(x))),
    simplify(diff(pow(vrb(x), num(3)), vrb(x)), mul(num(3), pow(vrb(x), num(2)))),
    simplify(diff(pow(vrb(x), num(0)), vrb(x)), num(0)).

test(product) :-
    simplify(diff(mul(vrb(x), vrb(y)), vrb(x)), vrb(y)),
    simplify(diff(mul(vrb(x), vrb(y)), vrb(y)), vrb(x)),
    simplify(diff(mul(num(2), mul(vrb(x), vrb(y))), vrb(x)), mul(num(2), vrb(y))).

% test(quotient) :-
%     simplify(diff(dvd(vrb(x), vrb(y)), vrb(x)), pow(vrb(y), num(-1))),
%     simplify(diff(dvd(vrb(y), vrb(x)), vrb(x)), dvd(num(-1), pow(vrb(x), num(2)))),
%     simplify(diff(dvd(num(2), mul(vrb(x), vrb(y))), vrb(x)), dvd(num(2), mul(vrb(y), pow(vrb(x), num(2))))).

% test(chain) :-
%     simplify(diff(sin(vrb(x)), vrb(x)), cos(vrb(x))),
%     simplify(diff(cos(vrb(x)), vrb(x)), mul(num(-1), sin(vrb(x)))),
%     simplify(diff(exp(vrb(x)), vrb(x)), exp(vrb(x))).

% test(chain_complex) :-
%     simplify(diff(sin(mul(num(2), vrb(x))), vrb(x)), mul(num(2), cos(mul(num(2), vrb(x))))),
%     simplify(diff(exp(add(vrb(x), num(2))), vrb(x)), exp(add(vrb(x), num(2)))),
%     simplify(diff(cos(sub(vrb(x), vrb(y))), vrb(x)), mul(num(-1), sin(sub(vrb(x), vrb(y))))),
%     simplify(diff(cos(sub(vrb(x), vrb(y))), vrb(y)), sin(sub(vrb(x), vrb(y)))).

% test(chain_product) :-
%     simplify(diff(mul(exp(vrb(x)), sin(vrb(x))), vrb(x)), add(mul(exp(vrb(x)), sin(vrb(x))), mul(exp(vrb(x)), cos(vrb(x))))),
%     simplify(diff(mul(cos(vrb(x)), pow(vrb(x), num(3))), vrb(x)), add(mul(mul(num(-1), sin(vrb(x))), pow(vrb(x), num(3))), mul(num(3), mul(cos(vrb(x)), pow(vrb(x), num(2)))))).

test(diff_power) :-
    simplify(diff(pow(vrb(x), num(3)), vrb(x)), R),
    R = mul(num(3), pow(vrb(x), num(2))).

test(diff_exponential) :-
    simplify(diff(exp(vrb(x)), vrb(x)), R),
    R = exp(vrb(x)).

% test(diff_logarithmic) :-
%     simplify(diff(ln(vrb(x)), vrb(x)), R),
%     R = dvd(num(1), vrb(x)).

test(diff_sin) :-
    simplify(diff(sin(vrb(x)), vrb(x)), R),
    R = cos(vrb(x)).

test(diff_cos) :-
    simplify(diff(cos(vrb(x)), vrb(x)), R),
    R = sub(num(0), sin(vrb(x))).

% test(diff_tan) :-
%     simplify(diff(tan(vrb(x)), vrb(x)), R),
%     R = pow(sec(vrb(x)), num(2)).

test(diff_product_rule) :-
    simplify(diff(mul(vrb(x), exp(vrb(x))), vrb(x)), R),
    R = add(exp(vrb(x)), mul(vrb(x), exp(vrb(x)))).

% test(diff_quotient_rule) :-
%     simplify(diff(dvd(vrb(x), exp(vrb(x))), vrb(x)), R),
%     R = dvd(sub(exp(vrb(x)), vrb(x)), pow(exp(vrb(x)), num(2))).

test(diff_chain_rule) :-
    simplify(diff(exp(sin(vrb(x))), vrb(x)), R),
    R = mul(cos(vrb(x)), exp(sin(vrb(x)))).

% test(diff_second_derivative) :-
%     simplify(diff(diff(pow(vrb(x), num(3)), vrb(x)), vrb(x)), R),
%     R = mul(num(6), vrb(x)).

test(diff_third_derivative) :-
    simplify(diff(diff(diff(pow(vrb(x), num(3)), vrb(x)), vrb(x)), vrb(x)), R),
    R = num(6).

% test(diff_complex_expr_1) :-
%     simplify(diff(sin(mul(exp(vrb(x)), cos(vrb(x)))), vrb(x)), R),
%     R = mul(exp(vrb(x)), mul(cos(vrb(x)), mul(cos(mul(exp(vrb(x)), cos(vrb(x)))), add(mul(sin(vrb(x)), exp(vrb(x))), mul(num(-1), sin(vrb(x))))))).

% test(diff_complex_expr_2) :-
%     simplify(diff(pow(ln(mul(vrb(x), exp(vrb(x)))), num(2)), vrb(x)), R),
%     R = mul(dvd(num(2), vrb(x)), ln(mul(vrb(x), exp(vrb(x))))).

% test(diff_complex_expr_3) :-
%     simplify(diff(diff(diff(diff(mul(vrb(x), ln(vrb(x))), vrb(x)), vrb(x)), vrb(x)), vrb(x)), R),
%     R = dvd(num(6), pow(vrb(x), num(4))).

% test(diff_complex_expr_4) :-
%   simplify(diff(add(pow(vrb(x), num(3)), mul(num(2), pow(vrb(x), num(2))), mul(num(3), vrb(x)), num(4)), vrb(x)), R),
%   R = add(mul(num(3), pow(vrb(x), num(2))), mul(num(4), vrb(x)), num(3)).

test(diff_complex_expr_5) :-
  simplify(diff(mul(sin(vrb(x)), add(exp(vrb(x)), num(1))), vrb(x)), R),
  R = add(mul(cos(vrb(x)), add(exp(vrb(x)), num(1))), mul(sin(vrb(x)), exp(vrb(x)))).

% test(diff_complex_expr_6) :-
%   simplify(diff(dvd(mul(pow(vrb(x), num(3)), exp(vrb(x))), add(vrb(x), num(1))), vrb(x)), R),
%   R = dvd(sub(mul(exp(vrb(x)), add(mul(num(4), pow(vrb(x), num(2))), mul(num(3), vrb(x)))), mul(vrb(x), mul(pow(vrb(x), num(3)), exp(vrb(x))))), pow(add(vrb(x), num(1)), num(2))).

% test(diff_complex_expr_7) :-
%   simplify(diff(mul(cos(vrb(x)), sin(add(vrb(x), exp(vrb(x))))), vrb(x)), R),
%   R = add(mul(num(-1), mul(sin(vrb(x)), sin(add(vrb(x), exp(vrb(x)))))), mul(cos(vrb(x)), mul(cos(add(vrb(x), exp(vrb(x)))), exp(vrb(x))))).

% test(diff_complex_expr_8) :-
%   simplify(diff(pow(exp(pow(vrb(x), num(2))), num(3)), vrb(x)), R),
%   R = mul(num(6), mul(exp(mul(pow(vrb(x), num(2)), num(3))), vrb(x))).

% test(diff_complex_expr_9) :-
%   simplify(diff(ln(mul(vrb(x), add(vrb(x), num(1)))), vrb(x)), R),
%   R = dvd(add(num(1), vrb(x)), mul(vrb(x), add(vrb(x), num(1)))).

% test(diff_complex_expr_10) :-
%   simplify(diff(pow(cos(add(mul(vrb(x), num(2)), num(3))), num(2)), vrb(x)), R),
%   R = mul(num(-4), mul(vrb(x), sin(add(mul(vrb(x), num(2)), num(3))))).

% test(diff_complex_expr_11) :-
%   simplify(diff(pow(sin(pow(vrb(x), num(3))), num(4)), vrb(x)), R),
%   R = mul(num(12), mul(pow(vrb(x), num(2)), mul(pow(sin(pow(vrb(x), num(3))), num(3)), cos(pow(vrb(x), num(3)))))).

% test(diff_complex_expr_12) :-
%   simplify(diff(exp(mul(pow(vrb(x), num(2)), num(3))), vrb(x)), R),
%   R = mul(num(6), mul(vrb(x), exp(mul(pow(vrb(x), num(2)), num(3))))).

% test(diff_complex_expr_13) :-
%   simplify(diff(add(pow(vrb(x), num(3)), mul(pow(vrb(x), num(2)), num(2)), mul(vrb(x), num(3)), num(4)), vrb(x)), R),
%   R = add(mul(num(3), pow(vrb(x), num(2))), mul(num(4), vrb(x)), num(3)).

% test(diff_complex_expr_14) :-
%   simplify(diff(add(exp(vrb(x)), mul(cos(vrb(x)), sin(vrb(x)))), vrb(x)), R),
%   R = add(exp(vrb(x)), sub(mul(num(-1), sin(vrb(x))), mul(cos(vrb(x)), cos(vrb(x))))).

% test(diff_complex_expr_15) :-
%   simplify(diff(mul(ln(add(vrb(x), num(1))), exp(vrb(x))), vrb(x)), R),
%   R = add(mul(exp(vrb(x)), ln(add(vrb(x), num(1)))), mul(exp(vrb(x)), dvd(num(1), add(vrb(x), num(1))))).

% test(diff_complex_expr_16) :-
%   simplify(diff(sub(pow(vrb(x), num(4)), mul(num(4), pow(vrb(x), num(2)))), vrb(x)), R),
%   R = sub(mul(num(4), pow(vrb(x), num(3))), mul(num(8), vrb(x))).

% test(diff_complex_expr_17) :-
%   simplify(diff(dvd(mul(num(2), exp(vrb(x))), add(vrb(x), num(1))), vrb(x)), R),
%   R = dvd(sub(mul(num(2), exp(vrb(x))), mul(num(2), mul(exp(vrb(x)), add(vrb(x), num(1))))), pow(add(vrb(x), num(1)), num(2))).

% test(diff_complex_expr_18) :-
%   simplify(diff(add(sin(vrb(x)), pow(cos(vrb(x)), num(3))), vrb(x)), R),
%   R = sub(mul(num(3), mul(pow(cos(vrb(x)), num(2)), num(-1), sin(vrb(x)))), cos(vrb(x))).

:- end_tests(differentiation).
