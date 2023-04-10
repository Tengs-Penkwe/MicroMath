:- use_module('../src/simp.pl').

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

test(simle_pow) :-
    simplify(pow(num(2), num(2)), num(4)),
    simplify(pow(num(1.2), num(3.1)), T),
    T = num(1.7597941219820576).

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
    simplify(mul(num(0), x), num(0)).

% Test differentiation simplification
test(diff_simple) :-
    simplify(diff(x, x), R),
    R = num(1),
    !.

% Test integration simplification
test(integ_simple) :-
    simplify(integ(num(5), x), R),
    R = mul(num(5), x),
    !.

:- end_tests(simplify).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% Test for Integration 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- begin_tests(integration).

test(constant) :-
    simplify(integ(num(3), vrb(x)), R),
    R = mul(num(3), vrb(x)).

test(variable) :-
    simplify(integ(vrb(x), vrb(x)), R),
    R = mul(num(0.5), pow(vrb(x), num(2))).

test(power) :-
    simplify(integ(pow(vrb(x), num(3)), vrb(x)), R),
    R = mul(num(0.25), pow(vrb(x), num(4))).

test(exponential) :-
    simplify(integ(exp(vrb(x)), vrb(x)), R),
    R = exp(vrb(x)).

test(logarithmic) :-
    simplify(integ(ln(vrb(x)), vrb(x)), R),
    R = sub(mul(vrb(x), ln(vrb(x))), vrb(x)).

test(constant_times_function) :-
    simplify(integ(mul(num(2), pow(vrb(x), num(2))), vrb(x)), R),
    R = mul(num(2), mul(num(1/3), pow(vrb(x), num(3)))).

test(sum) :-
    simplify(integ(add(pow(vrb(x), num(2)), pow(vrb(x), num(3))), vrb(x)), R),
    R = add(mul(num(1/3), pow(vrb(x), num(3))), mul(num(1/4), pow(vrb(x), num(4)))).

test(difference) :-
    simplify(integ(sub(pow(vrb(x), num(3)), pow(vrb(x), num(2))), vrb(x)), R),
    R = sub(mul(num(1/4), pow(vrb(x), num(4))), mul(num(1/3), pow(vrb(x), num(3)))).

% Additional tests
test(multiple_terms) :-
    simplify(integ(add(mul(num(3), vrb(x)), mul(num(4), pow(vrb(x), num(2)))), vrb(x)), R),
    R = add(mul(num(3/2), pow(vrb(x), num(2))), mul(num(4/3), pow(vrb(x), num(3)))).

test(sin) :-
    simplify(integ(sin(vrb(x)), vrb(x)), R),
    R = sub(num(0), cos(vrb(x))).

test(cos) :-
    simplify(integ(cos(vrb(x)), vrb(x)), R),
    R = sin(vrb(x)).

:- end_tests(integration).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% Test for Integration 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
