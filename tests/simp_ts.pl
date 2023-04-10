:- use_module('../src/simp.pl').

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

test(multi_variable)
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

% ?- simplify(integ(add(num(3), mul(num(2), variable(x))), variable(x)), R).
% R = add(add(mul(num(3), variable(x)), mul(num(1), pow(variable(x), num(2)))), const(C)).

% ?- simplify(diff(add(num(3), mul(num(2), variable(x))), variable(x)), R).
% R = num(2).

:- end_tests(simplify).

