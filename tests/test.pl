:- use_module('../src/simp.pl').
:- use_module('../src/parse.pl').

:- begin_tests(composed_test).

% Test multiplication simplification
test(simple_mul_simplify) :-
    phrase(expr(T), [4, *, 2]),
    T = mul(num(4), num(2)),
    simplify(T, R),
    R = num(8),
    !.

% Test division simplification
test(simple_dvd_simplify) :-
    phrase(expr(T), [9, /, 3]),
    T = dvd(num(9), num(3)),
    simplify(T, R),
    R = num(3),
    !.

% Test nested expression simplification
test(nested_expr_simplify) :-
    phrase(expr(T), [1, +, 2, *, 3, -, 4, /, 2]),
    T = sub(add(num(1), mul(num(2), num(3))), dvd(num(4), num(2))),
    simplify(T, R),
    R = sub(add(num(1), num(6)), num(2)),
    !.

% Test integration simplification
test(integ_simple) :-
    simplify(integ(num(5), x), R),
    R = mul(num(5), x),
    !.

% Test differentiation of a more complex expression
test(diff_expr) :-
    phrase(expr(T), [x, *, x, +, 1]),
    T = add(mul(x, x), num(1)),
    simplify(diff(T, x), R),
    R = add(mul(num(2), x), num(0)),
    !.

% Test integration of a more complex expression
test(integ_expr) :-
    phrase(expr(T), [x, *, x, +, 1]),
    T = add(mul(x, x), num(1)),
    simplify(integ(T, x), R),
    R = add(dvd(mul(num(1), pow(x, num(3))), num(3)), mul(x, num(1))),
    !.

:- end_tests(composed_test).
