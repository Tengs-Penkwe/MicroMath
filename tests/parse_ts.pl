:- use_module('../src/parse.pl').


:- begin_tests(parse).

test(simple_add) :-
    phrase(expr(T), [1 , +, 2]),
    T = add(num(1), num(2)),
    !.

test(simple_sub) :-
    phrase(expr(sub(num(1), num(2))), [1, -, 2]), !.

test(simple_mul) :-
    phrase(expr(mul(num(1), num(2))), [1, *, 2]), !.

test(simple_dvd) :-
    phrase(expr(dvd(num(1), num(2))), [1, /, 2]), !.

test(simple_pow) :-
    phrase(expr(pow(num(1), num(2))), [1, ^, 2]), !.

test(nested_expr) :-
    phrase(expr(add(num(1), mul(num(2), num(3)))), [1, +, 2, *, 3]), !.

test(parenthesized_expr) :-
    phrase(expr(add(num(1), mul(num(2), num(3)))), [1, +, '(', 2, *, 3, ')']), !.

test(complex_expr) :-
    phrase(expr(mul(num(1), add(num(2), pow(num(3), num(2))))), [1, *, '(', 2, +, 3, ^, 2, ')']), !.

test(variable) :-
    phrase(expr(T), [x]),
    T = vrb(x),
    !.

test(add_variable) :-
    phrase(expr(T), [x, +, y]),
    T = add(vrb(x), vrb(y)),
    !.

test(sub_variable) :-
    phrase(expr(T), [x, -, y]),
    T = sub(vrb(x), vrb(y)),
    !.

test(mul_variable) :-
    phrase(expr(T), [x, *, y]),
    T = mul(vrb(x), vrb(y)),
    !.

test(dvd_variable) :-
    phrase(expr(T), [x, /, y]),
    T = dvd(vrb(x), vrb(y)),
    !.

test(pow_variable) :-
    phrase(expr(T), [x, ^, y]),
    T = pow(vrb(x), vrb(y)),
    !.

test(sin_variable) :-
    phrase(expr(T), [sin, '(', x, ')']),
    T = sin(vrb(x)),
    !.

test(cos_variable) :-
    phrase(expr(T), [cos, '(', x, ')']),
    T = cos(vrb(x)),
    !.

test(exp_variable) :-
    phrase(expr(T), [exp, '(', x, ')']),
    T = exp(vrb(x)),
    !.

test(complex_expression) :-
    phrase(expr(T), [sin, '(', 2, '*', x, ')', /, x, ^, 2, *, '(', x, '+', 3, ')']),
    T = dvd(sin(mul(num(2), vrb(x))), mul(pow(vrb(x), num(2)), add(vrb(x), num(3)))),
    !.

test(diff_variable) :-
    phrase(expr(T), ['D', x, '(', 2, '*', x, ')']),
    T = diff(mul(num(2), vrb(x)), vrb(x)),
    !.

test(integ_variable) :-
    phrase(expr(T), [∫, '(', 2, '*', x, ')', x]),
    T = integ(mul(num(2), vrb(x)), vrb(x)),
    !.

test(diff_simple) :-
    phrase(expr(T), ['D', x, '(', x, ')']),
    T = diff(vrb(x), vrb(x)),
    !.

test(diff_complex) :-
    phrase(expr(T), ['D', x, '(', x, ^, 2, ')']),
    T = diff(pow(vrb(x), num(2)), vrb(x)),
    !.

test(diff_nested) :-
    phrase(expr(T), ['D', x, '(', 'D', x, '(', x, ^, 3, ')', ')']),
    T = diff(diff(pow(vrb(x), num(3)), vrb(x)), vrb(x)),
    !.

test(integ_simple) :-
    phrase(expr(T), [∫, '(', x, ')', x]),
    T = integ(vrb(x), vrb(x)),
    !.

test(integ_complex) :-
    phrase(expr(T), [∫, '(', x, ^, 2, ')', x]),
    T = integ(pow(vrb(x), num(2)), vrb(x)),
    !.

test(integ_nested) :-
    phrase(expr(T), [∫, '(', ∫, '(', x, ^, 3, ')', x, ')', x]),
    T = integ(integ(pow(vrb(x), num(3)), vrb(x)), vrb(x)),
    !.

test(diff_integ) :-
    phrase(expr(T), ['D', x, '(', ∫, '(', x, ')', x, ')']),
    T = diff(integ(vrb(x), vrb(x)), vrb(x)),
    !.

test(integ_diff) :-
    phrase(expr(T), [∫, '(', 'D', x, '(', x, ')', ')', x]),
    T = integ(diff(vrb(x), vrb(x)), vrb(x)),
    !.

test(integ_diff_nested) :-
    phrase(expr(T), [∫, '(', 'D', x, '(', ∫, '(', x, ^, 3, ')', x, ')', ')', x]),
    T = integ(diff(integ(pow(vrb(x), num(3)), vrb(x)), vrb(x)), vrb(x)),
    !.

:- end_tests(parse).
