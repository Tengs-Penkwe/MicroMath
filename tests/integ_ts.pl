:- use_module('../src/integ', [integ/3]).

:- begin_tests(integration).

test(constant) :-
    integ(3, x, 0).

test(variable) :-
    integ(x, x, 0.5 * x^2).

test(power) :-
    integ(x^3, x, 0.25 * x^4).

test(exponential) :-
  integ(exp(X), X, exp(X)).

test(logarithmic) :-
    integ(ln(x), x, x * (ln(x) - 1)).

test(constant_times_function) :-
    integ(2 * (x^2), x, 2 * (x^3 / 3)).

test(sum) :-
    integ(x^2 + x^3, x, (1/3 * x^3) + (1/4 * x^4)).

test(difference) :-
    integ(x^3 - x^2, x, (1/4 * x^4) - (1/3 * x^3)).

:- end_tests(integration).

