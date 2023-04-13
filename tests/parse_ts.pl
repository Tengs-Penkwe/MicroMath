:- use_module('../src/parse.pl').

:- begin_tests(parse).

test(simple_add) :-
    phrase(expr(T), [num(1), add, num(2)]),
    T = add(num(1), num(2)),!.

test(simple_sub) :-
    phrase(expr(sub(num(1), num(2))), [num(1), sub, num(2)]),!.

test(simple_mul) :-
    phrase(expr(mul(num(1), num(2))), [num(1), mul, num(2)]),!.

test(simple_dvd) :-
    phrase(expr(dvd(num(1), num(2))), [num(1), dvd, num(2)]),!.

test(simple_pow) :-
    phrase(expr(pow(num(1), num(2))), [num(1), pow, num(2)]),!.

test(nested_expr) :-
    phrase(expr(add(num(1), mul(num(2), num(3)))), [num(1), add, num(2), mul, num(3)]),!.

test(parenthesized_expr) :-
    phrase(expr(add(num(1), mul(num(2), num(3)))), [num(1), add, lparen, num(2), mul, num(3), rparen]),!.

test(complex_expr) :-
    phrase(expr(mul(num(1), add(num(2), pow(num(3), num(2))))), [num(1), mul, lparen, num(2), add, num(3), pow, num(2), rparen]),!.

test(variable) :-
    phrase(expr(T), [vrb(x)]),
    T = vrb(x),!.

test(add_variable) :-
    phrase(expr(T), [vrb(x), add, vrb(y)]),
    T = add(vrb(x), vrb(y)),!.

test(sub_variable) :-
    phrase(expr(T), [vrb(x), sub, vrb(y)]),
    T = sub(vrb(x), vrb(y)),!.

test(mul_variable) :-
    phrase(expr(T), [vrb(x), mul, vrb(y)]),
    T = mul(vrb(x), vrb(y)),!.

test(dvd_variable) :-
    phrase(expr(T), [vrb(x), dvd, vrb(y)]),
    T = dvd(vrb(x), vrb(y)),!.

test(pow_variable) :-
    phrase(expr(T), [vrb(x), pow, vrb(y)]),
    T = pow(vrb(x), vrb(y)),!.

test(sin_variable) :-
    phrase(expr(T), [sin, lparen, vrb(x), rparen]),
    T = sin(vrb(x)),!.

test(cos_variable) :-
    phrase(expr(T), [cos, lparen, vrb(x), rparen]),
    T = cos(vrb(x)),!.

test(exp_variable) :-
    phrase(expr(T), [exp, lparen, vrb(x), rparen]),
    T = exp(vrb(x)),!.

test(complex_expression) :-
    phrase(expr(T), [sin, lparen, num(2), mul, vrb(x), rparen, dvd, vrb(x), pow, num(2), mul, lparen, vrb(x), add, num(3), rparen]),
    T = dvd(sin(mul(num(2), vrb(x))), mul(pow(vrb(x), num(2)), add(vrb(x), num(3)))),!.

test(diff_variable) :-
    phrase(expr(T), [diff, vrb(x), lparen, num(2), mul, vrb(x), rparen]),
    T = diff(mul(num(2), vrb(x)), vrb(x)),!.

test(integ_variable) :-
    phrase(expr(T), [integ, lparen, num(2), mul, vrb(x), rparen, vrb(x)]),
    T = integ(mul(num(2), vrb(x)), vrb(x)),!.

test(diff_simple) :-
    phrase(expr(T), [diff, vrb(x), lparen, vrb(x), rparen]),
    T = diff(vrb(x), vrb(x)),!.

test(diff_complex) :-
    phrase(expr(T), [diff, vrb(x), lparen, vrb(x), pow, num(2), rparen]),
    T = diff(pow(vrb(x), num(2)), vrb(x)),!.

test(diff_nested) :-
    phrase(expr(T), [diff, vrb(x), lparen, diff, vrb(x), lparen, vrb(x), pow, num(3), rparen, rparen]),
    T = diff(diff(pow(vrb(x), num(3)), vrb(x)), vrb(x)),!.

test(integ_simple) :-
    phrase(expr(T), [integ, lparen, vrb(x), rparen, vrb(x)]),
    T = integ(vrb(x), vrb(x)),!.

test(integ_complex) :-
    phrase(expr(T), [integ, lparen, vrb(x), pow, num(2), rparen, vrb(x)]),
    T = integ(pow(vrb(x), num(2)), vrb(x)),!.

test(integ_nested) :-
    phrase(expr(T), [integ, lparen, integ, lparen, vrb(x), pow, num(3), rparen, vrb(x), rparen, vrb(x)]),
    T = integ(integ(pow(vrb(x), num(3)), vrb(x)), vrb(x)),!.

test(diff_integ) :-
    phrase(expr(T), [diff, vrb(x), lparen, integ, lparen, vrb(x), rparen, vrb(x), rparen]),
    T = diff(integ(vrb(x), vrb(x)), vrb(x)),!.

test(integ_diff) :-
    phrase(expr(T), [integ, lparen, diff, vrb(x), lparen, vrb(x), rparen, rparen, vrb(x)]),
    T = integ(diff(vrb(x), vrb(x)), vrb(x)),!.

test(integ_diff_nested) :-
    phrase(expr(T), [integ, lparen, diff, vrb(x), lparen, integ, lparen, vrb(x), pow, num(3), rparen, vrb(x), rparen, rparen, vrb(x)]),
    T = integ(diff(integ(pow(vrb(x), num(3)), vrb(x)), vrb(x)), vrb(x)),!.

:- end_tests(parse).

:- begin_tests(parse_complex).

test(complex_expression_2) :-
    phrase(expr(T), [num(1), dvd, vrb(x), add, sin, lparen, vrb(x), rparen]),
    T = add(dvd(num(1), vrb(x)), sin(vrb(x))),!.

test(complex_expression_3) :-
    phrase(expr(T), [num(2), mul, vrb(x), pow, num(2), add, num(3), mul, vrb(x)]),
    T = add(mul(num(2), pow(vrb(x), num(2))), mul(num(3), vrb(x))),!.

test(complex_expression_4) :-
    phrase(expr(T), [num(4), mul, vrb(x), pow, num(2), dvd, lparen, num(2), add, vrb(x), rparen]),
    T = mul(num(4), dvd(pow(vrb(x), num(2)), add(num(2), vrb(x)))),!.

test(complex_expression_5) :-
    phrase(expr(T), [sin, lparen, vrb(x), add, num(1), rparen, mul, cos, lparen, vrb(x), sub, num(1), rparen]),
    T = mul(sin(add(vrb(x), num(1))), cos(sub(vrb(x), num(1)))),!.

test(complex_expression_6) :-
    phrase(expr(T), [exp, lparen, num(2), mul, lparen, vrb(x), add, num(3), rparen, rparen]),
    T = exp(mul(num(2), add(vrb(x), num(3)))),!.

test(complex_expression_7) :-
    phrase(expr(T), [lparen, num(1), dvd, lparen, vrb(x), add, num(1), rparen, rparen, pow, num(3)]),
    T = pow(dvd(num(1), add(vrb(x), num(1))), num(3)),!.

test(complex_expression_8) :-
    phrase(expr(T), [diff, vrb(x), lparen, vrb(x), pow, num(2), add, num(3), mul, vrb(x), rparen]),
    T = diff(add(pow(vrb(x), num(2)), mul(num(3), vrb(x))), vrb(x)),!.

test(complex_expression_9) :-
    phrase(expr(T), [integ, lparen, sin, lparen, vrb(x), rparen, mul, cos, lparen, vrb(x), rparen, rparen, vrb(x)]),
    T = integ(mul(sin(vrb(x)), cos(vrb(x))), vrb(x)),!.

test(complex_expression_10) :-
    phrase(expr(T), [diff, vrb(x), lparen, exp, lparen, vrb(x), mul, vrb(x), pow, num(2), rparen, rparen]),
    T = diff(exp(mul(vrb(x), pow(vrb(x), num(2)))), vrb(x)),!.

test(complex_expression_11) :-
    phrase(expr(T), [integ, lparen, num(2), mul, vrb(x), pow, num(2), add, num(3), mul, vrb(x), rparen, vrb(x)]),
    T = integ(add(mul(num(2), pow(vrb(x), num(2))), mul(num(3), vrb(x))), vrb(x)),!.

:- end_tests(parse_complex).

:- begin_tests(tokenizer_and_parser).
% Test basic arithmetic operators
test(addition) :-
    parse_expression("1+2", add(num(1), num(2))),!.

test(subtraction) :-
    parse_expression("1-2", sub(num(1), num(2))),!.

test(multiplication) :-
    parse_expression("1*2", mul(num(1), num(2))),!.

test(division) :-
    parse_expression("1/2", dvd(num(1), num(2))),!.

test(power) :-
    parse_expression("1^2", pow(num(1), num(2))),!.

% Test functions
test(sine) :-
    parse_expression("sin(x)", sin(vrb(x))),!.

test(cosine) :-
    parse_expression("cos(x)", cos(vrb(x))),!.

test(exponential) :-
    parse_expression("exp(x)", exp(vrb(x))),!.

% Test variables
test(variable) :-
    parse_expression("x", vrb(x)),!.

% Test complex expressions
test(complex_expression_1) :-
    parse_expression("sin(x)+cos(x)", add(sin(vrb(x)), cos(vrb(x)))),!.

test(complex_expression_2) :-
    parse_expression("3*x^2", mul(num(3), pow(vrb(x), num(2)))),!.

test(complex_expression_3) :-
    parse_expression("x/(2+sin(y))", dvd(vrb(x), add(num(2), sin(vrb(y))))),!.

% Test nested functions
test(nested_functions) :-
    parse_expression("sin(cos(x))", sin(cos(vrb(x)))),!.

test(nested_functions_2) :-
    parse_expression("exp(sin(x)+cos(x))", exp(add(sin(vrb(x)), cos(vrb(x))))),!.

% Test expressions with multiple variables
test(multiple_variables) :-
    parse_expression("x+y", add(vrb(x), vrb(y))),!.

test(multiple_variables_2) :-
    parse_expression("x*y+z", add(mul(vrb(x), vrb(y)), vrb(z))),!.

% Test expressions with parentheses
test(parentheses) :-
    parse_expression("(x+y)*z", mul(add(vrb(x), vrb(y)), vrb(z))),!.

test(parentheses_2) :-
    parse_expression("x^(y+z)", pow(vrb(x), add(vrb(y), vrb(z)))),!.

% Test expressions with multiple operators
test(multiple_operators) :-
    parse_expression("x+y*z", add(vrb(x), mul(vrb(y), vrb(z)))),!.

test(multiple_operators_2) :-
    parse_expression("x*y+z^2", add(mul(vrb(x), vrb(y)), pow(vrb(z), num(2)))),!.

% Test expressions with constants
test(constants) :-
    parse_expression("2*x+3", add(mul(num(2), vrb(x)), num(3))),!.

test(constants_2) :-
    parse_expression("x^2+2*x+1", add(pow(vrb(x), num(2)), add(mul(num(2), vrb(x)), num(1)))),!.

  % Test decimals
test(decimal_number) :-
    parse_expression("1.23+2.34", add(num(1.23), num(2.34))),!.

% Test nested parentheses
test(nested_parentheses) :-
    parse_expression("(x + (y - z))", add(vrb(x), sub(vrb(y), vrb(z)))),!.

% % Test multi-letter variables
% test(multi_letter_variable) :-
%     parse_expression("value1 + value2", add(vrb(value1), vrb(value2))),!.

% Test a more complex expression with parentheses
test(complex_expression_parentheses) :-
    parse_expression("(x+y)*(z+(w-2))", mul(add(vrb(x), vrb(y)), add(vrb(z), sub(vrb(w), num(2))))),!.

% Test complex expression with functions, variables, and decimals
test(complex_expression_functions_variables_decimals) :-
    parse_expression("sin(0.5*x)+cos(1.5*y)", add(sin(mul(num(0.5), vrb(x))), cos(mul(num(1.5), vrb(y))))),!.

% Test complex expressions with functions and multiple variables
test(complex_expression_with_functions) :-
    parse_expression("sin(x*y)+cos(y*z)", add(sin(mul(vrb(x), vrb(y))), cos(mul(vrb(y), vrb(z))))),!.

test(complex_expression_with_functions_2) :-
    parse_expression("exp(x+y)*sin(z)", mul(exp(add(vrb(x), vrb(y))), sin(vrb(z)))),!.

:- end_tests(tokenizer_and_parser).

