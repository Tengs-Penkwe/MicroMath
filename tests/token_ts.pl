:- use_module('../src/token.pl'),!.

:- begin_tests(tokenizer).

% Test basic arithmetic operators
test(addition) :-
    tokenize("1+2", [num(1), add, num(2)]),!.

test(subtraction) :-
    tokenize("1-2", [num(1), sub, num(2)]),!.

test(multiplication) :-
    tokenize("1*2", [num(1), mul, num(2)]),!.

test(division) :-
    tokenize("1/2", [num(1), dvd, num(2)]),!.

test(power) :-
    tokenize("1^2", [num(1), pow, num(2)]),!.

% Test variables
test(variable) :-
    tokenize("x", [vrb(x)]),!.

test(addition) :-
    tokenize("x + y", [vrb(x), add, vrb(y)]),!.

test(subtraction) :-
    tokenize("x - y", [vrb(x), sub, vrb(y)]),!.

test(multiplication) :-
    tokenize("x * y", [vrb(x), mul, vrb(y)]),!.

test(division) :-
    tokenize("x / y", [vrb(x), dvd, vrb(y)]),!.

test(power) :-
    tokenize("x ^ 2", [vrb(x), pow, num(2)]),!.

test(sin) :-
    tokenize("sin(x)", [sin, lparen, vrb(x), rparen]),!.

test(cos) :-
    tokenize("cos(x)", [cos, lparen, vrb(x), rparen]),!.

test(exp) :-
    tokenize("exp(x)", [exp, lparen, vrb(x), rparen]),!.

% Test complex expressions
test(complex_expression_1) :-
    tokenize("sin(x)+cos(x)", [sin, lparen, vrb(x), rparen, add, cos, lparen, vrb(x), rparen]),!.

test(complex_expression_2) :-
    tokenize("3*x^2", [num(3), mul, vrb(x), pow, num(2)]),!.

test(complex_expression_3) :-
    tokenize("x/(2+sin(y))", [vrb(x), dvd, lparen, num(2), add, sin, lparen, vrb(y), rparen, rparen]),!.

% Test white spaces
test(white_spaces) :-
    tokenize("  1  +  2  ", [num(1), add, num(2)]),!.

test(white_spaces_function) :-
    tokenize(" sin ( x ) ", [sin, lparen, vrb(x), rparen]),!.

test(decimal1) :-
    tokenize("1.3", [num(1.3)]),!.

test(decimal2) :-
    tokenize("123.997 + 134.1123", [num(123.997), add, num(134.1123)]),!.

% test(variables) :-
%     tokenize("a1 + h2 - z00", [vrb(a1), add, vrb(h2), sub, vrb(z00)]),!.

:- end_tests(tokenizer).

