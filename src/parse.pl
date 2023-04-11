:- module(parse, [parse_expression/2, expr/3]).

:- use_module('../src/token.pl').

parse_expression(Expression, AST) :-
    tokenize(Expression, Tokens),
    phrase(expr(AST), Tokens).

expr(       T        ) --> term(T).
expr(       add(X,Y) ) --> term(X), [add], expr(Y).
expr(       sub(X,Y) ) --> term(X), [sub], expr(Y).

term(       T        ) --> factor(T).
term(       mul(X,Y) ) --> factor(X), [mul], term(Y).
term(       dvd(X,Y) ) --> factor(X), [dvd], term(Y).

factor(     F        ) --> base(F).
factor(     pow(X,Y) ) --> base(X), [pow], factor(Y).

base(       num(N)   ) --> [num(N)].
base(       vrb(X)   ) --> [vrb(X)].
base( diff(E, vrb(X))) --> [diff],  [vrb(X)], [lparen], expr(E), [rparen].
base(integ(E, vrb(X))) --> [integ], [lparen], expr(E), [rparen], [vrb(X)].
base(       sin(E)   ) --> [sin], [lparen], expr(E), [rparen].
base(       cos(E)   ) --> [cos], [lparen], expr(E), [rparen].
base(       exp(E)   ) --> [exp], [lparen], expr(E), [rparen].

base(       T        ) --> [lparen], expr(T), [rparen].

