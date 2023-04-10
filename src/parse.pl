:- module(parse, [parse_expression/2, expr/3]).

% :- use_module(library(dcg/basics)).

parse_expression(Expression, AST) :-
    ato(Expression, Codes),
    phrase(expr(AST), Codes).

parse_string(String, Result) :-
    atom_codes(String, Codes),
    tokenize(Codes, Tokens),
    phrase(expr(Result), Tokens).

tokenize([], []).
tokenize([Code | RestCodes], [Token | RestTokens]) :-
    ( char_type(Code, digit) ->
        number_codes(Token, [Code])
    ; char_type(Code, alpha) ->
        Token = Code
    ; atom_codes(Token, [Code])
    ),
    tokenize(RestCodes, RestTokens).

expr(       T        ) --> term(T).
expr(       add(X,Y) ) --> term(X), [+], expr(Y).
expr(       sub(X,Y) ) --> term(X), [-], expr(Y).

term(       T        ) --> factor(T).
term(       mul(X,Y) ) --> factor(X), [*], term(Y).
term(       dvd(X,Y) ) --> factor(X), [/], term(Y).

factor(     F        ) --> base(F).
factor(     pow(X,Y) ) --> base(X), [^], factor(Y).

% base(       ID       ) --> identifier(ID).
base(       num(N)   ) --> is_number(N).
base(       vrb(X)   ) --> is_variable(X).
base(     diff(E, X) ) --> ['D'], base(X), ['('], expr(E), [')'].
base(    integ(E, X) ) --> ['∫'], ['('], expr(E), [')'], base(X).
base(       sin(E)   ) --> [sin], ['('], expr(E), [')'].
base(       cos(E)   ) --> [cos], ['('], expr(E), [')'].
base(       exp(E)   ) --> [exp], ['('], expr(E), [')'].

base(       T        ) --> ['('], expr(T), [')'].

identifier( ID       ) --> [X], { id(X), ID = X }.

is_number(  N        ) --> [N], { number(N)}.
% is_integer( N        ) --> [X], { number(N), integer(N) }.
is_variable( V       ) --> [X], { atom(X), V = X }.


is_alpha(C) :-
  C >= 0'a, C =< 0'z;
  C >= 0'A, C =< 0'Z.

% id(X) :-
%   C >= 0'a, C =< 0'z.


id(a).
id(b).
id(c).
id(d).
id(e).
id(f).
id(g).
id(h).
id(i).
id(j).
id(k).
id(l).
id(m).
id(n).
id(o).
id(p).
id(q).
id(r).
id(s).
id(t).
id(u).
id(v).
id(w).
id(x).
id(y).
id(z).



