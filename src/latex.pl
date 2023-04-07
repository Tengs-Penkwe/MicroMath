% DCG rules for parsing LaTeX expressions
expr(T) --> term(T1), expr_rest(T1, T).
expr_rest(T1, T) --> "+", term(T2), expr_rest(T1+T2, T).
expr_rest(T1, T) --> "-", term(T2), expr_rest(T1-T2, T).
expr_rest(T, T) --> [].

term(T) --> factor(T1), term_rest(T1, T).
term_rest(T1, T) --> "*", factor(T2), term_rest(T1*T2, T).
term_rest(T1, T) --> "/", factor(T2), term_rest(T1/T2, T).
term_rest(T, T) --> [].

factor(sin(E)) --> "sin", "(", expr(E), ")".
factor(cos(E)) --> "cos", "(", expr(E), ")".
factor(exp(E)) --> "exp", "(", expr(E), ")".
factor(-E) --> "-", factor(E).
factor(E^N) --> "(", expr(E), ")", "^", num(N).
factor(E) --> "(", expr(E), ")".
factor(X) --> var(X).
factor(N) --> num(N).

num(N) --> [D], {digit(D), atom_number(D, N)}.
var(X) --> [X], {atom(X), char_type(X, alpha)}.

digit(D) :- member(D, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']).

% Helper predicate to parse LaTeX string into Prolog expression
parse_latex(Latex, Expr) :-
  atom_chars(Latex, Chars),
  phrase(expr(Expr), Chars).

% Example usage
% ?- parse_latex('x^2+x', Expr), dv(Expr, x, Result).
% Expr = x^2+x,
% Result = 1*x+1*1+0.

