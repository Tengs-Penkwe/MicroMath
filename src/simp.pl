:- module(simp, [simplify/2]).

% Base case: numbers and single variables are already simplified.
simplify(num(N), num(N)).
simplify(vrb(X), vrb(X)).

% Simplify addition, subtraction, multiplication, and division
simplify(add(E1, E2), R) :- simplify_operation(E1, E2, add, R).
simplify(sub(E1, E2), R) :- simplify_operation(E1, E2, sub, R).
simplify(mul(E1, E2), R) :- simplify_operation(E1, E2, mul, R).
simplify(dvd(E1, E2), R) :- simplify_operation(E1, E2, dvd, R).
simplify(pow(E1, E2), R) :- simplify_operation(E1, E2, pow, R).

simplify(integ(Expr, Var), R) :- simplify_operation(Expr, integ, Var, R).
simplify(diff(Expr, Var), R)  :- simplify_operation(Expr, diff, Var, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rules for Arithmetic Operation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  simplify_operation(E1, E2, Op, R) :-
    simplify(E1, S1),
    simplify(E2, S2),
    (S1 = num(N1), S2 = num(N2) ->
      arithmetic_op(Op, N1, N2, N3),
      R = num(N3)
    ;
      R =.. [Op, S1, S2]
    ).

arithmetic_op(add, N1, N2, N3) :- N3 is N1 + N2.
arithmetic_op(sub, N1, N2, N3) :- N3 is N1 - N2.
arithmetic_op(mul, N1, N2, N3) :- N3 is N1 * N2.
arithmetic_op(dvd, N1, N2, N3) :- N3 is N1 / N2.
arithmetic_op(pow, N1, N2, N3) :- N3 is N1 ^ N2.

% operation_expr(+, S1, S2, add(S1, S2)).
% operation_expr(-, S1, S2, sub(S1, S2)).
% operation_expr(*, S1, S2, mul(S1, S2)).
% operation_expr(/, S1, S2, dvd(S1, S2)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rules for Integration 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
simplify_operation(Expr, Op, Var, R) :-
  Op = integ,
  simplify(Expr, S),
  (S = num(N) -> 
    R = mul(num(N), Var)
  ;
    R =.. [Op, S, Var]
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rules for Differentiation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
simplify_operation(Expr, Op, Var, R) :-
  Op = diff,
  simplify(Expr, S),
  (S = num(N) -> 
    R = num(0)
  ;
    R =.. [Op, S, Var]
  ).
