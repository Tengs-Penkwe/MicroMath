:- module(simp, [simplify/2]).
% ALARM: we may need neg()
:- discontiguous simplify_operation/4.

% Base case: numbers and single variables are already simplified.
simplify(num(N), num(N)).
simplify(vrb(X), vrb(X)).

% Simplify addition, subtraction, multiplication, and division
simplify(add(E1, E2), R) :- simplify_operation(E1, E2, add, R).
simplify(sub(E1, E2), R) :- simplify_operation(E1, E2, sub, R).
simplify(mul(E1, E2), R) :- simplify_operation(E1, E2, mul, R).
simplify(dvd(E1, E2), R) :- simplify_operation(E1, E2, dvd, R).
simplify(pow(E1, E2), R) :- simplify_operation(E1, E2, pow, R).

%
simplify(integ(Expr, Var), R) :- simplify_operation(Expr, integ, Var, R).
simplify(diff(Expr, Var), R)  :- simplify_operation(Expr, diff, Var, R).

% Simplify cos, sin, and exp
simplify(cos(Expr), R) :- simplify_trig(Expr, cos, R).
simplify(sin(Expr), R) :- simplify_trig(Expr, sin, R).
simplify(exp(Expr), R) :- simplify_exp(Expr, R).

% Simplify the inner expression for cos and sin, then apply specific rules
simplify_trig(Expr, Trig, R) :-
    simplify(Expr, SimplifiedExpr),
    (SimplifiedExpr = num(0) ->
        (Trig = cos -> R = num(1); R = num(0))
    ;
        R =.. [Trig, SimplifiedExpr]
    ).

% Simplify the inner expression for exp, then apply specific rules
simplify_exp(Expr, R) :-
    simplify(Expr, SimplifiedExpr),
    (SimplifiedExpr = num(0) ->
        R = num(1)
    ;
        R = exp(SimplifiedExpr)
    ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rules for Arithmetic Operation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
simplify_operation(E1, E2, Op, R) :-
  simplify(E1, S1),
  simplify(E2, S2),
  (S1 = num(N1), S2 = num(N2) ->
    arithmetic_op(Op, N1, N2, N3),
    R = num(N3), !
  ;
    operation_expr(Op, S1, S2, S3),
    R = S3, !
    % R =.. [Op, S1, S2]
  ).

arithmetic_op(add, N1, N2, N3) :- N3 is N1 + N2.
arithmetic_op(sub, N1, N2, N3) :- N3 is N1 - N2.
arithmetic_op(mul, N1, N2, N3) :- N3 is N1 * N2.
arithmetic_op(dvd, N1, N2, N3) :- N3 is N1 / N2.
arithmetic_op(pow, N1, N2, N3) :- N3 is N1 ^ N2.

operation_expr(add, S1, S2, S3) :-
  (S1 = num(0) ->
    S3 = S2, !
  ;S2 = num(0) ->
    S3 = S1, !
  ;
    S3 = add(S1, S2)
  ).
operation_expr(sub, S1, S2, S3) :-
  (
    % S1 = num(0) ->
    % S3 = S2
    %   ALARM, TODO: add neg to AST
  S2 = num(0) ->
    S3 = S1, !
  ;
    S3 = sub(S1, S2)
  ).
operation_expr(mul, S1, S2, S3) :-
  (S1 = num(0) ->
      S3 = num(0), !
  ;S1 = num(1) ->
      S3 = S2, !
  ;S2 = num(0) ->
      S3 = num(0), !
  ;S2 = num(1) ->
      S3 = S1, !
  ;S1 = vrb(X), S2 = vrb(X) ->
      S3 = pow(vrb(X), num(2))
  ;
  (S1 = vrb(X), S2 = pow(vrb(X), E) -> 
      simplify(add(E, num(1)), SS)
  ;S1 = pow(vrb(X), E), S2 = vrb(X) -> 
      simplify(add(E, num(1)), SS)
  ),
      S3 = pow(vrb(X), SS)
  ;S1 = pow(vrb(X), E1), S2 = pow(vrb(X), E2) ->
      simplify(add(E1, E2), SS),
      S3 = pow(vrb(X), SS)
  ;
  S3 = mul(S1, S2)
  ).
operation_expr(dvd, S1, S2, S3) :-
  (
    S1 = num(0) ->
    S3 = num(0), !
    %   ALARM, TODO: divid zero error
  % S2 = num(0) ->
  %   S3 = S1
  ;S2 = num(1) ->
      S3 = S1, !
  ;S1 = vrb(X), S2 = vrb(X) ->
      S3 = num(1), !
  ;
  (S1 = vrb(X), S2 = pow(vrb(X), E) -> 
      simplify(sub(num(1), E), SS)
  ;S1 = pow(vrb(X), E), S2 = vrb(X) -> 
      simplify(sub(E, num(1)), SS)
  ),
      S3 = pow(vrb(X), SS), !
  ;S1 = pow(vrb(X), E1), S2 = pow(vrb(X), E2) ->
      simplify(sub(E1, E2), SS),
      S3 = pow(vrb(X), SS), !
  ;
    S3 = dvd(S1, S2)
  ).
operation_expr(pow, S1, S2, S3) :-
  (
   S1 = num(0) ->
      S3 = num(0)
  ;S1 = num(1) ->
      S3 = num(1)
  ;S2 = num(0) ->
      S3 = num(1)
  ;S2 = num(1) ->
      S3 = S1
  ;S1 = pow(vrb(X), E1)  ->
      simplify(mul(E1, S2), SS),
      S3 = pow(vrb(X), SS), !
  ;
    S3 = pow(S1, S2)
  ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rules for Integration 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
simplify_operation(Expr, Op, Var, R) :-
  Op = integ,
  simplify(Expr, S),
  (S = num(N) -> 
    R = mul(num(N), Var)
  ; S = vrb(X), Var = vrb(X) ->
    R = mul(num(0.5), pow(vrb(X), num(2)))
  ; S = pow(vrb(X), num(N)) ->
    R = mul(dvd(num(1), num(N)), pow(vrb(X), add(num(N), num(1)))),
    R =.. [Op, S, Var]
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rules for Differentiation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
simplify_operation(Expr, Op, Var, R) :-
  Op = diff,
  simplify(Expr, S),
  (S = num(_), Var = vrb(_) -> 
    R = num(0), !
  ; S = vrb(X), Var = vrb(Y),
    ( X = Y -> R = num(1), !
    ; 
      R = num(0), !
    )
  ; S = sin(E) ->
    simplify(diff(E, Var), D),
    simplify(mul(D, cos(E)), SS),
    R = SS
  ; S = cos(E) ->
    simplify(diff(E, Var), D),
    simplify(mul(D, sub(num(0), sin(E))), SS),
    R = SS
  ; S = exp(E) ->
    simplify(diff(E, Var), D),
    simplify(mul(D, exp(E)), SS),
    R = SS
  ; S = add(E1, E2) ->
    simplify(diff(E1, Var), S1),
    simplify(diff(E2, Var), S2),
    simplify(add(S1, S2), SS),
    R = SS
  ; S = sub(E1, E2) -> 
    simplify(diff(E1, Var), S1),
    simplify(diff(E2, Var), S2),
    simplify(sub(S1, S2), SS),
    R = SS
  ; S = mul(E1, E2) ->
    simplify(diff(E1, Var), D1),
    simplify(diff(E2, Var), D2),
    simplify(add(mul(D1, E2), mul(E1, D2)), SS),
    R = SS
  ; S = dvd(E1, E2) ->
    simplify(diff(E1, Var), D1),
    simplify(diff(E2, Var), D2),
    simplify(dvd(sub(mul(E2, D1), mul(E1, D2)), mul(E2, E2)), SS),
    % IDEA: why mul(E2, E2), not pow(E2, num(2))
    R = SS
  ; S = pow(E1, num(N)) ->
  % ALARM: there maybe some proble with base is constant
    simplify(mul(num(N), pow(E1, sub(num(N), num(1)))), SS),
    R = SS
  ;
    R =.. [Op, S, Var]
  ).
