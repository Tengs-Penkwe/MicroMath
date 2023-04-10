:- module(convert, [ast_to_string/2]).

ast_to_string(num(N), S) :- atom_number(S, N).
ast_to_string(vrb(X), X).

ast_to_string(add(E1, E2), S) :- ast_to_string_binop(E1, E2, add, S).
ast_to_string(sub(E1, E2), S) :- ast_to_string_binop(E1, E2, sub, S).
ast_to_string(mul(E1, E2), S) :- ast_to_string_binop(E1, E2, mul, S).
ast_to_string(dvd(E1, E2), S) :- ast_to_string_binop(E1, E2, dvd, S).
ast_to_string(pow(E1, E2), S) :- ast_to_string_binop(E1, E2, pow, S).

ast_to_string(sin(E), S) :- ast_to_string(E, S1), atomic_list_concat(['sin(', S1, ')'], S).
ast_to_string(cos(E), S) :- ast_to_string(E, S1), atomic_list_concat(['cos(', S1, ')'], S).
ast_to_string(exp(E), S) :- ast_to_string(E, S1), atomic_list_concat(['exp(', S1, ')'], S).
ast_to_string(ln(E), S)  :- ast_to_string(E, S1), atomic_list_concat(['ln(', S1, ')'], S).


ast_to_string(diff(E, X), S) :-
    ast_to_string(E, S1),
    ast_to_string(X, S2),
    atomic_list_concat(['D', S2, '(', S1, ')'], S).

ast_to_string(integ(E, X), S) :-
    ast_to_string(E, S1),
    ast_to_string(X, S2),
    atomic_list_concat(['∫', '(', S1, ')', S2], S).

% Helper predicate for binary operations
ast_to_string_binop(E1, E2, Op, S) :-
    ast_to_string(E1, S1),
    ast_to_string(E2, S2),
    operator_string(Op, Opstr),
    (needs_paren(E1, Op, left)  -> (P1 = '(', P2 = ')') ; (P1 = '', P2 = '')),
    (needs_paren(E2, Op, right) -> (P3 = '(', P4 = ')') ; (P3 = '', P4 = '')),
    atomic_list_concat([P1, S1, P2, Opstr, P3, S2, P4], S).

operator_string(add, '+').
operator_string(sub, '-').
operator_string(mul, "*").
operator_string(dvd, "/").
operator_string(pow, "^"). 

needs_paren(add(_, _), Op, _) :- member(Op, [mul, dvd, pow]).
needs_paren(sub(_, _), Op, _) :- member(Op, [sub, mul, dvd, pow]).
needs_paren(dvd(_, _), Op, _) :- member(Op, [dvd, pow]).
needs_paren(mul(_, _), Op, right) :- member(Op, [dvd, pow]).
needs_paren(mul(_, _), Op, left)  :- member(Op, [pow]).
needs_paren(_, _) :- false.
