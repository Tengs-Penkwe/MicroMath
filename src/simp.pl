:- module(simp, [simp/2]).

simp(X, X) :- atomic(X).

simp(A+B, V) :- 
  simp(A, VA),
  simp(B, VB),
  simp_vals(VA+VB, V).

simp_vals(0 + V, V) :- !.
simp_vals(V + 0, V) :- !.
simp_vals(V - 0, V) :- !.
simp_vals(0 - V, -V) :- !.
simp_vals(-(0), 0) :- !.
simp_vals(-(-X), X) :- !.

simp_vals(A+B, AB) :-
  number(A), number(B), !,
  AB is A+B.

simp_vals(A-B, AB) :-
  number(A), number(B), !,
  AB is A-B.

