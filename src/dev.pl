:- module(dev, [dv/3]).

dv(X, X, 1) :- var(X), !.

% dv(C, X, 0) :-
dv(C, _, 0) :- atomic(C), !.
%   atomic(C),
%   C \= X,
%   !.

% Addition & Subtraction
dv(A+B, X, DA+DB) :- 
  dv(A, X, DA),
  dv(B, X, DB).
dv(A-B, X, DA-DB) :-
  dv(A, X, DA),
  dv(B, X, DB).
dv(-A,  X, -DA)   :-
  -dv(A, X, DA).

% Multiplication & Division
dv(A*B, X, DA*B + DB*A) 
  :- dv(A, X, DA), dv(B, X, DB).
dv(F/G, X, (DF*G+DG*F)/G^2)
  :- dv(F, X, DF), dv(G, X, DG).

% Special Function
dv(sin(E), X, cos(E)*DE)
  :- dv(E, X, DE).
dv(cos(E), X, -sin(E)*DE)
  :- dv(E, X, DE).
dv(exp(E), X, exp(E)*DE)
  :- dv(E, X, DE).

% dv(X^n, X, n*X^(n-1))
