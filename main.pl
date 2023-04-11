% :- module(main, [run/0]).

% :- asserta(library_directory('.')).
% :- asserta(library_directory('./src')).
% % Expression Simplication
:- use_module('./src/parse.pl').
:- use_module('./src/simp.pl').
:- use_module('./src/convert.pl').

micro_math(Expression, Result) :-
  parse_expression(Expression, AST),
  simplify(AST, SimplifiedAST),
  ast_to_string(SimplifiedAST, Result).
