# MicroMath

MicroMath is a simple symbolic math library written in Prolog. It provides basic functionality for parsing, simplifying, and performing symbolic differentiation and integration.

## Features

- Expression parsing
- Expression simplification
- Symbolic differentiation
- Symbolic integration
- Conversion to string

## How to run tests
To run the tests in your Prolog project, you can do the following steps:

First, make sure you are in the root directory of your project.
Then, start the SWI-Prolog interpreter by typing swipl in the terminal.
Load the main.pl file and the test files by entering the following commands in the SWI-Prolog interpreter:
```bash
?- [main].
?- [tests/parse_ts, tests/simplify_ts].
```
Finally, run the tests using the `run_tests` predicate:
```bash
?- run_tests.
```
This command will execute all the tests in the loaded test files. You will see the test results and any errors or failures in the terminal.

If you prefer a one-liner command to run the tests, you can execute the following command in your terminal:

```bash
swipl -s main.pl -g "consult('tests/parse_ts'), consult('tests/simplify_ts'), run_tests, halt"
```
This command will load the main.pl, parse_ts.pl, and simplify_ts.pl files, run the tests, and then halt the interpreter.

# Features to add
1. Adding const to AST, so we can deal with 2rd integration correctly (Cx)
