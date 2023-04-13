# MicroMath

MicroMath is a simple symbolic math library written in Prolog. It provides basic functionality for parsing, simplifying, and performing symbolic differentiation and integration.

[Course Project Link](https://wiki.ubc.ca/Course:CPSC312-2023/Projects#MicroMath)

## Features

- Expression parsing
- Expression simplification
- Symbolic differentiation
- Symbolic integration
- Conversion to string

## Project Structure

The project is organized into the following main components, which interact with one another to provide the full functionality of MicroMath:

- `src`: Contains the source code for the library, divided into separate modules:
    - `token.pl`: Tokenizes the input string, converting it into a list of tokens.
    - `parse.pl`: Parses the tokens and generates an abstract syntax tree (AST) representing the mathematical expression.
    - `simp.pl`: Simplifies the AST, applying algebraic rules and simplifications to the expression.
    - `convert.pl`: Converts the simplified AST back into a human-readable string or atom.
- `tests`: Provides a suite of tests for each module, ensuring that the library functions correctly for various input cases.
- `examples`: Stores example use cases of the library, demonstrating how to utilize its features in different contexts.
- `main.pl`: Acts as the entry point for the library, combining all modules to offer the primary functionality of MicroMath. This includes the micro_math/2 predicate, which takes a string of an expression as input and returns its simplified version.

The workflow for processing a mathematical expression using MicroMath is as follows:

1. The input string is tokenized into a list of tokens by the `token` module.
2. The `parse` module parses the list of tokens and generates an AST.
3. The `simp` module simplifies the AST according to algebraic rules and simplifications.
4. The `convert` module converts the simplified AST back into a human-readable string or atom, representing the final simplified expression.

# Usage

Clone the repository:
```bash
git clone https://github.com/yourusername/MicroMath.git
cd MicroMath
```
Load the main file in a Prolog interpreter:
```Prolog
swipl -s main.pl
```
Use the `micro_math/2` predicate to parse, simplify, and convert mathematical expressions:
```prolog
?- micro_math("x+0", Result).
Result = "x".
```
To run tests, load the test.pl file in a Prolog interpreter:
```bash
swipl -s tests/test.pl
?- run_tests.
```
This command will execute all the tests in the loaded test files. You will see the test results and any errors or failures in the terminal.

If you prefer a one-liner command to run the tests, you can execute the following command in your terminal:

```bash
swipl -s main.pl -g "consult('tests/parse_ts'), consult('tests/simp_ts'), run_tests, halt"
```
This command will load the main.pl, parse_ts.pl, and simplify_ts.pl files, run the tests, and then halt the interpreter.

# Features to add
1. Adding const to AST, so we can deal with 2rd integration correctly (Cx)
2. Improve differentiation and integration, more generic 
```Prolog
ast_to_string(diff(exp(mul(vrb(x), pow(vrb(x), num(2)))), mul(num(2), vrb(x))),  T).
T = 'D2*x(exp(x*x^2))'.
```

# Processing Procedures

The processing of a mathematical expression in MicroMath involves four main steps: tokenization, parsing, simplification, and conversion to string.

1. **Tokenization**: The first step in processing a mathematical expression is to tokenize the input string. This process is handled by the `token.pl` module, which converts the input string into a list of tokens. Tokens represent the basic units of a mathematical expression, such as numbers, variables, and operators. By breaking the input string into tokens, we can more easily analyze and manipulate the expression in subsequent steps.
2. **Parsing**: After tokenization, the list of tokens is parsed by the `parse.pl` module. This module is responsible for constructing an abstract syntax tree (AST) from the tokens. The AST is a tree structure that represents the hierarchical relationships between the different components of the mathematical expression. By constructing an AST, we can more easily manipulate the expression's structure and apply algebraic rules in the simplification step.
3. **Simplification**: Once the AST has been generated, the `simp.pl` module is responsible for simplifying the expression. This involves applying various algebraic rules and simplifications to the AST, such as combining like terms, canceling out terms, and simplifying trigonometric functions. The output of this step is a simplified AST, which represents the most simplified form of the input expression.
4. **Conversion to String**: Finally, after the expression has been simplified, the `convert.pl` module converts the simplified AST back into a human-readable string or atom. This involves traversing the AST and concatenating the string representations of its nodes in a specific order. The output of this step is the final simplified expression in a human-readable format, which can be displayed or further processed as needed.

By combining these four steps, MicroMath can process a mathematical expression from a raw input string to a simplified, human-readable output. This modular design makes it easy to extend and modify the library to support additional features or improvements in the future.

