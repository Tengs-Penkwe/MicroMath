



# How to run tests
To run the tests in your Prolog project, you can do the following steps:

First, make sure you are in the root directory of your project.
Then, start the SWI-Prolog interpreter by typing swipl in the terminal.
Load the main.pl file and the test files by entering the following commands in the SWI-Prolog interpreter:
```bash
?- [src/main].
?- [tests/dev_ts, tests/integ_ts].
```
Finally, run the tests using the `run_tests` predicate:
```bash
?- run_tests.
```
This command will execute all the tests in the loaded test files. You will see the test results and any errors or failures in the terminal.

If you prefer a one-liner command to run the tests, you can execute the following command in your terminal:

```bash
swipl -s src/main.pl -g "consult('tests/dev_ts'), consult('tests/integ_ts'), run_tests, halt"
```
This command will load the main.pl, dev_ts.pl, and integ_ts.pl files, run the tests, and then halt the interpreter.
