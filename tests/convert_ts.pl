:- use_module('../src/convert.pl').

:- begin_tests(ast_to_string).

test(num) :-
    ast_to_string(num(5), '5').

test(numbers) :-
    ast_to_string(num(42), '42').

test(variable) :-
    ast_to_string(vrb(x), x).

test(add) :-
    ast_to_string(add(num(1), num(2)), '1+2').

test(sub) :-
    ast_to_string(sub(num(1), num(2)), '1-2').

test(mul) :-
    ast_to_string(mul(num(1), num(2)), '1*2').

test(dvd) :-
    ast_to_string(dvd(num(1), num(2)), '1/2').

test(pow) :-
    ast_to_string(pow(num(1), num(2)), '1^2').

test(sin) :-
    ast_to_string(sin(num(1)), 'sin(1)').

test(cos) :-
    ast_to_string(cos(num(1)), 'cos(1)').

test(exp) :-
    ast_to_string(exp(num(1)), 'exp(1)').

test(ln) :-
    ast_to_string(ln(num(1)), 'ln(1)').

test(diff) :-
    ast_to_string(diff(num(1), vrb(x)), 'Dx(1)').

test(integ) :-
    ast_to_string(integ(num(1), vrb(x)), '∫(1)x').

test(nested) :-
    ast_to_string(add(num(1), mul(num(2), num(3))), '1+2*3').

:- end_tests(ast_to_string).

:- begin_tests(parentheses).

test(simple_add) :-
    ast_to_string(add(num(1), num(2)), '1+2').

test(simple_sub) :-
    ast_to_string(sub(num(1), num(2)), '1-2').

test(simple_mul) :-
    ast_to_string(mul(num(1), num(2)), '1*2').

test(simple_dvd) :-
    ast_to_string(dvd(num(1), num(2)), '1/2').

test(add_mul) :-
    ast_to_string(add(num(1), mul(num(2), num(3))), '1+2*3').

test(add_dvd) :-
    ast_to_string(add(num(1), dvd(num(2), num(3))), '1+2/3').

test(mul_dvd) :-
    ast_to_string(mul(num(1), dvd(num(2), num(3))), '1*2/3').

test(sub_dvd) :-
    ast_to_string(sub(num(1), dvd(num(2), num(3))), '1-2/3').

test(nested_add_mul) :-
    ast_to_string(add(num(1), add(mul(num(2), num(3)), num(4))), '1+2*3+4').

test(nested_sub_mul) :-
    ast_to_string(sub(num(1), sub(mul(num(2), num(3)), num(4))), '1-(2*3-4)').

test(nested_add_dvd) :-
    ast_to_string(add(num(1), add(dvd(num(2), num(3)), num(4))), '1+2/3+4').

test(nested_sub_dvd) :-
    ast_to_string(sub(num(1), sub(dvd(num(2), num(3)), num(4))), '1-(2/3-4)').

test(nested_mul_dvd) :-
    ast_to_string(mul(num(1), mul(dvd(num(2), num(3)), num(4))), '1*2/3*4').

test(nested_add_mul_dvd) :-
    ast_to_string(add(num(1), mul(dvd(num(2), num(3)), num(4))), '1+2/3*4').

test(nested_sub_mul_dvd) :-
    ast_to_string(sub(num(1), mul(dvd(num(2), num(3)), num(4))), '1-2/3*4').

test(nested_dvd_dvd) :-
    ast_to_string(dvd(num(1), dvd(num(2), num(3))), '1/(2/3)').

test(nested_add_dvd_dvd) :-
    ast_to_string(add(num(1), dvd(num(2), num(3))), '1+2/3').

test(nested_sub_dvd_dvd) :-
    ast_to_string(sub(num(1), dvd(num(2), num(3))), '1-2/3').

test(nested_dvd_mul_dvd) :-
    ast_to_string(dvd(num(1), mul(dvd(num(2), num(3)), num(4))), '1/(2/3*4)').

test(nested_mul_mul) :-
    ast_to_string(mul(num(1), mul(num(2), num(3))), '1*2*3').

test(nested_dvd_add) :-
    ast_to_string(dvd(num(1), add(num(2), num(3))), '1/(2+3)').

test(nested_dvd_sub) :-
    ast_to_string(dvd(num(1), sub(num(2), num(3))), '1/(2-3)').

test(nested_mul_add_sub) :-
    ast_to_string(mul(add(num(1), num(2)), sub(num(3), num(4))), '(1+2)*(3-4)').

test(nested_dvd_add_sub) :-
    ast_to_string(dvd(add(num(1), num(2)), sub(num(3), num(4))), '(1+2)/(3-4)').

test(nested_mul_dvd_add_sub) :-
    ast_to_string(mul(dvd(num(1), num(2)), add(num(3), sub(num(4), num(5)))), '1/2*(3+4-5)').

test(nested_dvd_mul_add_sub) :-
    ast_to_string(dvd(mul(num(1), num(2)), add(num(3), sub(num(4), num(5)))), '1*2/(3+4-5)').

test(pow_pow) :-
    ast_to_string(pow(num(1), pow(num(2), num(3))), '1^2^3').

test(pow_dvd) :-
    ast_to_string(pow(dvd(num(1), num(2)), num(3)), '(1/2)^3').

test(pow_mul) :-
    ast_to_string(pow(mul(num(1), num(2)), num(3)), '(1*2)^3').

test(pow_add) :-
    ast_to_string(pow(add(num(1), num(2)), num(3)), '(1+2)^3').

test(pow_sub) :-
    ast_to_string(pow(sub(num(1), num(2)), num(3)), '(1-2)^3').

test(add_pow) :-
    ast_to_string(add(num(1), pow(num(2), num(3))), '1+2^3').

test(sub_pow) :-
    ast_to_string(sub(num(1), pow(num(2), num(3))), '1-2^3').

test(mul_pow) :-
    ast_to_string(mul(num(1), pow(num(2), num(3))), '1*2^3').

test(dvd_pow) :-
    ast_to_string(dvd(num(1), pow(num(2), num(3))), '1/2^3').

test(nested_pow_add_sub) :-
    ast_to_string(pow(add(num(1), num(2)), sub(num(3), num(4))), '(1+2)^(3-4)').

test(nested_pow_mul_dvd) :-
    ast_to_string(pow(mul(num(1), num(2)), dvd(num(3), num(4))), '(1*2)^(3/4)').

test(nested_add_pow_mul) :-
    ast_to_string(add(num(1), mul(pow(num(2), num(3)), num(4))), '1+2^3*4').

test(nested_sub_pow_dvd) :-
    ast_to_string(sub(num(1), dvd(pow(num(2), num(3)), num(4))), '1-2^3/4').

test(nested_mul_mul_mul) :-
    ast_to_string(mul(num(1), mul(num(2), mul(num(3), num(4)))), '1*2*3*4').

test(nested_mul_dvd_mul) :-
    ast_to_string(mul(num(1), dvd(mul(num(2), num(3)), num(4))), '1*2*3/4').

test(nested_dvd_dvd_dvd) :-
    ast_to_string(dvd(num(1), dvd(num(2), dvd(num(3), num(4)))), '1/(2/(3/4))').

test(nested_mul_dvd_dvd_mul) :-
    ast_to_string(mul(dvd(num(1), num(2)), dvd(num(3), mul(num(4), num(5)))), '1/2*3/(4*5)').

test(nested_dvd_mul_dvd_mul) :-
    ast_to_string(dvd(mul(num(1), num(2)), mul(dvd(num(3), num(4)), num(5))), '1*2/(3/4*5)').

test(nested_mul_dvd_dvd_dvd_mul) :-
    ast_to_string(mul(dvd(num(1), dvd(num(2), dvd(num(3), num(4)))), mul(num(5), num(6))), '1/(2/(3/4))*5*6').

test(nested_dvd_mul_mul_mul_dvd) :-
    ast_to_string(dvd(mul(num(1), mul(num(2), mul(num(3), num(4)))), dvd(num(5), num(6))), '1*2*3*4/(5/6)').

test(nested_mul_dvd_mul_dvd_mul) :-
    ast_to_string(mul(dvd(num(1), num(2)), mul(dvd(num(3), num(4)), num(5))), '1/2*3/4*5').

test(nested_dvd_mul_dvd_mul_dvd) :-
    ast_to_string(dvd(mul(num(1), num(2)), mul(dvd(num(3), num(4)), num(5))), '1*2/(3/4*5)').

test(nested_mul_mul_dvd_dvd_mul) :-
    ast_to_string(mul(mul(num(1), num(2)), dvd(dvd(num(3), num(4)), num(5))), '1*2*(3/4)/5').

test(nested_dvd_dvd_mul_mul_dvd) :-
    ast_to_string(dvd(dvd(num(1), num(2)), mul(mul(num(3), num(4)), num(5))), '(1/2)/(3*4*5)').

:- end_tests(parentheses).

:- begin_tests(ast_to_string_complex).

test(nested_expression_5) :-
    ast_to_string(add(mul(num(2), pow(vrb(x), num(2))), mul(num(3), vrb(x))), '2*x^2+3*x').

test(nested_expression_6) :-
    ast_to_string(dvd(mul(num(4), pow(vrb(x), num(2))), add(num(2), vrb(x))), '4*x^2/(2+x)').

test(nested_expression_7) :-
    ast_to_string(mul(sin(add(vrb(x), num(1))), cos(sub(vrb(x), num(1)))), 'sin(x+1)*cos(x-1)').

test(nested_expression_8) :-
    ast_to_string(exp(mul(num(2), add(vrb(x), num(3)))), 'exp(2*(x+3))').

test(nested_expression_9) :-
    ast_to_string(pow(dvd(num(1), add(vrb(x), num(1))), num(3)), '(1/(x+1))^3').

test(nested_expression_10) :-
    ast_to_string(diff(add(pow(vrb(x), num(2)), mul(num(3), vrb(x))), vrb(x)), 'Dx(x^2+3*x)').

test(nested_expression_11) :-
    ast_to_string(integ(mul(sin(vrb(x)), cos(vrb(x))), vrb(x)), '∫(sin(x)*cos(x))x').

test(nested_expression_12) :-
    ast_to_string(diff(exp(mul(vrb(x), pow(vrb(x), num(2)))), vrb(x)), 'Dx(exp(x*x^2))').

test(nested_expression_13) :-
    ast_to_string(integ(add(mul(num(2), pow(vrb(x), num(2))), mul(num(3), vrb(x))), vrb(x)), '∫(2*x^2+3*x)x').

:- end_tests(ast_to_string_complex).

