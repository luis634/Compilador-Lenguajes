import sys
sys.path.insert(0, "../..")



if sys.version_info[0] >= 3:
    raw_input = input

# List of tokens
tokens = [
    'ID',
    'CONSTANT_INT',
    'CONSTANT_REAL',
    'TEXT',
    'COMMENT'
]

reserved = {
    'program': 'PROGRAM',
    'integer':'INTEGER',
    'real':'REAL',
    'dimension': 'DIMENSION',
    'end' : 'END',
    'main': 'MAIN',
    'print': 'PRINT',
    'read': 'READ',
    'exit': 'EXIT',
    'do': 'DO',
    'begin': 'BEGIN',
    'or': 'OR',
    'and': 'AND',
    'not': 'NOT',
    'if': 'IF',
    'then': 'THEN',
    'else': 'ELSE',
    'elsif': 'ELSIF',
    'subroutine': 'SUBROUTINE'
}

tokens += reserved.values()
# tokens

def t_TEXT(t):
	r'\".*\"'
	return t

def t_COMMENT(t):
     r'\#.*'
     pass

def t_ID(t):
     r'(\b[a-zA-Z]+\b | \b[a-zA-Z]\w*[a-zA-Z0-9]+\b)'
     t.type = reserved.get(t.value,'ID')    # Check for reserved words
     return t

t_CONSTANT_INT  = r"\b\d+\b" #r"\b[-+]?\d+\b"

def t_CONSTANT_REAL (t):
	'[-+]?\d+(\.(\d+)?([eE][-+]?\d+)?|[eE][-+]?\d+)'
	return t


literals    = [',', ":", ";","(", ")", "[","]", "+", "-", "*", "/", "<",">", "="]

#ignore space, tab, enter
t_ignore  = ' \r\n\t'

# Error handling for lex (illegal characters)
def t_error(t):
    #print("Illegal character '%s'" % t.value[0])
    global error_flag_t
    error_flag_t = t
    t.lexer.skip(1)

# Build lex
import ply.lex as lex
lexer = lex.lex()

variables={}
types={}
i=0

# Grammar definition
def p_programa(p):
    'programa : PROGRAM ID V P M END PROGRAM ID'

def p_variables(p):
    '''V : V type_var ":" id_loop
    	| V type_var  ":" IDMAT
    	| empty'''

def p_variable_type(p):
    '''type_var : INTEGER
    			| REAL'''
    global i
    types[i]=p[1]


def p_matrix_format_id0(p):
    'IDMAT : ID "(" CONSTANT_INT second_dimension_const ")" MATLOOP'
    global i
    variables[i]=p[1]
    i+=1

def p_second_dimension_const(p):
	'''second_dimension_const : "," CONSTANT_INT
							| empty'''


def p_matrix_format1(p):
    'MAT : "(" E second_dimension ")" '

def p_second_dimension(p):
    '''second_dimension : "," E
    				 | empty'''

def p_matrix_format_id2(p):
    'MATLOOP : "," IDMAT'
    types[i]=types[i-1]

def p_matrix_format_id3(p):
    'MATLOOP : empty'

def p_id_loop(p):
    'id_loop : ID id_loop_cont'
    global i
    variables[i]=p[1]
    i+=1

def p_id_loop_continue(p):
    'id_loop_cont : "," id_loop'
    global i
    types[i]=types[i-1]


def p_id_loop_empty(p):
    'id_loop_cont : empty'

def p_subroutine(p):
    'P : SUBROUTINE ID "(" ")" S END SUBROUTINE ID P'
def p_subroutine_empty(p):
    'P : empty'

def p_main(p):
    'M : BEGIN S END MAIN'




def p_algebraic_expression(p):
    'E : MULDIV'
def p_expression_addition(p):
    'E : MULDIV "+" E'
def p_expression_substraction(p):
    'E : MULDIV "-" E'
def p_expression_muldiv(p):
    'MULDIV : F'
def p_expression_multiplication(p):
    'MULDIV : F "*" MULDIV'
def p_expression_division(p):
    'MULDIV : F "/" MULDIV'
def p_algebraic_id(p):
    'F : ID'
def p_algebraic_idmat(p):
    'F : ID MAT'
def p_algebraic_constant(p):
    'F : CONSTANT_INT'
def p_algebraic_parenth(p):
    'F : "(" E ")"'




def p_logic_expr(p):
	'EL : S_AND'
def p_logic_expr_or(p):
	'EL : S_AND OR EL'
def p_logic_expr_sand(p):
	'S_AND : S_NOT'
def p_logic_expr_and(p):
	'S_AND : S_NOT AND S_AND'
def p_logic_expr_not(p):
	'S_NOT : NOT TL'
def p_logic_expr_snot(p):
	'S_NOT : TL'
def p_logic_expr_TL(p):
	'TL : E logic_operators E'
def p_logic_expr_TLparenth(p):
	'TL : "(" EL ")"'
def p_logic_operators_0(p):
	'logic_operators : "<"'
def p_logic_operators_1(p):
	'logic_operators : "<" "="'
def p_logic_operators_2(p):
	'logic_operators : "<" ">"'
def p_logic_operators_3(p):
	'logic_operators : "="'
def p_logic_operators_4(p):
	'logic_operators : "=" ">"'
def p_logic_operators_5(p):
	'logic_operators : ">"'


def p_s_if(p):
	'S : IF inside_if else_if END IF S'
def p_s_else(p):
	'else_if : ELSE S'
def p_s_notelse(p):
	'else_if : empty'
def p_if_checkEL(p):
	'inside_if : "(" EL ")" THEN S'
def p_if_checkELSIF(p):
	'inside_if : "(" EL ")" THEN S ELSIF inside_if'


def p_do_parameters(p):
	'S : DO "(" ID "=" E ";" E ";" expresion_or_empty ")" S END DO S'
def p_expresion_or_empty0(p):
	'expresion_or_empty : E '
def p_expresion_or_empty1(p):
	'expresion_or_empty : empty'
def p_do_NOparameters(p):
	'S : DO S END DO S'
def p_exit(p):
	'S : EXIT'

def p_callfunc(p):
	'S : SUBROUTINE ID "[" "]" S'
def p_asignvalue(p):
	'S : ID mat_or_empty "=" E S'
def p_asignvalueMAT(p):
	'mat_or_empty : MAT'
def p_asignvalueEmpty(p):
	'mat_or_empty : empty'


def p_statement_print(p):
    'S : PRINT ">" ">" print_options S'
def p_print_options_ea(p):
	'print_options : E'
def p_print_options_variable(p):
	'print_options : ID mat_or_empty'
def p_print_options_text(p):
	'print_options : TEXT'
def p_statement_read(p):
    'S : READ "<" "<" ID mat_or_empty S'



def p_s_empty(p):
	'S : empty'
def p_empty(p):
    'empty :'
    pass

# Error handkling for yacc (syntax error)
def p_error(p):
    print("\t<< SYNTAX ERROR >>")
    global error_flag
    error_flag = p

# Build yacc
import ply.yacc as yacc
yacc.yacc()

# Test of entered strings

file = open("test.txt", "r")
input_test = file.read()

error_flag = 0
error_flag_t = 0
lexer.input(input_test)
print("These are the tokens found: ")
while True:
	tok = lexer.token()
	print tok
	if not tok:
		break      # No more input
	print("TOKEN: " + tok.value)
yacc.parse(input_test)
if error_flag_t != 0:
	print("lexic error")
else:
	if error_flag == 0:
		print('Correct')
print("\n")

print variables
print types
