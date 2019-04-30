 # -----------------------------------------------------------------------------
# calc.py
#
# A simple calculator with variables.
# -----------------------------------------------------------------------------

tokens = (
    'HOLA','COMMA',
    'QUE','TAL',
    )

# Tokens

t_HOLA    = r'hola'
t_COMMA   = r','
t_QUE     = r'que'
t_TAL     = r'tal'

t_ignore  = ' \t'

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build the lexer
import ply.lex as lex
lex.lex()

def p_grammar(p):
    'grammar : statement QUE TAL'

def p_statement(p):
    '''statement : statement COMMA HOLA
                | HOLA'''

def p_error(p):
    print("Syntax error")

import ply.yacc as yacc
yacc.yacc()

while True:
    try:
        s = raw_input('calc > ')   # use input() on Python 3
    except EOFError:
        break
    yacc.parse(s)
