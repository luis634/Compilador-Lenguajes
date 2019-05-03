tokens = [
    'COMMA','END','PROGRAM','NUMBER','CALL',
    'INT','REAL','OPAREN','CPAREN','EQUAL','EQUALS',
    'SUBROUTINE','READ','WRITE','IF','THEN',
    'ELSE','ELSIF','COLLON','DO','EXIT',
    'MINUS','PLUS','MUL','DIV','NOT','AND','OR', 'STRING',
    'GT','GET','NE','LET','LT','ID',
    ]
reserved = {
    'program': 'PROGRAM',
    'if': 'IF',
    'real':'REAL',
    'end' : 'END',
    'read': 'READ',
    'write': 'WRITE',
    'exit': 'EXIT',
    'do': 'DO',
    'or': 'OR',
    'and': 'AND',
    'not': 'NOT',
    'if': 'IF',
    'then': 'THEN',
    'else': 'ELSE',
    'elsif': 'ELSIF',
    'subroutine': 'SUBROUTINE',
    'int': 'INT',
    'call': 'CALL',
 }
# Tokens

t_COMMA = r','
t_STRING= r'".*"'
t_OPAREN= r'\('
t_CPAREN= r'\)'
t_EQUAL = r'='
t_EQUALS = r'=='
t_COLLON= r':'
t_MINUS = r'\-'
t_PLUS  = r'\+'
t_MUL   = r'\*'
t_DIV   = r'/'
t_GT    = r'<'
t_GET   = r'<='
t_NE    = r'<>'
t_LET   = r'>='
t_LT    = r'>'
t_NUMBER= r'[0-9]+'

def t_ID(t):
     r'(\b[a-zA-Z]+\b | \b[a-zA-Z]\w*[a-zA-Z0-9]+\b)'
     t.type = reserved.get(t.value,'ID')    # Check for reserved words
     return t

def t_REAL (t):
	'[-+]?\d+(\.(\d+)?([eE][-+]?\d+)?|[eE][-+]?\d+)'
	return t

t_ignore  = ' \t'
t_ignore_COMMENT = r'\#.*' #Comentarios

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build the lexer
import ply.lex as lex
lexer = lex.lex()

start = 'programa'

precedence = (
('left','PLUS','MINUS'),
('left','MUL','DIV'),
)

PJumps = []     #Jumps stack
PReturns = []   #Returns stack
PTypes = []     #Types stack
POper = []      #Operators stack
POperand = []   #Operands stack
PExit = []      #Exit stack
PTemp = []      #Temporals Stack
for x in range(0,30):
    PTemp.append('~'+str(x))
quadList = []   #Lista de los cuadruplos
quadCount = 0   #Cuenta el numero de cuadruplo en el que nos encontramos
#Diccionario con los tipos de variables
variables = {}
type = ""

class Node:
    def __init__(self, d1=None, d2 = None, base = None):
        self.d1 = d1
        self.d2 = d2
        self.base = base
        return
    def __str__(self):
        return str(self.d1) + ' '+str(self.d2)+' ' + str(self.base)
    def clear(self):
        self.d1 = None
        self.d2 = None
        self.base = None
        return
    def clone(self):
        return(Node(self.d1,self.d2,self.base))
matrices = {}  #Lista con las variables dimensionadas
i = Node()
base = 0

# for x in range(0,30):
#     matrices.append(dValues)
def p_p(p):
    'p :'
    print "p"
def p_empty(p):
     'empty :'
     pass
     p[0] = "empty"
def p_programa(p):
    'programa : PROGRAM ID variables statements END PROGRAM ID subroutines'

def p_variables(p):
    '''variables : variables variables1
                | variables variables2
                | variables variables3
                | variables variables4
                | '''
def p_variables1(p):
    'variables1 : tipo ID loopvars '
    variables[p[2]]=p[1]
def p_loopvars(p):
    '''loopvars : loopvars COMMA ID
                | empty'''
    if (p[1] != "empty"):
        variables[p[3]]=type
def p_variables2(p):
    '''variables2 : tipo ID EQUAL NUMBER
                    | tipo ID EQUAL REAL '''
    variables[p[2]]=p[1]
    if p[1] == 'int':
        quad = ['=',int(float(p[4])),"",p[2]]
    else:
        quad = ['=',float(p[4] ),"",p[2]]
    quadList.append(quad)
    global quadCount
    quadCount = quadCount + 1
def p_variables3(p):
    'variables3 : tipo ID paso1dim OPAREN paso2dim NUMBER paso3dim COMMA NUMBER paso4dim CPAREN paso52dim'
def p_variables4(p):
    'variables4 : tipo ID paso1dim OPAREN paso2dim NUMBER paso3dim CPAREN paso51dim'
def p_paso1dim(p):
    'paso1dim : '
    # variables['[]' + p[-1]]=p[-2]
    # p[0] = '[]' + p[-1]
    variables[p[-1]]=p[-2]
    p[0] = p[-1]
def p_paso2dim(p):
    'paso2dim : '
    i = Node(1)
def p_paso3dim(p):
    'paso3dim : '
    d = int(p[-1])
    i.d1 = d
    # matrices.setdefault(p[-4],i.clone())
def p_paso4dim(p):
    'paso4dim : '
    d = int(p[-1])
    i.d2 = d
    # matrices.setdefault(p[-7],i.clone())
def p_paso51dim(p):
    'paso51dim :'
    global base
    i.base = base
    matrices[p[-6]] = i.clone()
    base = base + i.d1
    i.clear()
    # matrices[p[-6]].d2 = base
    # base = base + matrices[p[-6]].d1
def p_paso52dim(p):
    'paso52dim :'
    global base
    i.base = base
    matrices[p[-9]] = i.clone()
    base = base + i.d1 * i.d2
    i.clear()
    # matrices[p[-9]].d2 = matrices[p[-9]].base.d1
    # base = base + matrices[p[-9]].d1
def p_statements(p):
    '''statements : statements statement
                | statement
                | '''

def p_tipo(p):
    '''tipo : INT
            | REAL'''
    global type
    type = p[1]
    p[0]=p[1]

def p_subroutines(p):
    '''subroutines : subroutines SUBROUTINE ID statements END SUBROUTINE
                | SUBROUTINE ID statements END SUBROUTINE
                | '''

def p_statement(p):
    '''statement : ID paso8 EQUAL expression paso9
                | ID paso8 OPAREN expression pasogetdim1 COMMA expression pasogetdim2 CPAREN EQUAL expression paso9
                | ID paso8 OPAREN expression pasogetdim CPAREN EQUAL expression paso9
                | CALL ID
                | ID COLLON DO paso1do statements paso2do END DO ID
                | READ ID readquad
                | WRITE write writequad
                | IF paso1bool ifs ELSE paso3bool statements END IF paso4bool
                | IF paso1bool ifs END IF paso4bool
                | EXIT paso4do
                | DO ID paso1for EQUAL expression paso2for COMMA expression paso3for COMMA expression statements paso4for1 END DO
                | DO ID paso1for EQUAL expression paso2for COMMA expression paso3for statements paso4for2 END DO
                | DO paso1do statements paso2do END DO'''
def p_write(p):
        '''write : expression correction
                | STRING'''
        p[0]=p[1]
def p_correction(p):
        'correction : '
        POperand.pop()
        PTypes.pop()
def p_paso1bool(p):
    'paso1bool : '
    PJumps.append('/')
def p_paso3bool(p):
    'paso3bool :'
    quad = ['goto','','','']
    quadList.append(quad)
    global quadCount
    quadCount = quadCount + 1
    dir = PJumps.pop()
    quadList[dir][3] = quadCount
    PJumps.append(quadCount-1)
def p_paso1do(p):
    'paso1do : '
    PJumps.append(quadCount)
    PExit.append('-')
def p_paso2do(p):
    'paso2do : '
    dir = PJumps.pop()
    quad = ['goto','','',dir]
    quadList.append(quad)
    global quadCount
    quadCount = quadCount + 1
    if PExit:
        while PExit[-1] != '-':
            dir = PExit.pop()
            quadList[dir][3] = quadCount
        PExit.pop()
def p_paso1for(p):
    'paso1for : '
    POperand.append((p[-1]))
    PTypes.append('int')

def p_paso2for(p):
    'paso2for : '
    E = POperand.pop()
    id = POperand[-1]
    quad = ['=',E,'',id]
    quadList.append(quad)
    global quadCount
    quadCount = quadCount + 1
def p_paso3for(p):
    'paso3for : '
    PJumps.append(quadCount)
    E = POperand.pop()
    id = POperand[-1]
    result = PTemp.pop()
    quad = ['<=',id, E, result]
    quadList.append(quad)
    global quadCount
    quadCount = quadCount + 1
    quad = ['gotoF',result,'','']
    quadList.append(quad)
    global quadCount
    quadCount = quadCount + 1
def p_paso4for1(p):
    'paso4for1 : '
    print POperand
    E = POperand.pop()
    id = POperand.pop()
    quad = ['+',id,E,id]
    quadList.append(quad)
    global quadCount
    quadCount = quadCount + 1
    dir = PJumps.pop()
    quad = ['goto','','',dir]
    quadList.append(quad)
    global quadCount
    quadCount = quadCount + 1
    quadList[dir+1][3] = quadCount
def p_paso4for2(p):
    'paso4for2 : '
    id = POperand.pop()
    quad = ['+',id,'%1',id]
    quadList.append(quad)
    global quadCount
    quadCount = quadCount + 1
    dir = PJumps.pop()
    quad = ['goto','','',dir]
    quadList.append(quad)
    global quadCount
    quadCount = quadCount + 1
    quadList[dir+1][3] = quadCount
def p_writequad(p):
    'writequad : '
    Str = p[-1]
    quad = ['print','','',Str]
    quadList.append(quad)
    global quadCount
    quadCount = quadCount + 1
def p_readquad(p):
    'readquad : '
    id = p[-1]
    quad = ['read','','',id]
    quadList.append(quad)
    global quadCount
    quadCount = quadCount + 1
# def p_declare(p):
    # '''declare : ID paso8 EQUAL expression paso9
    #             | ID paso8 p dimention EQUAL expression paso9'''
# def p_dimention(p):
    # '''dimention : OPAREN expression COMMA expression pasogetdim CPAREN
    #             | OPAREN expression pasogetdim CPAREN'''
def p_paso8(p):
    'paso8 : '
    POperand.append((p[-1]))
    PTypes.append(variables[p[-1]])
def p_paso9(p):
    'paso9 : '
    result = POperand.pop()
    id = POperand.pop()
    quad = ['=',result,"",id]
    quadList.append(quad)
    global quadCount
    quadCount = quadCount + 1
def p_pasogetdim(p):
    'pasogetdim : '
    Si = POperand.pop()
    R = POperand.pop()
    PTypes.pop()
    PTypes.pop()
    result = PTemp.pop()
    global quadCount
    # if matrices[R].d2 == None:
    quad = ['+',Si,'%'+str(matrices[R].base),result]
    quadList.append(quad)
    quadCount = quadCount + 1
    # else:
    #     quad = ['*',Si,'%'+str(matrices[R].d2),result]
    #     quadList.append(quad)
    #     POperand.append(result)
    #     Si = POperand.pop()
    #     result = PTemp.pop()
    #     quad = ['+',Si,'%'+str(matrices[R].base),result]
    #     quadList.append(quad)
    #     quadCount = quadCount + 2
    POperand.append('('+result+')')
    PTypes.append(variables[R])
def p_pasogetdim1(p):
    'pasogetdim1 : '
    Si = POperand.pop()
    R = POperand[-1]
    PTypes.pop()
    result = PTemp.pop()
    global quadCount
    quad = ['*',Si,'%'+str(matrices[R].d2),result]
    quadList.append(quad)
    quadCount = quadCount + 1
    POperand.append(result)
    PTypes.append(variables[R])
def p_pasogetdim2(p):
    'pasogetdim2 : '
    R1 = POperand.pop()
    R2 = POperand.pop()
    PTypes.pop()
    PTypes.pop()
    result = PTemp.pop()
    global quadCount
    quad = ['+',R1,R2,result]
    quadList.append(quad)
    quadCount = quadCount + 1
    POperand.append(result)
    R = POperand.pop()
    bas = POperand.pop()
    result = PTemp.pop()
    quad = ['+',R,'%'+str(matrices[bas].base),result]
    quadList.append(quad)
    quadCount = quadCount + 1
    POperand.append('('+result+')')
    PTypes.append(variables[bas])
def p_ifs(p):
    'ifs : OPAREN logicexp CPAREN paso2bool THEN statements iffs '
def p_paso2bool(p):
    'paso2bool :'
    e = POperand.pop()
    tipo = PTypes.pop()
    if tipo != 'bool':
        raise Exception("ERROR: Type Mismatch! not bool")
    else:
        quad = ['gotoF', e, '', '']
        quadList.append(quad)
        global quadCount
        quadCount = quadCount + 1
        PJumps.append(quadCount-1)
def p_paso4bool(p):
    'paso4bool :'
    dir = PJumps.pop()
    while dir != '/':
        global quadCount
        quadList[dir][3] = quadCount
        dir = PJumps.pop()
def p_iffs(p):
    '''iffs : iffs ELSIF paso3bool OPAREN logicexp CPAREN paso2bool THEN statements
            | ELSIF paso3bool OPAREN logicexp CPAREN paso2bool THEN statements
            | '''
# def p_dowhile(p):
#     '''dowhile : dowhile statements IF OPAREN logicexp CPAREN EXIT paso4do statements ID
#                 | statements IF OPAREN logicexp CPAREN EXIT paso4do statements ID'''
# def p_dowhilenoid(p):
#     '''dowhilenoid : dowhilenoid IF OPAREN logicexp CPAREN EXIT paso4do statements
#     | IF OPAREN logicexp CPAREN EXIT paso4do statements'''
def p_paso4do(p):
    'paso4do : '
    # e = POperand.pop()
    # tipo = PTypes.pop()
    # if tipo != 'bool':
    #     raise Exception("ERROR: Type Mismatch! not bool")
    # else:
    #     quad = ['gotoF', e, '', quadCount + 2]
    #     quadList.append(quad)
    #     global quadCount
    #     quadCount = quadCount + 1
    PExit.append(quadCount)
    quad = ['goto','','','']
    quadList.append(quad)
    global quadCount
    quadCount = quadCount + 1
def resultType(tipo1,tipo2,oper):
    if (oper == '<' or oper == '>' or oper == '<=' or oper == '>=' or oper == '<>' or oper == '=='):
        return 'bool'
    elif (oper == 'not' and tipo2 == 'bool'):
        return 'bool'
    elif(tipo1 == tipo2):
        return tipo1
    elif(tipo1 != tipo2):
        return 'real'
    else:
        return 'error'
def printQuad(quadlist):
    for i in range(len(quadlist)):
        print i,
        print quadlist[i]
def p_expression(p):
    '''expression : expression paso4 PLUS paso23 subexpression paso4
                | expression paso4 MINUS paso23 subexpression paso4
                | subexpression paso4 '''
    p[0]=p[1]
def p_paso4(p):
    'paso4 : '
    if (POper):
        temp = POper.pop()
        POper.append(temp)
        if temp == '+' or temp == '-':
            rightOp = POperand.pop()
            rightType = PTypes.pop()
            leftOp = POperand.pop()
            leftType = PTypes.pop()
            oper = POper.pop()
            typeResult = resultType(leftType,rightType,oper)
            result = PTemp.pop()
            quad = [oper,leftOp,rightOp,result]
            quadList.append(quad)
            global quadCount
            quadCount = quadCount + 1
            POperand.append(result)
            PTypes.append(typeResult)
def p_paso23(p):
    'paso23 :'
    POper.append(p[-1])
def p_subexpression(p):
    '''subexpression : subexpression paso5 MUL paso23 ssubexpression paso5
                | subexpression paso5 DIV paso23 ssubexpression paso5
                | ssubexpression paso5'''
    p[0]=p[1]
def p_paso5(p):
    'paso5 :'
    if (POper):
        temp = POper.pop()
        POper.append(temp)
        if temp == '*' or temp == '/':
            rightOp = POperand.pop()
            rightType = PTypes.pop()
            leftOp = POperand.pop()
            leftType = PTypes.pop()
            oper = POper.pop()
            typeResult = resultType(leftType,rightType,oper)
            result = PTemp.pop()
            quad = [oper,leftOp,rightOp,result]
            quadList.append(quad)
            global quadCount
            quadCount = quadCount + 1
            POperand.append(result)
            PTypes.append(typeResult)
def p_ssubexpression(p):
    '''ssubexpression : ID paso1
                        | ID paso14 OPAREN expression pasogetdim CPAREN
                        | ID paso14 OPAREN expression pasogetdim1 COMMA expression pasogetdim2 CPAREN
                        | NUMBER paso12
                        | REAL paso13
                        | OPAREN paso6 expression CPAREN paso7'''
    p[0]=p[1]
def p_paso1(p):
    'paso1 :'
    POperand.append(p[-1])
    #PTypes.append('real')
    PTypes.append(variables[p[-1]])
def p_paso12(p):
    'paso12 :'
    op = '%'+p[-1]
    POperand.append(op)
    PTypes.append('int')
def p_paso13(p):
    'paso13 :'
    op = '&'+p[-1]
    POperand.append(op)
    PTypes.append('real')
def p_paso14(p):
    'paso14 :'
    POperand.append(p[-1])
    #PTypes.append('real')
    PTypes.append(variables[p[-1]])
def p_paso6(p):
    'paso6 :'
    POper.append("(")
def p_paso7(p):
    'paso7 :'
    POper.pop()
def p_andornot(p):
    'andornot : '
    POper.append(p[-1])
    PTypes.append('bool')
def p_logicexp(p):
    '''logicexp : logicexp paso2lgexp AND andornot NOT andornot slogicexp pasonot paso2lgexp
                | logicexp paso2lgexp OR andornot NOT andornot slogicexp pasonot paso2lgexp
                | logicexp paso2lgexp AND andornot NOT andornot slogicexp pasonot logicop slogicexp paso1lgexp paso2lgexp
                | logicexp paso2lgexp OR andornot NOT andornot slogicexp pasonot logicop slogicexp paso1lgexp paso2lgexp
                | NOT andornot slogicexp pasonot paso2lgexp
                | slogicexp logicop slogicexp paso1lgexp paso2lgexp
                | logicexp paso2lgexp AND andornot slogicexp logicop slogicexp paso1lgexp paso2lgexp
                | logicexp paso2lgexp OR andornot slogicexp logicop slogicexp paso1lgexp paso2lgexp'''
def p_paso1lgexp(p):
    'paso1lgexp :'
    if (POper):
        temp = POper.pop()
        POper.append(temp)
        if temp == '<' or temp == '>' or temp == '<=' or temp == '>=' or temp == '<>' or temp == '==':
            rightOp = POperand.pop()
            rightType = PTypes.pop()
            leftOp = POperand.pop()
            leftType = PTypes.pop()
            oper = POper.pop()
            typeResult = resultType(leftType,rightType,oper)
            result = PTemp.pop()
            quad = [oper,leftOp,rightOp,result]
            quadList.append(quad)
            global quadCount
            quadCount = quadCount + 1
            POperand.append(result)
            PTypes.append(typeResult)
def p_paso2lgexp(p):
    'paso2lgexp :'
    if (POper):
        temp = POper.pop()
        POper.append(temp)
        if temp == 'and' or temp == 'or':
            rightOp = POperand.pop()
            rightType = PTypes.pop()
            leftOp = POperand.pop()
            leftType = PTypes.pop()
            oper = POper.pop()
            typeResult = resultType(leftType,rightType,oper)
            if typeResult != 'bool':
                raise Exception("ERROR: Type Mismatch! not bool")
            else:
                result = PTemp.pop()
                quad = [oper,leftOp,rightOp,result]
                quadList.append(quad)
                global quadCount
                quadCount = quadCount + 1
                POperand.append(result)
                PTypes.append(typeResult)
def p_pasonot(p):
    'pasonot : '
    if POper:
        temp = POper.pop()
        POper.append(temp)
        if temp == "not":
            rightOp = POperand.pop()
            rightType = PTypes.pop()
            #No se requiere otro operador
            oper = POper.pop()
            typeResult = resultType('bool',rightType,oper)
            if typeResult != 'bool' :
                raise Exception("ERROR: Type Mismatch! Not Boolean")
            else:
                result = PTemp.pop()
                quad = [oper,rightOp,'',result]
                quadList.append(quad)
                global quadCount
                quadCount = quadCount + 1
                POperand.append(result)
                PTypes.append(typeResult)

def p_slogicexp(p):
    '''slogicexp : OPAREN logicexp CPAREN
                | ID paso1
                | NUMBER paso12
                | REAL paso13
                | ID OPAREN expression COMMA expression CPAREN
                | ID OPAREN expression CPAREN'''
def p_logicop(p):
    '''logicop : GT
                | GET
                | NE
                | EQUALS
                | LET
                | LT'''
    POper.append(p[1])
def p_error(p):
    if p is not None:
        print("\tSYNTAX ERROR at line " + str(p.lineno) + " Token: " + str(p.lexpos))
        global error_flag
        error_flag = p
    # if p is not :
    #     print ("Line %s, Sintax Error %s" % (p.lineno, p.value))
    # else:
    #     print('Unexpected end of input')


import ply.yacc as yacc
yacc.yacc()

error_flag = 0
# while True:
# s = raw_input('calc > ')   # use input() on Python 3
f = open("programa2.txt", "r")
s = ''
for line in f:
    try:
        s += line
    except EOFError:
        break
# s = f.read()
lexer.input(s)
# print("These are the tokens found: ")
while True:
    tok = lexer.token()
    print tok
    if not tok:
        break      # No more input
    if(isinstance(tok.value,str)):
         print("TOKEN: " + tok.value)
yacc.parse(s)
if error_flag == 0:
    print('Correcto')

################################################################################################
#Ejecutor
def is_number(s):
    try:
        float(s)
        return True
    except ValueError:
        return False

dimentionVector = [] #Array with all memory space
for k in range(0,base):
    dimentionVector.append(0)
values = {}
temporals = []
tValues = {'value':None,'type':None}
for x in range(0,30):
    temporals.append(tValues)
for value in variables:
    if variables[value] == 'int' and value[0] != '[':
        values.setdefault(value, 0)
    elif variables[value] == 'real' and value[0] != '[':
        values.setdefault(value, 0.0)
# print values
# print variables
printQuad(quadList)
# print quadCount
# print POperand
# print PTemp
# print PTypes
# print POper
cont = 0
while cont < len(quadList):
    quad = quadList[cont]
    # print cont,
    # print quad
    if quad[0] == '+' or quad[0] == '-' or quad[0] == '*' or quad[0] == '/' or quad[0] == '==' or quad[0] == '<' or quad[0] == '>' or quad[0] == '<>' or quad[0] == '<=' or quad[0] == '>=':
        if quad[1][0] == '%': #Int
            num1 = int(float(quad[1][1:]))
            type1 = 'int'
        elif quad[1][0] == '&': #Real
            num1 = float(quad[1][1:])
            type1 = 'real'
        elif quad[1][0] == '~': #temporal
            #print temporals[int(float(quad[1][1:]))]
            num1 = temporals[int(float(quad[1][1:]))][value]
            type1 = temporals[int(float(quad[1][1:]))][type]
        else:               #variable
            num1 = values.get(quad[1])
            type1 = variables.get(quad[1])
        # print num1
        if quad[2][0] == '%':
            num2 = int(float(quad[2][1:]))
            type2 = 'int'
        elif quad[2][0] == '&': #Real
            num2 = float(quad[2][1:])
            type2 = 'real'
        elif quad[2][0] == '~': #temporal
            #print temporals[int(float(quad[2][1:]))]
            num2 = temporals[int(float(quad[2][1:]))][value]
            type2 = temporals[int(float(quad[2][1:]))][type]
        else:               #variable
            num2 = values.get(quad[2])
            type2 = variables.get(quad[2])
        # print num2
        if quad[0] == '+':
            # print '+'
            result = num1 + num2
        elif quad[0] == '-':
            # print '-'
            result = num1 - num2
        elif quad[0] == '*':
            # print '*'
            result = float(num1) * num2
        elif quad[0] == '/':
            # print '/'
            result = float(num1) / num2
        elif quad[0] == '==':
            # print '=='
            result = num1 == num2
        elif quad[0] == '<':
            # print '<'
            result = num1 < num2
        elif quad[0] == '>':
            # print '>'
            result = num1 > num2
        elif quad[0] == '<=':
            # print '<='
            result = num1 <= num2
        elif quad[0] == '>=':
            # print '>='
            result = num1 >= num2
        elif quad[0] == '<>':
            # print '<>'
            result = num1 != num2
        # print result
        if quad[3][0] == '~':
            temporals[int(float(quad[3][1:]))][value] = result
            temporals[int(float(quad[3][1:]))][type] = resultType(type1,type2,quad[0])
            #print temporals[int(float(quad[3][1:]))][type]
        else:
            values[quad[3]] = result
            variables[quad[3]] = resultType(type1,type2,None)
    elif quad[0] == '=' and quad[3][0] != '(': #Asignacion
            # print "asignacion"
        if quad[1][0] == '%':
            values[quad[3]] = int(float(quad[1][1:]))
            #variables[quad[3]] = 'int'
        elif quad[1][0] == '&' and variables[quad[3]] == 'real':
            values[quad[3]] = float(quad[1][1:])
            #variables[quad[3]] = 'real'
        elif quad[1][0] == '~' and (variables[quad[3]] == 'real' or variables[quad[3]] == temporals[int(float(quad[1][1:]))][type]):
            values[quad[3]] = temporals[int(float(quad[1][1:]))][value]
            #variables[quad[3]] = temporals[int(float(quad[1][1:]))][type]
        elif quad[1] in values.keys():
            values[quad[3]] = values[quad[1]]
        else:
            print ("type error cannot declare int as real, skipping step")
            print cont
    elif quad[0] == '=':
        if quad[1][0] == '(':
            address = quad[1][1:-1]
            if address[0] == '~':
                address = address[1:]
            address = temporals[int(float(address))][value]
            print address
            assign = dimentionVector[address]
        else:
            if quad[1][0] == '%':
                assign = int(float(quad[1][1:]))
            elif quad[1][0] == '&':
                assign = float(quad[1][1:])
        address = quad[3][2:-1]
        address = temporals[int(float(address))][value]
        dimentionVector[int(address)] = assign
    elif quad[0] == 'not':
        # print 'not'
        if quad[1][0] == '~':
            temporals[int(float(quad[3][1:]))][value] = not temporals[int(float(quad[1][1:]))][value]
            temporals[int(float(quad[3][1:]))][type] = temporals[int(float(quad[1][1:]))][type]
            print temporals[int(float(quad[3][1:]))][value]
    elif quad[0] == 'goto':
        cont = int(float(quad[3])) - 1
    elif quad[0] == 'gotoF':
        if quad[1][0] == '~':
            if not temporals[int(float(quad[1][1:]))][value]:
                cont = int(float(quad[3])) - 1
    elif quad[0] == 'read':
        input = raw_input()
        if is_number(input):
            if variables[quad[3]] == 'int':
                values[quad[3]] = int(float(input))
            else:
                values[quad[3]] = float(input)
    elif quad[0] == 'print':
        if quad[3][0] == '"':
            print quad[3][1:-1]
        else:
            print values[quad[3]]

    cont = cont + 1
#print temporals
print values
print variables
# print matrices['[]a']
# print matrices['[]b']
# print matrices['[]c']
# printQuad(quadList)
print i
print dimentionVector
