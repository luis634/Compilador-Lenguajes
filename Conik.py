#Reserved keywords
reserved = {
    'int' : 'INT_KEYWORD',
    'float' : 'FLOAT_KEYWORD',
    'var' : 'VAR_KEYWORD',
    'parabola' : 'PARABOLA_KEYWORD',
    'ellipse' : 'ELLIPSE_KEYWORD',
    'bool' : 'BOOL_KEYWORD',
    'hyperbola' : 'HYPERBOLA_KEYWORD',
    'circle' : 'CIRCLE_KEYWORD',
    'plot' : 'PLOT_KEYWORD',
    'rotate' : 'ROTATE_KEYWORD',
    'reflect' : 'REFLECT_KEYWORD',
    'if' : 'IF_STATEMENT',
    'else' : 'ELSE_STATEMENT',
    'proc' : 'PROC_KEYWORD',
    'true' : 'TRUE_KEYWORD',
    'for' : 'FOR_LOOP_KEYWORD',
    'false' : 'FALSE_KEYWORD',
    'while' : 'WHILE_LOOP_KEYWORD',
	'red' : 'RED_KEYWORD',
	'orange' : 'ORANGE_KEYWORD',
	'yellow' : 'YELLOW_KEYWORD',
	'green' : 'GREEN_KEYWORD',
	'blue' : 'BLUE_KEYWORD',
	'purple' : 'PURPLE_KEYWORD',
    'black' : 'BLACK_KEYWORD',
    'and' : 'AND_KEYWORD',
    'or' : 'OR_KEYWORD',
    'not' : 'NOT_KEYWORD',
    'print' : 'PRINT_KEYWORD',
    'program' : 'PROGRAM_KEYWORD',
    'void' : 'VOID_KEYWORD',
    'return': 'RETURN_KEYWORD'

    #Planned tokens:

    # 'area' : 'AREA_KEYWORD',
    # 'perimeter' : 'PERIMETER_KEYWORD',
    # 'focus' : 'FOCUS_KEYWORD',
    # 'axis' : 'AXIS_KEYWORD',
    # 'center' : 'CENTER_KEYWORD',
    # 'pi' : 'PI_KEYWORD',
    # 'euler' : 'EULER_KEYWORD',
    # 'vertex' : 'VERTEX_KEYWORD',
    # 'radius' : 'RADIUS_KEYWORD',
    # 'standard' : 'STANDARD_KEYWORD',
    # 'general' : 'GENERAL_KEYWORD',
    # 'exc' : 'EXC_KEYWORD',
    # 'reflection' : 'REFLECTION_KEYWORD',
	# 'translate' : 'TRANS_KEYWORD',
	# 'rotation' : 'ROTATION_KEYWORD',
	# 'stretch' : 'STRETCH_KEYWORD',
}


tokens = ['ID', 'CONS_STRING', 'CONS_INT',
        'CONS_FLOAT', 'RELOP',
        'OPEN_BRACKET','CLOSE_BRACKET','OPEN_SQUARE_BRACKET','CLOSE_SQUARE_BRACKET', 'COMMA', 'PLUSOP', 'MINUSOP',
        'TIMESOP', 'DIVIDEOP', 'OPEN_PARENTHESES', 'CLOSE_PARENTHESES',
        'ARROW', 'SEMICOLON', 'EQUALOP'] + list(reserved.values())

t_CONS_STRING = r'\".*\"'
t_CONS_INT = r'[0-9]+'
t_CONS_FLOAT = r'[0-9]+\.[0-9]+'

#Old tokens:
t_OPEN_BRACKET = r'\{'
t_CLOSE_BRACKET = r'\}'
t_OPEN_SQUARE_BRACKET = r'\['
t_CLOSE_SQUARE_BRACKET = r'\]'
t_COMMA = r'\,'
t_PLUSOP = r'\+'
t_MINUSOP = r'\-'
t_TIMESOP = r'\*'
t_DIVIDEOP = r'/'
t_OPEN_PARENTHESES = r'\('
t_CLOSE_PARENTHESES = r'\)'
t_ARROW = r'\~' # For equation declaration
t_SEMICOLON = r';'
t_EQUALOP = r'='
t_RELOP = r'<(>)? | > | =='

t_ignore = " \t" #Ignore whitespace

t_ignore_COMMENT = r'\#.*' #Ignore comments

def t_ID(t):
    r'[A-Za-z]([A-Za-z]|[0-9])*'
    t.type = reserved.get(t.value, 'ID')
    if t.type == 'ID':
        gv.currentId = t.value
    else:
        gv.currentId = ''
    return t

#Counter for lines
def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

#If error, print character that is the problem
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build the lexer
import ply.lex as lex
lexer = lex.lex()



#Grammar and parsing
import symbol_table as symtab
from mem import mem #Memory
import vm #Virtual machine
import re #Regular expressions
from global_variables import gv #Global variables
from semantics_cube import sem_cube #Semantics cube
from semantics_cube import operators_dict
from semantics_cube import var_types_dict

PilaOp = [] #Operands stack
PTypes = [] #Types stack
POper = [] #Operators stack
PJumps = [] #Jumps stack
PReturns = [] #Returns stack
listPendingQuads = [] #List made of lists which are the quads that are missing
PModDataTypes = [] #Stack for data types in module declaration
temporal_mem = []
PAssign = [] #Stack where we save the array stuff from the left side

#The starting rule:
def p_PROGRAM(t):
    'PROGRAM : goto_main PROGRAM_KEYWORD ID addProg SEMICOLON A'
    #print(symtab.SYM_TABLE)
    quad = ["END",[],[],[]]
    gv.quadList.append(quad)
    gv.quadCount += 1
    cont=0
    # prints quad list generated from program
    # for x in gv.quadList:
    #     print(str(cont) + ".- " + str(x))
    #     cont = cont + 1

    #Finally, after program ends, run the VM
    vm.run(gv.quadList, symtab, mem)

def p_addProg(t):
    'addProg :'
    gv.currentType = "PROGRAM" # tipo de dato "PROGRAM"
    symtab.add_variable("GLOBAL",gv.currentId,gv.currentType,None,None)

def p_TYPE_S(t):
    '''TYPE_S : PARABOLA_KEYWORD
            | ELLIPSE_KEYWORD
			| HYPERBOLA_KEYWORD
			| CIRCLE_KEYWORD'''
    gv.currentType = t[1]
    return t[1]

def p_TYPE_P(t):
    '''TYPE_P : INT_KEYWORD
            | FLOAT_KEYWORD
    		| BOOL_KEYWORD'''
    gv.currentType = t[1]
    return t[1]

def p_A(t):
    '''A : VARS B
            | B'''

def p_B(t):
    '''B : MODULE B
            | fill_main BLOCK'''

def p_goto_main(t):
    'goto_main : '
    quad = ['GOTO', [], [], -1]
    gv.quadList.append(quad)
    gv.quadCount += 1

#fills the missing quad number in the first quad (GOTO main)
def p_fill_main(t):
    'fill_main :'
    gv.quadList[0][3] = gv.quadCount

def p_VARS(t):
    '''VARS : VAR_KEYWORD V'''

def p_V(t):
    '''V : TYPE_P C
            | TYPE_S C '''

def p_C(t):
    '''C : ID add_variable D
            | ID OPEN_SQUARE_BRACKET EXP CLOSE_SQUARE_BRACKET add_variableArr D
			| ID OPEN_SQUARE_BRACKET EXP CLOSE_SQUARE_BRACKET OPEN_SQUARE_BRACKET EXP CLOSE_SQUARE_BRACKET add_variableArr2 D'''


def p_add_variable(t):
    'add_variable :'
    size = 1
    # Adding variable to symtab
    if mem.checkSizeAvail(size, gv.currentType, gv.currentScope) :
        memAddress = mem.add_var(gv.currentType, None, size, gv.currentScope)
        # Assigned memory address to variable
        symtab.add_variable(gv.currentScope,gv.currentId,gv.currentType, size, memAddress)
        # Linked var memory to var table
    else :
        raise Exception("Memory size exceeded in variables declaration")

#Receives a number in a string, and returns either float or int depending on its type
def getCons(x):
    if "." in x:
        return float(x)
    else:
        return int(x)

def p_add_variableArr(t):
    'add_variableArr :'
    # size = whatever the user put between the square brackets
    size = PilaOp.pop()
    if isinstance(size,str):
        if size[0] == '%':
            size = getCons(size[1:])
    PTypes.pop()
    if mem.checkSizeAvail(size, gv.currentType, gv.currentScope) :
        memAddress = mem.add_var(gv.currentType, None, size, gv.currentScope)
        # add variable
        symtab.add_variable(gv.currentScope,gv.currentId,gv.currentType, size, memAddress)
    else :
        raise Exception("Memory size exceeded in variables declaration (1D ARRAY SIZE)")

def p_add_variableArr2(t):
    'add_variableArr2 :'
    a = PilaOp.pop()
    b = PilaOp.pop()
    PTypes.pop()
    PTypes.pop()

    if isinstance(a,str):
        if a[0] == '%':
            a = getCons(a[1:])
            #print("a: " + str(a))
    if isinstance(b,str):
        if b[0] == '%':
            b = getCons(b[1:])
            #print("b: " + str(b))

    size = a * b

    if mem.checkSizeAvail(size, gv.currentType, gv.currentScope) :
        memAddress = mem.add_var(gv.currentType, None, size, gv.currentScope)
        symtab.add_variable(gv.currentScope,gv.currentId,gv.currentType, size, memAddress)
        symtab.add_dims(gv.currentScope,gv.currentId,b,a)
    else :
        raise Exception("Memory size exceeded in variables declaration (2D ARRAY SIZE)")


def p_D(t):
    '''D : COMMA C
            | SEMICOLON V
            | SEMICOLON'''

def p_BLOCK(t):
    'BLOCK : OPEN_BRACKET G'

def p_G(t):
    '''G : STATEMENT G
            | CLOSE_BRACKET'''

def p_STATEMENT(t):
    '''STATEMENT : EXPRESSION_BOOL
            | ASSIGN
			| WRITE
			| FOR_LOOP
			| WHILE_LOOP
			| CONDITION
            | RETURN_STATEMENT
            | PROC_CALL'''
            # | F_CALL'''

def p_RETURN_STATEMENT(t):
    '''RETURN_STATEMENT : RETURN_KEYWORD EXPRESSION_BOOL SEMICOLON ret
            | RETURN_KEYWORD SEMICOLON ret_void'''

def p_ret(t):
    'ret :'
    if symtab.get_return_type_module(gv.currentScope) != "void":
        retValue = PilaOp.pop()
        gv.retValue = retValue
        quad = ["RETURN",retValue,[],symtab.return_mod_address(gv.currentScope)]
        gv.quadList.append(quad)
        gv.quadCount = gv.quadCount + 1

        quad = ["GOTO", [], [], []]
        gv.quadList.append(quad)
        gv.quadCount = gv.quadCount + 1

        PReturns.append(gv.quadCount-1)
        gv.flagReturn = True
    else:
        raise Exception("Void function can't return value")

def p_ret_void(t):
    '''ret_void :'''
    if symtab.get_return_type_module(gv.currentScope) == "void":
        quad = ["GOTO", [], [], []]
        gv.quadList.append(quad)
        gv.quadCount = gv.quadCount + 1
        if not gv.flagReturn:
            PJumps.append("piso")

        PJumps.append(gv.currentQuad)
        gv.flagReturn = True
    else:
        raise Exception("Function must return value")

def p_PROC_CALL(t):
    '''PROC_CALL : PROC_KEYWORD ID modCall_paso1 modCall_paso2 OPEN_PARENTHESES V1 '''
    #print("PROC_CALL")

def p_V1(t):
    '''V1 : EXP modCall_paso3 W1
            | EXP modCall_paso3 COMMA modCall_paso4 V1
            | W1'''

def p_W1(t):
    '''W1 : modCall_paso5 CLOSE_PARENTHESES SEMICOLON modCall_paso6'''

def p_ASSIGN(t):
    '''ASSIGN : ID ASSIGN0D EQUALOP EXPRESSION SEMICOLON
                | ID ASSIGNCS ARROW CONS_STRING ASSIGN_S SEMICOLON
                | ID OPEN_SQUARE_BRACKET EXP CLOSE_SQUARE_BRACKET ASSIGN1D EQUALOP EXPRESSION SEMICOLON
                | ID OPEN_SQUARE_BRACKET EXP CLOSE_SQUARE_BRACKET OPEN_SQUARE_BRACKET EXP CLOSE_SQUARE_BRACKET ASSIGN2D EQUALOP EXPRESSION SEMICOLON'''
    lastType = PTypes.pop() #Get the type of the id (on the right side of the assign)
    result_Type = sem_cube[operators_dict["="]][var_types_dict[symtab.get_return_type(gv.currentScope,t[1])]][var_types_dict[lastType]] #Check if the assign is valid
    if result_Type != -1 :
        # Checks if the left side of = is another Arr
        if not PAssign:
            quad = ["=",PilaOp.pop(),[],gv.currentArrAddressL]
        else:
            quad = ["=",PilaOp.pop(),[],PAssign.pop()]
        gv.quadList.append(quad)
        gv.quadCount = gv.quadCount + 1
    else:
        raise Exception("Incompatible types " + lastType + " assigned to " + symtab.get_return_type(gv.currentScope,t[1]))

def p_ASSIGNCS(t):
    'ASSIGNCS :'
    PTypes.append(symtab.get_return_type(gv.currentScope,gv.currentId))
    gv.currentArrAddressL = symtab.get_var_address(gv.currentScope,t[-1])

def p_ASSIGN_S(t):
    'ASSIGN_S :'
    #Check what type are we assigning an equation to, and check if they're valid equations
    if symtab.get_return_type(gv.currentScope, t[-4]) == "circle":
        if verifyEquationCircle(t[-1]):
            PilaOp.append(t[-1])
            saveCircleEqValues(t[-1])
        else:
            raise Exception("Error: invalid circle syntax")
    elif symtab.get_return_type(gv.currentScope, t[-4]) == "ellipse":
        if verifyEquationEllipse(t[-1]):
            PilaOp.append(t[-1])
            saveEllipseEqValues(t[-1])
        else:
            raise Exception("Error: invalid ellipse syntax")
    elif symtab.get_return_type(gv.currentScope, t[-4]) == "parabola":
        if verifyEquationParabola(t[-1]):
            PilaOp.append(t[-1])
            saveParabolaEqValues(t[-1])
        else:
            raise Exception("Error: invalid parabola syntax")
    elif symtab.get_return_type(gv.currentScope, t[-4]) == "hyperbola":
        if verifyEquationHyperbola(t[-1]):
            PilaOp.append(t[-1])
            saveHyperbolaEqValues(t[-1])
        else:
            raise Exception("Error: invalid hyperbola syntax")

#Create the quads to save the circle equation values (string) in memory
def saveCircleEqValues(circleEquation):
    #Sanitize the input removing spaces and the string
    circleEquation = circleEquation.replace(" ", "")
    circleEquation = circleEquation.replace("\"", "")
    A=""
    B=""
    R=""
    i = 0
    j = 0
    #A*X^2 + B*Y^2 = R
    #Where A=B
    for x in circleEquation:
        if x == "X" or x == "x":
            A= circleEquation[:i]
            j = i+4
        elif x == "Y" or x == "y":
            B = circleEquation[j:i]
            j = i + 4
            R = circleEquation[j:]
        i = i + 1
    if A == "":
        A = "1"
    if B == "":
        B = "1"
    size = 1
    #Check type of A, B and R for adding in the symbol table
    if "." in A:
        AType = "float"
    else:
        AType = "int"
    if "." in B:
        BType = "float"
    else:
        BType = "int"
    if "." in R:
        RType = "float"
    else:
        RType = "int"

    #For A value of circle equation
    if mem.checkSizeAvail(size, "circle", "TEMP") :
        result = mem.nextAvail(4) #4 = circle
        symtab.add_variable(gv.currentScope,"#"+gv.currentId+"A",AType, size, result)
        quad = ["=", "%"+A , [], result]
        gv.quadList.append(quad)
        gv.quadCount += 1
    else :
        raise Exception("Memory size exceeded in variables declaration")

    #For B value of circle equation
    if mem.checkSizeAvail(size, "circle", "TEMP") :
        result = mem.nextAvail(4) #4 = circle
        symtab.add_variable(gv.currentScope,"#"+gv.currentId+"B",BType, size, result)
        quad = ["=", "%"+B , [], result]
        gv.quadList.append(quad)
        gv.quadCount += 1
    else :
        raise Exception("Memory size exceeded in variables declaration")

    #For R value of circle equation
    if mem.checkSizeAvail(size, "circle", "TEMP") :
        result = mem.nextAvail(4) #4 = circle
        symtab.add_variable(gv.currentScope,"#"+gv.currentId+"R",RType, size, result)
        quad = ["=", "%"+R , [], result]
        gv.quadList.append(quad)
        gv.quadCount += 1
    else :
        raise Exception("Memory size exceeded in variables declaration")

#Returns true if the equation is valid for a circle, false if it isn't
def verifyEquationCircle(equation):
    #equation = equation.replace(" ", "")
    equation = equation[1:-1]
    if re.match(r'\s*([0-9]+(\.[0-9]+)?)?\s*\s*x\s*\^\s*2\s*\+\s*([0-9]+(\.[0-9]+)?)?\s*y\s*\^\s*2\s*\=\s*([0-9]+(\.[0-9]+)?\s*)\s*', equation):
        return True
    else:
        return False

#Create the quads to save the parabola equation values (string) in memory
def saveParabolaEqValues(parabolaEquation):
    #Sanitize the input removing spaces and the string
    parabolaEquation = parabolaEquation.replace(" ", "")
    parabolaEquation = parabolaEquation.replace("\"", "")
    A=""
    B=""
    C=""
    i = 0
    j = 0
    #Y = A*X^2 + B*x + C
    flagCasita = False
    for x in parabolaEquation:
        if x == "=":
            j = i + 1
        elif x == "^":
            A = parabolaEquation[j:i-1]
            j = i + 3
            flagCasita = True
        elif (x == "X" or x == "x") and flagCasita:
            if parabolaEquation[j-1] == "-":
                B = B + "-"
            B = B + parabolaEquation[j:i]
            j = i + 1
            C = parabolaEquation[j:]
        i = i + 1

    if A == "":
        A = "1"
    if B == "":
        B = "1"
    if C == "":
        C = "1"

    size = 1

    if int(A) == 0 or float(A) == 0:
        raise Exception("Error: invalid parabola syntax")
    #Check type of A, B and C for adding in the symbol table
    if "." in A:
        AType = "float"
    else:
        AType = "int"
    if "." in B:
        BType = "float"
    else:
        BType = "int"
    if "." in C:
        CType = "float"
    else:
        CType = "int"

    #For A value of parabola equation
    if mem.checkSizeAvail(size, "parabola", "TEMP") :
        result = mem.nextAvail(3) #3 = parabola
        #memAddress = mem.add_var("parabola", None, size, gv.currentScope)
        symtab.add_variable(gv.currentScope,"#"+gv.currentId+"A",AType, size, result)
        quad = ["=", "%"+A , [], result]
        gv.quadList.append(quad)
        gv.quadCount += 1
    else :
        raise Exception("Memory size exceeded in variables declaration")

    #For B value of parabola equation
    if mem.checkSizeAvail(size, "parabola", "TEMP") :
        result = mem.nextAvail(3) #3 = parabola
        #memAddress = mem.add_var("parabola", None, size, gv.currentScope)
        symtab.add_variable(gv.currentScope,"#"+gv.currentId+"B",BType, size, result)
        quad = ["=", "%"+B , [], result]
        gv.quadList.append(quad)
        gv.quadCount += 1
    else :
        raise Exception("Memory size exceeded in variables declaration")

    #For C value of parabola equation
    if mem.checkSizeAvail(size, "parabola", "TEMP") :
        result = mem.nextAvail(3) #3 = parabola
        #memAddress = mem.add_var("parabola", None, size, gv.currentScope)
        symtab.add_variable(gv.currentScope,"#"+gv.currentId+"C",CType, size, result)
        quad = ["=", "%"+C , [], result]
        gv.quadList.append(quad)
        gv.quadCount += 1
    else :
        raise Exception("Memory size exceeded in variables declaration")

#Returns true if the equation is valid for a parabola, false if it isn't
def verifyEquationParabola(equation):
    equation = equation[1:-1]
    if re.match(r'\s*y\s*\=\s*[-+]?([0-9]+(\.[0-9]+)?)?\s*x\s*\^\s*2\s*[+-]\s*([0-9]+(\.[0-9]+)?)?\s*x\s*[-+]\s*([0-9]+(\.[0-9]+)?)?\s*', equation):
        return True
    else:
        return False

#Create the quads to save the ellipse equation values (string) in memory
def saveEllipseEqValues(ellipseEquation):
    #Sanitize the input removing spaces and the string
    ellipseEquation = ellipseEquation.replace(" ", "")
    ellipseEquation = ellipseEquation.replace("\"", "")
    A=""
    B=""
    R=""
    i = 0
    j = 0
    #X^2/A^2 + Y^2/B^2 = R
    #Where A=B
    for x in ellipseEquation:
        if x == "/":
            j = i + 1
        elif x == "+":
            A= ellipseEquation[j:i]
            j = i+5
        elif x == "=":
            B = ellipseEquation[j:i]
            j = i + 1
            R = ellipseEquation[j:]
        i = i + 1
    if A == "":
        A = "1"
    if B == "":
        B = "1"
    size = 1
    #Check type of A, B and R for adding in the symbol table
    if "." in A:
        AType = "float"
    else:
        AType = "int"
    if "." in B:
        BType = "float"
    else:
        BType = "int"
    if "." in R:
        RType = "float"
    else:
        RType = "int"

    #For A value of circle equation
    if mem.checkSizeAvail(size, "ellipse", "TEMP") :
        result = mem.nextAvail(5) #5 = ellipse
        symtab.add_variable(gv.currentScope,"#"+gv.currentId+"A",AType, size, result)
        quad = ["=", "%"+A , [], result]
        gv.quadList.append(quad)
        gv.quadCount += 1
    else :
        raise Exception("Memory size exceeded in variables declaration")

    #For B value of circle equation
    if mem.checkSizeAvail(size, "ellipse", "TEMP") :
        result = mem.nextAvail(5) #5 = ellipse
        symtab.add_variable(gv.currentScope,"#"+gv.currentId+"B",BType, size, result)
        quad = ["=", "%"+B , [], result]
        gv.quadList.append(quad)
        gv.quadCount += 1
    else :
        raise Exception("Memory size exceeded in variables declaration")

    #For R value of ellipse equation
    if mem.checkSizeAvail(size, "ellipse", "TEMP") :
        result = mem.nextAvail(5) #5 = ellipse
        symtab.add_variable(gv.currentScope,"#"+gv.currentId+"R",RType, size, result)
        quad = ["=", "%"+R , [], result]
        gv.quadList.append(quad)
        gv.quadCount += 1
    else :
        raise Exception("Memory size exceeded in variables declaration")

#Returns true if the equation is valid for a ellipse, false if it isn't
def verifyEquationEllipse(equation):
    equation = equation[1:-1]
    if re.match(r'\s*x\s*\^\s*2\s*\/\s*([0-9]+(\.[0-9]+)?\s*)\+\s*y\s*\^\s*2\s*\/\s*([0-9]+(\.[0-9]+)?\s*)\=\s*1\s*', equation):
        return True
    else:
        return False

#Create the quads to save the hyperbola equation values (string) in memory
def saveHyperbolaEqValues(hyperbolaEquation):
    #Sanitize the input removing spaces and the string
    hyperbolaEquation = hyperbolaEquation.replace(" ", "")
    hyperbolaEquation = hyperbolaEquation.replace("\"", "")
    A=""
    B=""
    i = 0
    j = 0
    #X^2/A^2 - Y^2/B^2 = 1
    # OR
    #-X^2/A^2 + Y^2/B^2 = 1
    if hyperbolaEquation[0] == "+":
        A = "+"
    elif hyperbolaEquation[0] == "-":
        A = "-"

    for x in hyperbolaEquation:
        if x == "/":
            j = i + 1
        elif x == "Y" or x == "y":
            A = A + hyperbolaEquation[j:i-1]
            #Take sign of B
            if hyperbolaEquation[i-1] == "+":
                B = "+"
            elif hyperbolaEquation[i-1] == "-":
                B = "-"
            j = i + 4
        elif x == "=":
            B = B + hyperbolaEquation[j:i]
            j = i + 1
        i = i + 1
    if A == "" or A == "+":
        A = "1"
    elif A == "-":
        A = "-1"
    if B == "" or B == "+":
        B = "1"
    elif B == "-":
        B = "-1"
    size = 1
    #Check type of A, B and R for adding in the symbol table
    if "." in A:
        AType = "float"
    else:
        AType = "int"
    if "." in B:
        BType = "float"
    else:
        BType = "int"

    if (A[0] == "-" and B[0] == "-") or (A[0] != "-" and B[0] != "-"):
        #checks that signs are opposite
        #if (A > 0 and B > 0) or (A < 0 and B > 0):
        raise Exception("Invalid Hyperbola Syntax")

    #For A value of hyperbola equation
    if mem.checkSizeAvail(size, "hyperbola", "TEMP") :
        result = mem.nextAvail(6) #6 = hyperbola
        symtab.add_variable(gv.currentScope,"#"+gv.currentId+"A",AType, size, result)
        quad = ["=", "%"+A , [], result]
        gv.quadList.append(quad)
        gv.quadCount += 1
    else :
        raise Exception("Memory size exceeded in variables declaration")

    #For B value of hyperbola equation
    if mem.checkSizeAvail(size, "hyperbola", "TEMP") :
        result = mem.nextAvail(6) #6 = hyperbola
        symtab.add_variable(gv.currentScope,"#"+gv.currentId+"B",BType, size, result)
        quad = ["=", "%"+B , [], result]
        gv.quadList.append(quad)
        gv.quadCount += 1
    else :
        raise Exception("Memory size exceeded in variables declaration")

#Returns true if the equation is valid for a hyperbola, false if it isn't
def verifyEquationHyperbola(equation):
    equation = equation[1:-1]
    if re.match(r'\s*[+-]?\s*x\s*\^\s*2\s*\/\s*([0-9]+(\.[0-9]+)?\s*)[+-]\s*y\s*\^\s*2\s*\/\s*([0-9]+(\.[0-9]+)?\s*)\=\s*1\s*', equation):
        return True
    else:
        return False

def p_ASSIGN0D(t):
    'ASSIGN0D :'
    gv.currentArrAddressL = symtab.get_var_address(gv.currentScope,t[-1])

#For 1D arrays
def p_ASSIGN1D(t):
    'ASSIGN1D :'
    gv.currentArrAddressL = symtab.get_var_address(gv.currentScope,t[-4])
    pos = PilaOp.pop()
    posType = PTypes.pop()
    if posType != "int":
        raise Exception("Array Index must be an integer")
    if gv.currentArrAddressL < mem.memorySize or mem.memorySize*3 <= gv.currentArrAddressL < mem.memorySize*4 or mem.memorySize*6 <= gv.currentArrAddressL < mem.memorySize*7:
        rType = "int"
    elif mem.memorySize <= gv.currentArrAddressL < mem.memorySize*2 or mem.memorySize*4 <= gv.currentArrAddressL < mem.memorySize*5 or mem.memorySize*7 <= gv.currentArrAddressL < mem.memorySize*8:
        rType = "float"
    elif mem.memorySize*2 <= gv.currentArrAddressL < mem.memorySize*3 or mem.memorySize*5 <= gv.currentArrAddressL < mem.memorySize*6 or mem.memorySize*8 <= gv.currentArrAddressL < mem.memorySize*9:
        rType = "bool"
    result_Type = sem_cube[operators_dict["+"]][var_types_dict[posType]][var_types_dict[rType]]
    #Create a new temp variable for use in the '+' quad just below
    if mem.checkSizeAvail(1, result_Type, "TEMP"):
        result = mem.nextAvail(result_Type)
    else:
        raise Exception("Ran out of memory")

    #Create '+' quad for the BaseDir of Array + index of array
    quad = ["+","%" + str(gv.currentArrAddressL),pos,result]

    #Add the '+' quad to quadlist
    gv.quadList.append(quad)
    gv.quadCount += 1

    PAssign.append(result)

#To be completed: Arrays 2D
def p_ASSIGN2D(t):
    'ASSIGN2D :'
    #gv.currentArrAddressL = symtab.get_var_address(gv.currentScope,gv.currentId)
    #symtab.get_var_address(gv.currentScope,t[-1])
    #print("arr1d " + str(gv.currentArrAddress))
    #ID OPEN_SQUARE_BRACKET EXP CLOSE_SQUARE_BRACKET OPEN_SQUARE_BRACKET EXP CLOSE_SQUARE_BRACKET ASSIGN2D EQUALOP EXPRESSION SEMICOLON
    gv.currentArrAddressL = symtab.get_var_address(gv.currentScope,t[-7])
    # print(gv.currentArrAddressL)
    index2 = PilaOp.pop()
    index1 = PilaOp.pop()
    tipo2 = PTypes.pop()
    tipo1 = PTypes.pop()
    #PTypes.append(tipo)#regresar el tipo a la pila
    if tipo1 != "int":
        # if isinstance(index1,str):
        #     if index1[0] == '%':
        #         index1 = getCons(index1[1:])
        #         print("index1: " + str(index1))
        raise Exception("Array Index must be an integer")
    if tipo2 != "int":
        # if isinstance(index2,str):
        #     if index2[0] == '%':
        #         index2 = getCons(index2[1:])
        #         print("index2: " + str(index2))
        raise Exception("Array Index must be an integer")

    if gv.currentArrAddressL < mem.memorySize or mem.memorySize*3 <= gv.currentArrAddressL < mem.memorySize*4 or mem.memorySize*6 <= gv.currentArrAddressL < mem.memorySize*7:
        rType = "int"
    elif mem.memorySize <= gv.currentArrAddressL < mem.memorySize*2 or mem.memorySize*4 <= gv.currentArrAddressL < mem.memorySize*5 or mem.memorySize*7 <= gv.currentArrAddressL < mem.memorySize*8:
        rType = "float"
    elif mem.memorySize*2 <= gv.currentArrAddressL < mem.memorySize*3 or mem.memorySize*5 <= gv.currentArrAddressL < mem.memorySize*6 or mem.memorySize*8 <= gv.currentArrAddressL < mem.memorySize*9:
        rType = "bool"
    result_Type = sem_cube[operators_dict["+"]][var_types_dict[tipo1]][var_types_dict[rType]]
    #Create a new temp variable for use in the '+' quad just below
    if mem.checkSizeAvail(1, result_Type, "TEMP"):
        result = mem.nextAvail(result_Type)
    else:
        raise Exception("Ran out of memory")

    #calculate address
    rr = symtab.get_dims2(symtab.get_scope(gv.currentArrAddressL),t[-7])
    #calculate addresses skipped by rows (first index a[X][])
    quad = ["*", "%"+str(rr),index1,result]
    #Add the '*' quad to quadlist
    gv.quadList.append(quad)
    gv.quadCount += 1

    if mem.checkSizeAvail(1, result_Type, "TEMP"):
        result2 = mem.nextAvail(result_Type)
    else:
        raise Exception("Ran out of memory")

    #calculate addresses skipped by columns (second index a[][X])
    quad = ["+", result,index2,result2]
    #Add the '+' quad to quadlist
    gv.quadList.append(quad)
    gv.quadCount += 1

    if mem.checkSizeAvail(1, result_Type, "TEMP"):
        result3 = mem.nextAvail(result_Type)
    else:
        raise Exception("Ran out of memory")

    #Create '+' quad for the BaseDir of Array + index of array
    quad = ["+" ,result2 ,"%" + str(gv.currentArrAddressL),result3]

    #Add the '+' quad to quadlist
    gv.quadList.append(quad)
    gv.quadCount += 1

    PAssign.append(result3)

    #gv.currentArrAddressL = gv.currentArrAddressL + index1*symtab.get_dims2(gv.currentScope,gv.currentId) + index2

def p_FOR_LOOP(t):
    'FOR_LOOP : FOR_LOOP_KEYWORD saveCount OPEN_PARENTHESES ASSIGN forJump EXPRESSION_BOOL forExpression SEMICOLON ID EQUALOP EXP pop_exp CLOSE_PARENTHESES BLOCK forBack'

def p_saveCount(t):
    'saveCount :'
    gv.saveCount = gv.quadCount

def p_pop_exp(t):
    'pop_exp :'
    list_quad_aux = []
    quad_aux = gv.quadList.pop()
    gv.quadCount = gv.quadCount - 1
    while quad_aux[0] != "GOTOF": #quitamos quads hasta el GOTOF para agragarlos al rato
        list_quad_aux.append(quad_aux)
        quad_aux = gv.quadList.pop()
        gv.quadCount = gv.quadCount - 1
    gv.quadList.append(quad_aux)
    gv.quadCount = gv.quadCount + 1
    listPendingQuads.append(list_quad_aux)

def p_forExpression(t):
    'forExpression :'
    exp_type = PTypes.pop()
    if exp_type != "bool":
        raise Exception("ERROR: Type Mismatch!!! paso1, gotoF")
    else:
        result = PilaOp.pop()
        quad = ["GOTOF",result,[],-1]#genera cuadruplo
        gv.quadList.append(quad)#agrega cuadruplo
        gv.quadCount = gv.quadCount + 1#incrmenta cuenta de cuadruplos
        PJumps.append(gv.quadCount - 1)

def p_forBack(t):
    'forBack :'
    end = PJumps.pop()
    ret = PJumps.pop()
    list_quad_aux = listPendingQuads.pop()
    while list_quad_aux: #agrega quads del incremento que quitamos hace rato
        quad = list_quad_aux.pop()
        gv.quadList.append(quad)
        gv.quadCount = gv.quadCount + 1
    address = symtab.get_var_address(gv.currentScope,t[-6])
    quad2 = ["=",quad[3],[],address]
    gv.quadList.append(quad2)
    gv.quadCount = gv.quadCount + 1
    ###############################
    quad = ["GOTO",[],[],ret+1] #genera cuadruplo
    gv.quadList.append(quad) #agrega cuadruplo
    gv.quadCount = gv.quadCount + 1 #incrmenta cuenta de cuadruplos
    gv.quadList[end][3] = gv.quadCount

def p_forJump(t):
    'forJump :'
    PJumps.append(gv.quadCount - 1)

def p_MODULE(t):
    #'MODULE : ID modDef_paso1 OPEN_PARENTHESES I'
    '''MODULE : PROC_KEYWORD TYPE_P ID modDef_paso1 OPEN_PARENTHESES I
            | PROC_KEYWORD VOID_KEYWORD ID modDef_paso1 OPEN_PARENTHESES I'''

def p_modDef_paso1(t):
    'modDef_paso1 :'
    if t[-2] == "void":
        symtab.add_module(gv.currentId,"void", None)
        gv.currentScope = t[-1]
    else:
        if mem.checkSizeAvail(1,gv.currentType,"GLOBAL"):
            memAddress = mem.add_var(gv.currentType,None,1,"GLOBAL")
            symtab.add_module(gv.currentId,gv.currentType,memAddress)
            gv.currentScope = t[-1]
        else:
            raise Exception("Memory size exceeded in module declaration")

def p_I(t):
    '''I : TYPE_P ID modDef_paso2 J
            | TYPE_S ID modDef_paso2 J'''

def p_modDef_paso2(t):
    'modDef_paso2 :'
    size = 1
    if mem.checkSizeAvail(size, gv.currentType, gv.currentScope) :
        memAddress = mem.add_var(gv.currentType, None, size, gv.currentScope)
        symtab.add_variable(gv.currentScope,gv.currentId,gv.currentType, size, memAddress)
        PModDataTypes.append(gv.currentType)
        symtab.add_param_num(gv.currentScope,gv.currentId,len(PModDataTypes))#add param number for PARAM OpCode
    else :
        raise Exception("Memory size exceeded in variables declaration")

def p_J(t):
    '''J : COMMA I
	    | CLOSE_PARENTHESES modDef_paso4 modDef_paso5 modDef_paso6 BLOCK ret_glob modDef_paso7
	    | CLOSE_PARENTHESES modDef_paso4 VARS modDef_paso5 modDef_paso6 BLOCK ret_glob modDef_paso7'''

def p_modDef_paso4(t):
    'modDef_paso4 :'
    symtab.add_num_param(gv.currentScope)
    symtab.add_param_type_list(gv.currentScope,PModDataTypes)

def p_modDef_paso5(t):
    'modDef_paso5 :'
    symtab.add_num_vars(gv.currentScope)

def p_modDef_paso6(t):
    'modDef_paso6 :'
    symtab.add_num_quad(gv.currentScope, gv.quadCount)

def p_modDef_paso7(t):
    'modDef_paso7 :'
    quad = ["ENDPROC",[],[],[]]
    gv.quadList.append(quad)
    gv.quadCount = gv.quadCount + 1

    while PReturns:
        jump = PReturns.pop()
        #print(jump)
        gv.quadList[jump][3] = gv.quadCount - 1
    #Check if there are pending return quads and fill them
    # if gv.flagReturn:
    #     jump = PReturns.pop()
    #     PReturns.append(jump)
    #     while jump != "piso":
    #         print(jump)
    #         gv.quadList[jump][3] = gv.quadCount - 1
    #         jump = PReturns.pop()

def p_ret_glob(t):
    'ret_glob :'
    gv.currentScope = "GLOBAL"

def p_COLOR(t):
    '''COLOR : RED_KEYWORD
            | ORANGE_KEYWORD
			| YELLOW_KEYWORD
			| GREEN_KEYWORD
			| BLUE_KEYWORD
			| PURPLE_KEYWORD
            | BLACK_KEYWORD'''
    gv.plotColor = t[1]

def p_WRITE(t):
    '''WRITE : PRINT
            | PLOT
            | ROTATE
            | REFLECT'''

def p_PRINT(t):
    '''PRINT : PRINT_KEYWORD OPEN_PARENTHESES M
                | PRINT_KEYWORD OPEN_PARENTHESES MM'''

def p_M(t):
    '''M : CONS_STRING CLOSE_PARENTHESES SEMICOLON'''
    string = t[1][1:-1] #quitar comillas al inicio y al final
    quad = ["PRINT",[],[],string]
    gv.quadList.append(quad)
    gv.quadCount = gv.quadCount + 1

def p_MM(t):
    '''MM : EXPRESSION_BOOL CLOSE_PARENTHESES SEMICOLON'''
    if len(PTypes) > 0 :
        PTypes.pop()
        temp = PilaOp.pop()
        quad = ["PRINT",[],[],temp]
        gv.quadList.append(quad)
        gv.quadCount = gv.quadCount + 1

def p_WHILE_LOOP(t):
    'WHILE_LOOP : WHILE_LOOP_KEYWORD WHILE_paso1 OPEN_PARENTHESES EXPRESSION_BOOL CLOSE_PARENTHESES WHILE_paso2 BLOCK WHILE_paso3'

def p_WHILE_paso1(t):
    'WHILE_paso1 :'
    PJumps.append(gv.quadCount)

def p_WHILE_paso2(t):
    'WHILE_paso2 :'
    exp_type = PTypes.pop()
    if exp_type != "bool":
        raise Exception("ERROR: Type Mismatch!!! paso1, gotoF")
    else:
        result = PilaOp.pop()
        quad = ["GOTOF",result,[],-1] # Generate quad for gotof while
        gv.quadList.append(quad)
        gv.quadCount = gv.quadCount + 1 # Increment the quad count
        PJumps.append(gv.quadCount - 1)

def p_WHILE_paso3(t):
    'WHILE_paso3 :'
    end = PJumps.pop()
    ret = PJumps.pop()
    quad = ["GOTO",[],[],ret] #genera cuadruplo
    gv.quadList.append(quad) #agrega cuadruplo
    gv.quadCount = gv.quadCount + 1 #incrmenta cuenta de cuadruplos
    gv.quadList[end][3] = gv.quadCount #fill


def p_PLOT(t):
    '''PLOT : PLOT_KEYWORD OPEN_PARENTHESES ID save_name COMMA COLOR flagColor CLOSE_PARENTHESES SEMICOLON
            | PLOT_KEYWORD OPEN_PARENTHESES ID save_name CLOSE_PARENTHESES SEMICOLON'''
    address = symtab.get_var_address(gv.currentScope,gv.plotName)
    if mem.memorySize*11 <= address < mem.memorySize*19:
        if gv.flagColor:
            quad = ["PLOT",address,gv.plotColor,[]]
            gv.flagColor = False
        else:
            quad = ["PLOT",address,[],[]]
        gv.quadList.append(quad)
        gv.quadCount = gv.quadCount + 1
    else:
        raise Exception("ERROR: PLOT needs Parabola, Circle, Ellipse or Hyperbola input.")

def p_ROTATE(t):
    '''ROTATE : ROTATE_KEYWORD OPEN_PARENTHESES ID save_name CLOSE_PARENTHESES SEMICOLON'''
    address = symtab.get_var_address(gv.currentScope,gv.plotName)
    if mem.memorySize*11 <= address < mem.memorySize*19:
        quad = ["ROTATE",address,[],[]]
        gv.quadList.append(quad)
        gv.quadCount = gv.quadCount + 1
    else:
        raise Exception("ERROR: ROTATE needs Parabola, Circle, Ellipse or Hyperbola input.")

def p_REFLECT(t):
    '''REFLECT : REFLECT_KEYWORD OPEN_PARENTHESES ID save_name CLOSE_PARENTHESES SEMICOLON'''
    address = symtab.get_var_address(gv.currentScope,gv.plotName)
    if mem.memorySize*11 <= address < mem.memorySize*19:
        quad = ["REFLECT",address,[],[]]
        gv.quadList.append(quad)
        gv.quadCount = gv.quadCount + 1
    else:
        raise Exception("ERROR: REFLECT needs Parabola, Circle, Ellipse or Hyperbola input.")

def p_save_name(t):
    'save_name :'
    gv.plotName = t[-1]

def p_flagColor(t):
    'flagColor :'
    gv.flagColor = True

def p_CONDITION(t):
    'CONDITION : IF_STATEMENT OPEN_PARENTHESES EXPRESSION_BOOL CLOSE_PARENTHESES gotoFcond N'

def p_gotoFcond(t):
    'gotoFcond :'
    exp_type = PTypes.pop()
    if exp_type != "bool":
        raise Exception("ERROR: Type Mismatch!!! paso1, gotoF")
    else:
        result = PilaOp.pop()
        quad = ["GOTOF",result,[],-1]#genera cuadruplo
        gv.quadList.append(quad)#agrega cuadruplo
        gv.quadCount = gv.quadCount + 1 #incrmenta cuenta de cuadruplos
        PJumps.append(gv.quadCount - 1)

def p_N(t):
    '''N : BLOCK ELSE_STATEMENT gotoElse BLOCK endif
            | BLOCK endif'''

def p_gotoElse(t):
    'gotoElse :'
    quad = ["GOTO",[],[],-1] #genera cuadruplo
    gv.quadList.append(quad) #agrega cuadruplo
    gv.quadCount = gv.quadCount + 1 #incrmenta cuenta de cuadruplos
    falso = PJumps.pop()
    PJumps.append(gv.quadCount - 1)
    gv.quadList[falso][3] = gv.quadCount

def p_endif(t):
    'endif :'
    end = PJumps.pop()
    gv.quadList[end][3] = gv.quadCount

# def p_EXPRESSION_OP(t):
#     '''EXPRESSION_OP : EXPRESSION_BOOL'''

def p_EXPRESSION_BOOL(t):
    '''EXPRESSION_BOOL : EXPRESSION paso2bool BBB
            | EXPRESSION paso2bool
            | NOT_KEYWORD paso1bool EXPRESSION_BOOL pasoNotBool
            | NOT_KEYWORD paso1bool EXPRESSION_BOOL pasoNotBool BBB'''

def p_BBB(t):
    '''BBB : AND_KEYWORD paso1bool EXPRESSION_BOOL
            | OR_KEYWORD paso1bool EXPRESSION_BOOL'''

def p_paso1bool(t):
    'paso1bool :'
    POper.append(t[-1]) # Append left side of expression between boolean operator
    PTypes.append("bool")

def p_paso2bool(t):
    'paso2bool :'
    if POper:
        temp = POper.pop()
        POper.append(temp)
        if temp == "and" or temp == "or":
            right_op = PilaOp.pop()
            right_type = PTypes.pop()
            left_op = PilaOp.pop()
            left_type = PTypes.pop()
            operator = POper.pop()
            result_Type = sem_cube[operators_dict[operator]][var_types_dict[left_type]][var_types_dict[right_type]]
            if result_Type != -1 :
                if mem.checkSizeAvail(1, result_Type, "TEMP"):
                    result = mem.nextAvail(result_Type)
                else:
                    raise Exception("Ran out of memory")
                quad = [operator,left_op,right_op,result]# genera cuadruplo
                gv.quadList.append(quad)# agrega cuadruplo
                gv.quadCount = gv.quadCount + 1# incrmenta cuenta de cuadruplos
                PilaOp.append(result)
                if result_Type == 0:
                    PTypes.append("int")
                elif result_Type == 1:
                    PTypes.append("float")
                elif result_Type == 2:
                    PTypes.append("bool")
                #if any operand were a temporal space, return it to AVAIL
            else:
                raise Exception("ERROR: Type Mismatch!!! Boolean operation invalid")

def p_pasoNotBool(t):
    'pasoNotBool :'
    if POper:
        temp = POper.pop()
        POper.append(temp)
        if temp == "not":
            right_op = PilaOp.pop()
            right_type = PTypes.pop()
            #We don't require left operator, 'NOT' is unary
            operator = POper.pop()
            result_Type = sem_cube[operators_dict[operator]][2][var_types_dict[right_type]]
            if result_Type != -1 :
                if mem.checkSizeAvail(1, result_Type, "TEMP"):
                    result = mem.nextAvail(result_Type)
                else:
                    raise Exception("Ran out of memory")
                quad = [operator, right_op, [], result]
                gv.quadList.append(quad)# agrega cuadruplo
                gv.quadCount = gv.quadCount + 1# incrmenta cuenta de cuadruplos
                PilaOp.append(result)
                if result_Type == 0:
                    PTypes.append("int")
                elif result_Type == 1:
                    PTypes.append("float")
                elif result_Type == 2:
                    PTypes.append("bool")
                    #if any operand were a temporal space, return it to AVAIL
            else:
                raise Exception("ERROR: Type Mismatch!!! Boolean operation invalid")

def p_EXPRESSION(t):
    '''EXPRESSION : EXP RELOP paso8 EXP paso9
            | EXP'''
# def p_EXPRESSION(t):
#     '''EXPRESSION : EXP paso9 RRR
#             | EXP paso9'''

# def p_RRR(t):
#     'RRR : RELOP paso8 EXPRESSION'

def p_paso8(t):
    'paso8 :'
    POper.append(t[-1])

def p_paso9(t):
    'paso9 :'
    if POper:
        temp = POper.pop()
        POper.append(temp)
        if temp == ">" or temp == "<" or temp == "<>" or temp == "==" :
            right_op = PilaOp.pop()
            right_type = PTypes.pop()
            left_op = PilaOp.pop()
            left_type = PTypes.pop()
            operator = POper.pop()
            result_Type = sem_cube[operators_dict[operator]][var_types_dict[left_type]][var_types_dict[right_type]]
            if result_Type != -1 :
                if mem.checkSizeAvail(1, result_Type, "TEMP"):
                    result = mem.nextAvail(result_Type)
                else:
                    raise Exception("Ran out of memory")
                quad = [operator,left_op,right_op,result]#genera cuadruplo
                gv.quadList.append(quad)#agrega cuadruplo
                gv.quadCount = gv.quadCount + 1#incrmenta cuenta de cuadruplos
                PilaOp.append(result)
                if result_Type == 0:
                    PTypes.append("int")
                elif result_Type == 1:
                    PTypes.append("float")
                elif result_Type == 2:
                    PTypes.append("bool")
                #if any operand were a temporal space, return it to AVAIL
            else:
                raise Exception("ERROR: Type Mismatch!!! paso4")

def p_EXP(t):
    '''EXP : TERM paso4 P
            | TERM paso4'''

def p_paso4(t):
    'paso4 :'
    if POper:
        temp = POper.pop()
        POper.append(temp)

        if temp == "+" or temp == "-" :
            right_op = PilaOp.pop()
            right_type = PTypes.pop()
            left_op = PilaOp.pop()
            left_type = PTypes.pop()
            operator = POper.pop()
            result_Type = sem_cube[operators_dict[operator]][var_types_dict[left_type]][var_types_dict[right_type]]
            if result_Type != -1 :
                if mem.checkSizeAvail(1, result_Type, "TEMP"):
                    result = mem.nextAvail(result_Type)
                else:
                    raise Exception("Ran out of memory")
                #address1
                quad = [operator,left_op,right_op,result]
                gv.quadList.append(quad)
                gv.quadCount = gv.quadCount + 1#incrmenta cuenta de cuadruplos
                PilaOp.append(result)
                if result_Type == 0:
                    PTypes.append("int")
                elif result_Type == 1:
                    PTypes.append("float")
                elif result_Type == 2:
                    PTypes.append("bool")
                #if any operand were a temporal space, return it to AVAIL
            else:
                raise Exception("ERROR: Type Mismatch!!! paso4")

def p_P(t):
    '''P : PLUSOP paso2a EXP
			| MINUSOP paso2b EXP'''

def p_paso2a(t):
    'paso2a :'
    POper.append("+")

def p_paso2b(t):
    'paso2b :'
    POper.append("-")

def p_TERM(t):
    '''TERM : FACTOR paso5 Q
            | FACTOR paso5'''

def p_paso5(t):
    'paso5 :'
    if POper:
        temp = POper.pop()
        POper.append(temp)

        if temp == "*" or temp == "/" :
            right_op = PilaOp.pop()
            right_type = PTypes.pop()
            left_op = PilaOp.pop()
            left_type = PTypes.pop()
            operator = POper.pop()
            result_Type = sem_cube[operators_dict[operator]][var_types_dict[left_type]][var_types_dict[right_type]]
            if result_Type != -1 :
                if mem.checkSizeAvail(1, result_Type, "TEMP"):
                    result = mem.nextAvail(result_Type)
                else:
                    raise Exception("Ran out of memory")
                quad = [operator,left_op,right_op,result]
                gv.quadList.append(quad)
                gv.quadCount = gv.quadCount + 1#incrmenta cuenta de cuadruplos
                PilaOp.append(result)
                if result_Type == 0:
                    PTypes.append("int")
                elif result_Type == 1:
                    PTypes.append("float")
                elif result_Type == 2:
                    PTypes.append("bool")
                #if any operand were a temporal space, return it to AVAIL
            else:
                raise Exception("ERROR: Type Mismatch!!! paso4")

def p_Q(t):
    '''Q : TIMESOP paso3a TERM
			| DIVIDEOP paso3b TERM'''

def p_paso3a(t):
    'paso3a :'
    POper.append("*")

def p_paso3b(t):
    'paso3b :'
    POper.append("/")

def p_FACTOR(t):
    '''FACTOR : OPEN_PARENTHESES paso6 EXPRESSION_BOOL CLOSE_PARENTHESES paso7
            | VAR_CONS'''

def p_paso6(t):
    'paso6 :'
    POper.append("(")

def p_paso7(t):
    'paso7 :'
    POper.pop()

def p_VAR_CONS(t):
    '''VAR_CONS : ID paso1a
			| PROC_KEYWORD ID S
            | ID SC
			| CONS_INT paso1b
            | CONS_FLOAT paso1c
            | MINUSOP addminus CONS_INT paso1b
            | MINUSOP addminus CONS_FLOAT paso1c
            | TRUE_KEYWORD paso1d
            | FALSE_KEYWORD paso1d'''

def p_addminus(t):
    'addminus :'
    gv.minusFlag = True

def p_paso1a(t):
    'paso1a :'
    address = symtab.get_var_address(gv.currentScope,t[-1])
    PilaOp.append(address)
    PTypes.append(symtab.SYM_TABLE[gv.currentScope][t[-1]]["#type"])

def p_paso1b(t):
    'paso1b :'
    if gv.minusFlag:
        op = "%-" + t[-1]
        gv.minusFlag = False
    else:
        op = "%" + t[-1]
    PilaOp.append(op)
    PTypes.append("int")

def p_paso1c(t):
    'paso1c :'
    if gv.minusFlag:
        op = "%-" + t[-1]
        gv.minusFlag = False
    else:
        op = "%" + t[-1]
    PilaOp.append(op)
    PTypes.append("float")

def p_paso1d(t):
    'paso1d :'
    PilaOp.append(t[-1])
    PTypes.append("bool")

def p_S(t):
    '''S : modCall_paso1 modCall_paso2 OPEN_PARENTHESES SS'''

def p_SC(t):
    '''SC : OPEN_SQUARE_BRACKET EXP CLOSE_SQUARE_BRACKET arrCall1
            | OPEN_SQUARE_BRACKET EXP CLOSE_SQUARE_BRACKET OPEN_SQUARE_BRACKET EXP CLOSE_SQUARE_BRACKET arrCall2'''

def p_arrCall1(t):
    'arrCall1 :'
    gv.currentArrAddress = symtab.get_var_address(gv.currentScope,t[-4])
    pos = PilaOp.pop()
    posType = PTypes.pop()
    posFlag = False #check if pos is a constant
    if isinstance(pos,str):
        if pos[0] == '%':
            pos = getCons(pos[1:])
            posFlag = True

    if gv.currentArrAddress < mem.memorySize or mem.memorySize*3 <= gv.currentArrAddress < mem.memorySize*4 or mem.memorySize*6 <= gv.currentArrAddress < mem.memorySize*7:
        rType = "int"
    elif mem.memorySize <= gv.currentArrAddress < mem.memorySize*2 or mem.memorySize*4 <= gv.currentArrAddress < mem.memorySize*5 or mem.memorySize*7 <= gv.currentArrAddress < mem.memorySize*8:
        rType = "float"
    elif mem.memorySize*2 <= gv.currentArrAddress < mem.memorySize*3 or mem.memorySize*5 <= gv.currentArrAddress < mem.memorySize*6 or mem.memorySize*8 <= gv.currentArrAddress < mem.memorySize*9:
        rType = "bool"

    #result_Type = sem_cube[operators_dict["+"]][var_types_dict[posType]][var_types_dict["int"]]
    result_Type = sem_cube[operators_dict["+"]][var_types_dict[posType]][var_types_dict[rType]]
    if mem.checkSizeAvail(1, result_Type, "TEMP"):
        result = mem.nextAvail(result_Type)
    else:
        raise Exception("Ran out of memory")
    #address1
    left_op = "%" + str(gv.currentArrAddress)
    if (mem.memorySize*6 <= pos < mem.memorySize*9 or pos == symtab.get_var_address(gv.currentScope,gv.currentId)) and not posFlag:
        quad = ["+",left_op,pos,result]
    else:
        quad = ["+",left_op,"%" + str(pos),result]
    gv.quadList.append(quad)
    gv.quadCount = gv.quadCount + 1#incrmenta cuenta de cuadruplos
    if mem.checkSizeAvail(1, result_Type, "TEMP"):
        result2 = mem.nextAvail(result_Type)
    else:
        raise Exception("Ran out of memory")

    quad = ["ACC",result,[],result2]#Acceso a valor en result (result es una direccion)
    gv.quadList.append(quad)
    gv.quadCount = gv.quadCount + 1#incrmenta cuenta de cuadruplos

    PilaOp.append(result2)
    if result_Type == 0:
        PTypes.append("int")
    elif result_Type == 1:
        PTypes.append("float")
    elif result_Type == 2:
        PTypes.append("bool")

    if posFlag:
        x = 1
    else:
        x = []
    quad = ["VER",pos,x,symtab.get_size(gv.currentScope,t[-4])]
    gv.quadList.append(quad)
    gv.quadCount = gv.quadCount + 1#incrmenta cuenta de cuadruplos

def p_arrCall2(t):
    'arrCall2 :'
    gv.currentArrAddress = symtab.get_var_address(gv.currentScope,t[-7])

    index2 = PilaOp.pop()
    index1 = PilaOp.pop()
    tipo2 = PTypes.pop()
    tipo1 = PTypes.pop()

    pos1Flag = False #check if index1 is a constant
    if isinstance(index1,str):
        if index1[0] == '%':
            index1 = getCons(index1[1:])
            pos1Flag = True
    pos2Flag = False #check if index2 is a constant
    if isinstance(index2,str):
        if index2[0] == '%':
            index2 = getCons(index2[1:])
            pos2Flag = True

    #print(pos1Flag,pos2Flag)

    if gv.currentArrAddress < mem.memorySize or mem.memorySize*3 <= gv.currentArrAddress < mem.memorySize*4 or mem.memorySize*6 <= gv.currentArrAddress < mem.memorySize*7:
        rType = "int"
    elif mem.memorySize <= gv.currentArrAddress < mem.memorySize*2 or mem.memorySize*4 <= gv.currentArrAddress < mem.memorySize*5 or mem.memorySize*7 <= gv.currentArrAddress < mem.memorySize*8:
        rType = "float"
    elif mem.memorySize*2 <= gv.currentArrAddress < mem.memorySize*3 or mem.memorySize*5 <= gv.currentArrAddress < mem.memorySize*6 or mem.memorySize*8 <= gv.currentArrAddress < mem.memorySize*9:
        rType = "bool"

    #result_Type = sem_cube[operators_dict["+"]][var_types_dict[posType]][var_types_dict["int"]]
    result_Type = sem_cube[operators_dict["+"]][var_types_dict[tipo1]][var_types_dict[rType]]
    if mem.checkSizeAvail(1, result_Type, "TEMP"):
        result = mem.nextAvail(result_Type)
    else:
        raise Exception("Ran out of memory")
    #address1
    #left_op = "%" + str(gv.currentArrAddress)
    #calculate address
    left_op = symtab.get_dims2(symtab.get_scope(gv.currentArrAddress),t[-7])
    #calculate addresses skipped by rows (first index a[X][])
    #left_op = "%" + str(gv.currentArrAddress)
    #print("index1 :",index1)
    #if (mem.memorySize*6 <= index1 < mem.memorySize*9 or index1 == symtab.get_var_address(gv.currentScope,t[-7])) and not pos1Flag:
    if not pos1Flag:
        quad = ["*","%"+str(left_op),index1,result]
    else:
        quad = ["*","%"+str(left_op),"%" + str(index1),result]
    gv.quadList.append(quad)
    gv.quadCount = gv.quadCount + 1#incrmenta cuenta de cuadruplos

    #calculate addresses skipped by columns (second index a[][X])
    if mem.checkSizeAvail(1, result_Type, "TEMP"):
        result2 = mem.nextAvail(result_Type)
    else:
        raise Exception("Ran out of memory")
    if (mem.memorySize*6 <= index2 < mem.memorySize*9 or index2 == symtab.get_var_address(gv.currentScope,gv.currentId)) and not pos2Flag:
    #if not pos2Flag:
        quad = ["+",result,index2,result2]
    else:
        quad = ["+",result,"%" + str(index2),result2]
    gv.quadList.append(quad)
    gv.quadCount = gv.quadCount + 1#incrmenta cuenta de cuadruplos

    #added base address
    if mem.checkSizeAvail(1, result_Type, "TEMP"):
        result3 = mem.nextAvail(result_Type)
    else:
        raise Exception("Ran out of memory")
    #if (mem.memorySize*6 <= index2 < mem.memorySize*9 or index2 == symtab.get_var_address(gv.currentScope,gv.currentId)) and not pos2Flag:
    #    quad = ["+",result,index2,result2]
    #else:
    #    quad = ["+",result,"%" + str(index2),result2]
    quad = ["+","%" + str(gv.currentArrAddress),result2,result3]
    gv.quadList.append(quad)
    gv.quadCount = gv.quadCount + 1#incrmenta cuenta de cuadruplos

    #create ACC to access value in address
    if mem.checkSizeAvail(1, result_Type, "TEMP"):
        result4 = mem.nextAvail(result_Type)
    else:
        raise Exception("Ran out of memory")

    quad = ["ACC",result3,[],result4]#Acceso a valor en result (result es una direccion)
    gv.quadList.append(quad)
    gv.quadCount = gv.quadCount + 1#incrmenta cuenta de cuadruplos

    PilaOp.append(result4)
    if result_Type == 0:
        PTypes.append("int")
    elif result_Type == 1:
        PTypes.append("float")
    elif result_Type == 2:
        PTypes.append("bool")

    if pos1Flag:
        x = 1
    else:
        x = []
    #quad = ["VER",index1,x,symtab.get_size(gv.currentScope,t[-7])]
    quad = ["VER",index1,x,symtab.get_dims1(symtab.get_scope(gv.currentArrAddress),t[-7])]
    gv.quadList.append(quad)
    gv.quadCount = gv.quadCount + 1#incrmenta cuenta de cuadruplos
    if pos2Flag:
        x = 1
    else:
        x = []
    quad = ["VER",index2,x,symtab.get_dims2(symtab.get_scope(gv.currentArrAddress),t[-7])]
    gv.quadList.append(quad)
    gv.quadCount = gv.quadCount + 1#incrmenta cuenta de cuadruplos

def p_SS(t):
    '''SS : EXP modCall_paso3 SSS
            | EXP modCall_paso3 COMMA modCall_paso4 SS
			| SSS'''

def p_SSS(t):
    '''SSS : modCall_paso5 CLOSE_PARENTHESES modCall_paso6'''

def p_modCall_paso1(t):
    'modCall_paso1 :'
    if symtab.mod_exist(t[-1]) :
        gv.currentModCall = t[-1]
    else :
        raise Exception("ERROR: Calling inexistent module!! paso1")

def p_modCall_paso2(t):
    'modCall_paso2 :'
    quad = ["ERA", t[-2], [], []]
    gv.paramCount = 1
    gv.quadList.append(quad)
    gv.quadCount = gv.quadCount + 1

def p_modCall_paso3(t):
    'modCall_paso3 :'
    Argument = PilaOp.pop()
    ArgumentType = PTypes.pop()
    TL = symtab.get_param_type_list(gv.currentModCall)#jala lista de tipos de ese modulo
    if ArgumentType == TL[gv.paramCount-1] :
        quad = ["PARAM",Argument,[],gv.paramCount]
        gv.quadList.append(quad)
        gv.quadCount = gv.quadCount + 1
    else :
        raise Exception("ERROR: Incorrect parameter type!! paso3")

def p_modCall_paso4(t):
    'modCall_paso4 :'
    TL = symtab.get_param_type_list(gv.currentModCall)#jala lista de tipos de ese modulo
    if(gv.paramCount == len(TL)) :
        raise Exception("ERROR: Number of parameters is inconsistent!! paso1")
    else :
        gv.paramCount = gv.paramCount + 1

def p_modCall_paso5(t):
    'modCall_paso5 :'
    #verify last parameter points to null
    TL = symtab.get_param_type_list(gv.currentModCall)#jala lista de tipos de ese modulo
    if len(TL) == gv.paramCount :
        gv.paramCount = 0 #limpia para siguiente llamada
    else :
        raise Exception("ERROR: Number of parameters is inconsistent!! paso1")

def p_modCall_paso6(t):
    'modCall_paso6 :'
    quad = ["GOSUB",gv.currentModCall,[],symtab.get_num_quad(gv.currentModCall)]
    gv.quadList.append(quad)
    gv.quadCount = gv.quadCount + 1

    if symtab.get_return_type_module(gv.currentModCall) != "void":
        if mem.checkSizeAvail(1,symtab.get_return_type_module(gv.currentModCall),gv.currentScope):
            #get next available temporal variable of the type of the function called
            result = mem.nextAvail(symtab.get_return_type_module(gv.currentModCall))
            #print(symtab.get_return_type_module(gv.currentModCall))
            #symtab.add_variable(gv.currentScope,"#"+gv.currentId+"A",AType, size, result)
            modAddress = symtab.return_mod_address(gv.currentModCall)
            quad = ["=", modAddress, [], result]
            gv.quadList.append(quad)
            gv.quadCount = gv.quadCount + 1
            PilaOp.append(result)

def p_error(p):
    if p:
        # get formatted representation of stack
        stack_state_str = ' '.join([symbol.type for symbol in parser.symstack][1:])

        print('Syntax error in input! Parser State:{} {} . {}'
            .format(parser.state,
                stack_state_str,
                p))
    else:
        print("Syntax error at EOF")

import ply.yacc as yacc
import os
parser = yacc.yacc()
fileName = input("Enter a file name \n")
file = open(fileName, "r")
code = ""
#Add all lines to one string for parsing
for line in file:
    try:
        code += line
    except EOFError:
        break

#Finally parse the input code
try:
    parser.parse(code)
finally:
print("Parsing complete")
