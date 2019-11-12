import ply.lex as lex
import ply.yacc as yacc
import sys

# lex tokenizes string 
# (looking at every char in the string and figuring out what it is)

# yacc is parser generator
# (we provide it grammar to create a parser)

# sys is just python standard stuff

# ---------------------------------

# list of tokens
tokens = [

    'INT',
    'FLOAT',
    'NAME',
    'PLUS',
    'MINUS',
    'MULTIPLY',
    'DIVIDE',
    'POWER',
    'MODULO',
    'EQUALS',

]

# "t_" is ply thing, when it sees t_ and variable ply
# it is going assume that i'm telling the lex what the token actually looks like

t_PLUS = r'\+'
t_MINUS = r'\-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'\/'
t_POWER = r'\^'
t_MODULO = r'\%'
t_EQUALS = r'\='

# When ply sees spaces, it ignores it
# 2 2 3 + - 
# equals
# 2 2 3+-
t_ignore = r' '

# ----------------------------

# int, float and name is more complicated to make function for them
# float = any integer fallowed by a dot fallowed by a integer (with 1 or more characters)
#           (asteriks wouldn't work because otherwise a float would be "3." which is illegal)
# float has to be above int so first it looks for float instead of integers
#           (ex:    123.43 -> recognises int(123) and throws error with (.43))
def t_FLOAT(t):
    r'-?\d\.\d+'
    t.value = float(t.value)
    return t

# int = any integer (with 1 or more characters)
def t_INT(t):
    r'-?\d+'
    t.value = int(t.value)
    return t

# name = any character from a-z or A-Z or underscore "_" and the first character can't be a number
def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = 'NAME'
    return t

# when error spotted it runs this action
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# ----------------------------

# Creating a lexer
lexer = lex.lex()

# order of calculating, lower have higher priority
precedence = (

    ('left', 'PLUS', 'MINUS'),
    ('left', 'MULTIPLY', 'DIVIDE'),
    ('left', 'POWER')

)

# Creating a parser, that takes parameter p
#       p is Python tuple, ex: p0 = calc
#                              p1 = expression  (whatever the calc will evaluate)
#                              p1 = empty       (emptyline -> no parser errors for empty strings)
#       this means a calc can be an expression or it can be empty
# Parser creates a tree using the grammar we specify
def p_calc(p):
    '''
    calc : expression
         | var_assign
         | empty
    '''
    print(run(p[1]))
    # print(p[1])

def p_var_assign(p):
    '''
    var_assign : NAME expression EQUALS
               | NAME NAME EQUALS
    '''
    p[0] = ('=', p[1], p[2])

# Allows to stack expressions (ex: 1 2 3 4 + * -)
# Creates expression in order like {'-', 1, {'*', 2, {'+', 3, 4}}}
def p_expression(p):
    '''
    expression : expression expression POWER
               | expression expression MODULO
               | expression expression MULTIPLY
               | expression expression DIVIDE
               | expression expression PLUS
               | expression expression MINUS
    '''
    p[0] = (p[3], p[1], p[2])

# If the input will not be an INT or FLOAT it will print Syntax error
def p_expression_int_float(p):
    '''
    expression : INT
               | FLOAT
    '''
    p[0] = p[1]

def p_expression_var(p):
    '''
    expression : NAME
    '''
    p[0] = ('var', p[1])

def p_error(p):
    if hasattr(p, 'type'):
        print("Syntax Error: misplaced sign:", p.value)
    else:
        print("Syntax Error: too many digits.")

def p_empty(p):
    '''
    empty : 
    '''
    p[0] = None

# ----------------------------

parser = yacc.yacc()
env = {}

def run(p):
    global env
    if type(p) == tuple:
        if p[0] == '+':
            return run(p[1]) + run(p[2])
        if p[0] == '-':
            return run(p[1]) - run(p[2])
        if p[0] == '*':
            return run(p[1]) * run(p[2])
        if p[0] == '/':
            if p[2] == 0:
                return 'Math Error: Dividing zero'
            return run(p[1]) / run(p[2])
        if p[0] == '^':
            return run(p[1]) ** run(p[2])
        if p[0] == '%':
            if p[2] == 0:
                return 'Math Error: Dividing zero'
            return run(p[1]) % run(p[2])
        if p[0] == '=':
            env[p[1]] = run(p[2])
            print(env)
        if p[0] == 'var':
            if p[1] not in env:
                return 'Undeclared variable found!'
            else:
                return env[p[1]]
    else: 
        return p

# ----------------------------

print("\n[Polish Reversed Notation Calculator]")
print("Available Options: (*, /, +, -, %, variables)\n")
while True:
    # Scans for input, breaks with (CTRL + C) in the console
    try:
        s = input('>> ')
    except EOFError:
        break
    parser.parse(s)

# ----------------------------

# Sample inputs:
# >> 2 3+4*
# 20

# >> 1 2 3 4 + * -
# -13

# >> -1 2 -3 4 + * -
# -3

# >> 8 -7 6 -5 4 * -3 % / - +
# 4.0

# >> 2 3 2 ^ ^
# 512

# >> 2 3+*
# Syntax Error: misplaced sign: *

# >> 2 3 4 +
# Syntax Error: too many digits.

# >> 2.4 3+
# 2.4 3+
# disclaimer: added float values

# DO ZROBIENIA:

# >> 5 5 + + 5 5 - -
# Syntax Error: misplaced sign: +
# Syntax Error: misplaced sign: -
# None

# >> 5 5 + + 5 5 -
# 5 5 + + 5 5 -
# Syntax Error: misplaced sign: +
# 0

# >> 2 0 %
# Math Error: Dividing zero

# >> 2 0 /
# Math Error: Dividing zero

# >> 0 0 /
# Math Error: Dividing zero

# lexer.input("1 2 3 4 + * -")
# while True:
#     tok = lexer.token()
#     if not tok:
#         break
#     print(tok)