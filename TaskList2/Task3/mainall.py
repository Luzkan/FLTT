import ply.lex as lex
import ply.yacc as yacc
import sys

# List of token names
tokens = [
    'BACKSLASHTRICKS',
    'STRING',
    'EXP',
    'ALLCOMMENTS',
    'SMTHELSE',
]

# Expression for those tokens
def t_STRING(t):
    r'"([\s\S\n])*?"\n*'
    return t

def t_EXP(t):
    r'<([\s\S\n])*?>\n*'
    return t

def t_BACKSLASHTRICKS(t):
    r'(\/\\\n\/.*)|(\/\/.*\\\n.*)'
    t.value = '\n'
    return t

def t_ALLCOMMENTS(t):
    r'(/\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+/)|(//.*)'
    t.value = '\n'
    return t

def t_SMTHELSE(t):
    r'[\s\S]'
    return t

# Error handling rule
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

# --------------------------------------------------

# Get the token map from the lexer
def p_expression_string(p):
    'expression : STRING'
    p[0] = p[1]

def p_expression_backslashtricks(p):
    'expression : BACKSLASHTRICKS'
    p[0] = p[1]

def p_expression_exp(p):
    'expression : EXP'
    p[0] = p[1]

def p_expression_allcomments(p):
    'expression : ALLCOMMENTS'
    p[0] = p[1]

def p_expression_SMTHELSE(p):
    'expression : SMTHELSE'
    p[0] = p[1]

# Stacking expressions
def p_expression(p):
    '''
    expression : expression expression
    '''
    p[0] = p[1] + p[2]

# Error rule
def p_error(p):
    print("Syntax Error!", p)

# Build the parser
parser = yacc.yacc()

# Opens a file
f = open(".\\Task3\\testhard.txt","r", encoding="utf8")
if f.mode == 'r':
	contents = f.read()

# Do the job
while True:
   try:
       s = contents
   except EOFError:
       break
   if not s: continue
   result = parser.parse(s)
   break
   
# Write effects to new file
g = open(".\\Task3\\testhardresult.txt","w+")
g.write(result)
g.close() 