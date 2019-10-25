import ply.lex as lex
import ply.yacc as yacc
import sys

deleteDocComment = False

# List of token names
tokens = [
    'ALLCOMMENTS',
    'DOC',
    'SMTHELSE',
]

# Expression for those tokens
# Marcel: My god this regular expression took my way beyond the time I ever wish for :( 
def t_DOC(t):
    r'(\/\*((\*[^/])|\!)([^*]|[\r\n]|(\*+([^*/]|[\r\n]))|(\/\+([^*/]|[\r\n])))*\*+\/)|(\/\/(\!|\/).*)'
    global deleteDocComment
    if(deleteDocComment):
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
def p_expression_doc(p):
    'expression : DOC'
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
f = open(".\\Task3\\preformatted.txt","r", encoding="utf8")
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
g = open(".\\Task3\\formatted.txt","w+")
g.write(result)
g.close() 