import ply.lex as lex
import ply.yacc as yacc
import sys

# List of token names
tokens = [
    'COMMENT',
    'SMTHELSE',
]

# Expression for those tokens
def t_COMMENT(t):
    r'<!--([\s\S])*?-->\n*'
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
def p_expression_comment(p):
    'expression : COMMENT'
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
f = open(".\\Task2\\preformatted.txt","r", encoding="utf8")
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
g = open(".\\Task2\\formatted.txt","w+")
g.write(result)
g.close() 