import ply.lex as lex
import ply.yacc as yacc
import sys

# List of token names
tokens = [
    'CHAR',
    'NEWLINE',
    'SPACE',
]

# Expression for those tokens
newlinesNum = 0
def t_NEWLINE(t):
    r'\s*\n+\s*'
    t.value = '\n'
    global newlinesNum
    newlinesNum += 1
    return t

def t_SPACE(t):
    r'[\ \t]+'
    t.value = ' '
    return t

charsNum = 0
def t_CHAR(t):
    r'\S+'
    global charsNum
    charsNum += 1
    return t

# Error handling rule
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

# --------------------------------------------------

# Get the token map from the lexer
def p_expression_char(p):
    'expression : CHAR'
    p[0] = p[1]

def p_expression_space(p):
    'expression : SPACE'
    p[0] = p[1]

def p_expression_newline(p):
    'expression : NEWLINE'
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
f = open(".\\Task1\\preformatted.txt","r", encoding="utf8")
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
g = open(".\\Task1\\formatted.txt","w+")
g.write(result)
g.write("\n\nWords: %s" % charsNum)
g.write("\nLines: %s" % newlinesNum)
g.close() 