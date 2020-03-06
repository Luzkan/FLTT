import ply.lex as lex
import ply.yacc as yacc
from itertools import cycle
import math
import sys

# --------------------------------------------------- //
# ----------------- Miscellaneous ------------------ //
# ------------------------------------------------- //

def saveFile(asm, postfix):
    with open(outputFile+postfix, 'w+') as fp:
        for a in asm:
            fp.write(a + "\n")

def addComments(asm, cm):
    for idx, c in enumerate(cm):
        spaces = (16 - len(asm[idx]))
        space = " "
        if ']' in c:
            groupIdx = c.find(']')+1
            tag = c[:groupIdx]
            desc = c[groupIdx:]
            cSpaces = (15 - len(tag))
            asm[idx] = (str(asm[idx] + space*spaces + tag + cSpaces*space + "|" + desc))
        else:
            asm[idx] = (str(asm[idx] + space*spaces + c))

# --------------------------------------------------- //
# --------------------- Lexer ---------------------- //
# ------------------------------------------------- //
# Tokens

cmdTokens = {
    'PLUS' : 'PLUS',
    'MINUS' : 'MINUS',
    'TIMES' : 'TIMES',
    'DIV' : 'DIV',
    'MOD' : 'MOD',

    'NEQ' : 'NEQ',
    'LEQ' : 'LEQ',
    'GEQ' : 'GEQ',
    'LE' : 'LE',
    'GE' : 'GE',
    'EQ' : 'EQ',

    'DECLARE' : 'DECLARE',
    'BEGIN' : 'BEGIN',
    'END' : 'END',

    'ASSIGN' : 'ASSIGN',
    'IF' : 'IF',
    'THEN' : 'THEN',
    'ELSE' : 'ELSE',
    'ENDIF' : 'ENDIF',

    'WHILE' : 'WHILE',
    'DO' : 'DO',
    'ENDWHILE' : 'ENDWHILE',
    'ENDDO' : 'ENDDO',

    'FOR' : 'FOR',
    'FROM' : 'FROM',
    'TO' : 'TO',
    'DOWNTO' : 'DOWNTO',
    'ENDFOR' : 'ENDFOR',

    'READ' : 'READ',
    'WRITE' : 'WRITE',
}

tokens = [
    'NUM', 'PID', 'FLOAT',
     'LEFT', 'RIGHT', 'SEM', 'COL', 'COM'
] + list(cmdTokens.values())

# "t_" is ply thing, when it sees t_ and variable ply
# it is going assume that i'm telling the lex what the token actually looks like
t_LEFT = r'\('
t_RIGHT = r'\)'
t_COL = r'\:'
t_SEM = r'\;'
t_COM = r'\,'

# When ply sees spaces, it ignores it
t_ignore = r' '

# When ply sees [ comment ], it ignores it
t_ignore_COMMENT = r'\[.*\]'
t_ignore_BLOCKCOMMENT = r'\[([^]]|[\r\n])*\]'

# ----------------------------
# Rules

# Linetracking rule
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_PID(t):
    r'[_a-z]+'
    return t

# Catch for Oneliners
def t_CTID(t):
    r'ENDIF|ENDFOR|ENDWHILE|ENDDO|THEN|ELSE|DOWNTO|DO|WHILE'
    t.type = cmdTokens.get(t.value,'ID') 
    return t

# Check for reserved words
def t_ID(t):
    r'[A-Z_][A-Z]*'
    t.type = cmdTokens.get(t.value,'ID')    
    return t

# Float to Int converter (if bad input file)
def t_FLOAT(t):
    r'-?\d\.\d+'
    t.value = int(t.value)
    print("[Float] Converted to Int: '%s'" % t.value)
    return t

# Num is any integer (with 1 or more characters)
def t_NUM(t):
    r'-?\d+'
    t.value = int(t.value)
    return t
    
# When error spotted
def t_error(t):
    print("Skipping useless character: '%s'" % t.value[0])
    t.lexer.skip(1)

# EOF handling rule
def t_eof(t):
    return None

# ----------------------------
# Creating a lexer
lexer = lex.lex()

# Order of calculating, lower have higher priority
precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIV', 'MOD'),
)

# --------------------------------------------------- //
# -------------------- Parser ---------------------- //
# ------------------------------------------------- //
# Creating a parser, that takes parameter p (which is Python Tuple)

def p_program(p):
    '''
    program : DECLARE declarations BEGIN commands END
    '''
    p[0] = ("PROGRAM", p[2], p[4])
    run(p[0])
    a_end("# [END] Finished.")
    finishOutput(dec, valuesDefined, asm, cm, optAsm)
    
pre = []
init = []
lock = True

def p_program_other(p):
    '''
    program : BEGIN commands END
    '''
    p[0] = ("BEGIN", p[2])
    run(p[0])
    a_end("# [END] Finished.")
    finishOutput(dec, valuesDefined, asm, cm, optAsm)

def p_declarations_pid_lr(p):
    '''
    declarations : declarations COM PID LEFT NUM COL NUM RIGHT
    '''
    if p[5] > (p[7]):
        e_error("Invalid array range", p.lineno(5))
    else:
        if p[3] in pre:
            e_error(str(p[3])+" already declared", p.lineno(3))
        else:
            pre.append(p[3])
            p[0] = ("decslr", p[1], p[3], p[5], p[7])

def p_declarations_pid(p):
    '''
    declarations : declarations COM PID
    '''
    if p[3] in pre:
        e_error(str(p[3])+" already declared", p.lineno(3))
    else:
        pre.append(p[3])
        p[0] = ("decs", p[1], p[3])

def p_declarations_fin_lr(p):
    '''
    declarations : PID LEFT NUM COL NUM RIGHT
    '''
    if p[3] > (p[5]):
        e_error("Invalid array range", p.lineno(5))
    else:
        if p[1] in pre:
            e_error(str(p[1])+" already declared", p.lineno(1))
        else:
            pre.append([p[1], p[3], p[5]])
            p[0] = ("decflr", p[1], p[3], p[5])

def p_declarations_fin(p):
    '''
    declarations : PID
    '''
    if p[1] in pre:
        e_error(str(p[1])+" already declared", p.lineno(1))
    else:
        pre.append(p[1])
        p[0] = ("decf", p[1])

def p_commands(p):
    '''
    commands : commands command
    '''
    p[0] = ("cmd", p[1], p[2])

def p_commands_final(p):
    '''
    commands : command
    '''
    p[0] = ("cmdf", p[1])

def p_command_assign(p):
    '''
    command : identifier ASSIGN expression SEM
    '''
    p[0] = ("ass", p[1], p[3])

def p_command_if_else(p):
    '''
    command : IF condition THEN commands ELSE commands ENDIF
    '''
    p[0] = ("ifelse", p[2], p[4], p[6])

def p_command_if(p):
    '''
    command : IF condition THEN commands ENDIF
    '''
    p[0] = ("if", p[2], p[4])

def p_command_while(p):
    '''
    command : WHILE condition DO commands ENDWHILE
    '''
    p[0] = ("while", p[2], p[4])

def p_command_do(p):
    '''
    command : DO commands WHILE condition ENDDO
    '''
    p[0] = ("do", p[2], p[4])

def p_command_for_up(p):
    '''
    command : FOR identifier FROM value TO value DO commands ENDFOR
    '''
    p[0] = ("forup", p[2], p[4], p[6], p[8])

def p_command_for_down(p):
    '''
    command : FOR identifier FROM value DOWNTO value DO commands ENDFOR
    '''
    p[0] = ("fordown", p[2], p[4], p[6], p[8])

def p_command_read(p):
    '''
    command : READ identifier SEM
    '''
    p[0] = ("read", p[2])

def p_command_write(p):
    '''
    command : WRITE value SEM
    '''
    p[0] = ("write", p[2])

def p_expression(p):
    '''
    expression : value MOD value
               | value TIMES value
               | value DIV value
               | value PLUS value
               | value MINUS value
    '''
    # a EXPRESSION a
    if type(p[1][1]) == tuple and type(p[3][1]) == tuple:
        if (len(p[1][1]) == 3 and len(p[3][1]) == 3):    # L = dec; R = dec
            if p[1][1][1] == p[3][1][1]:            
                if p[2] == 'TIMES':                     # x*x = ?
                    p[0] = ("exprc", p[1], p[2], p[3])  
                elif p[2] == 'DIV':                     # x/x = 1
                    p[0] = ("exprv", ("val", 1))        
                elif p[2] == 'MOD':                     # xmodx = 0
                    p[0] = ("exprv", ("val", 0))        
                elif p[2] == 'MINUS':                   # x-x = 0
                    p[0] = ("exprv", ("val", 0))
                else:                                   # x+x = 2*x = shift, but meh
                    p[0] = ("exprc", p[1], p[2], p[3])                                # x+x = 2 * x
            else:
                p[0] = ("exprc", p[1], p[2], p[3])
        elif (len(p[1][1]) == 4 and len(p[3][1]) == 4): # L = tab; R = tab
            if (p[1][1][1] == p[3][1][1] and p[1][1][2] == p[3][1][2]):            
                if p[2] == 'TIMES':                     # x*x = ?
                    p[0] = ("exprc", p[1], p[2], p[3])  
                elif p[2] == 'DIV':                     # x/x = 1
                    p[0] = ("exprv", ("val", 1))        
                elif p[2] == 'MOD':                     # xmodx = 0
                    p[0] = ("exprv", ("val", 0))        
                elif p[2] == 'MINUS':                   # x-x = 0
                    p[0] = ("exprv", ("val", 0))
                else:                                   # x+x = 2*x = shift, but meh
                    p[0] = ("exprc", p[1], p[2], p[3])  
            else:
                p[0] = ("exprc", p[1], p[2], p[3])
        elif (len(p[1][1]) == 4 and len(p[3][1]) == 3): # L = tab; R = dec
            p[0] = ("exprc", p[1], p[2], p[3])
        else:                                           # L = tab; R = dec
            p[0] = ("exprc", p[1], p[2], p[3])
    # R = int, L = int
    elif (type(p[3][1]) == int and type(p[1][1]) == int):
        if p[2] == 'PLUS':
            p[0] = ("exprv", ("val", (p[3][1] + p[1][1])))
        if p[2] == 'MINUS':
            p[0] = ("exprv", ("val", (p[3][1] - p[1][1])))        
        if p[2] == 'TIMES':
            p[0] = ("exprv", ("val", (p[3][1] * p[1][1])))
        if p[2] == 'DIV':
            p[0] = ("exprv", ("val", (math.floor(p[3][1] / p[1][1]))))
        if p[2] == 'MOD':
            p[0] = ("exprv", ("val", (p[3][1] % p[1][1])))
    elif type(p[3][1]) == int:
        if p[3][1] == 0 and (p[2] == 'TIMES' or p[2] == 'DIV' or p[2] == 'MOD'):    # (x/0, x*0, xmod0) = 0
            p[0] = ("exprv", p[3])
        elif p[3][1] == 0 and (p[2] == 'MINUS' or p[2] == 'PLUS'):                  # (x+0, x-0) = x
            p[0] = ("exprv", p[1])
        elif p[3][1] == 1 and p[2] == 'TIMES':  # x*1 = x
            p[0] = ("exprv", p[1])
        elif p[3][1] == 1 and p[2] == 'DIV':    # x/1 = x
            p[0] = ("exprv", p[1])
        elif p[3][1] == 1 and p[2] == 'MOD':    # x mod 1 = 0
            p[0] = ("exprv", ("val", 0))
        else:
            p[0] = ("exprc", p[1], p[2], p[3])

    # L = int
    elif type(p[1][1]) == int:
        if p[1][1] == 0 and (p[2] == 'TIMES' or p[2] == 'DIV' or p[2] == 'MOD'):   # (0/x, 0*x, 0modx) = 0
            p[0] = ("exprv", p[1])
        elif p[1][1] == 0 and p[2] == 'PLUS':   # 0+x = x
            p[0] = ("exprv", p[3])
        elif p[1][1] == 0 and p[2] == 'MINUS':  # 0-x = -x, meh
            p[0] = ("exprc", p[1], p[2], p[3])
        elif p[1][1] == 1 and p[2] == 'TIMES':  # 1*x = x
            p[0] = ("exprv", p[3])
        elif p[1][1] == 1 and p[2] == 'DIV':    # 1/x = 0 (or 1/1 = 1, but above coded)
            p[0] = ("exprv", ("val", 0))
        elif p[1][1] == 1 and p[2] == 'MOD':    # 1modx = x but if x negative then meh
            p[0] = ("exprc", p[1], p[2], p[3])
        else:
            p[0] = ("exprc", p[1], p[2], p[3])
    else:
        p[0] = ("exprc", p[1], p[2], p[3])

def p_expression_final(p):
    '''
    expression : value
    '''
    p[0] = ("exprv", p[1])

def p_value(p):
    '''
    value : FLOAT
          | NUM
          | identifier
    '''
    p[0] = ("val", p[1])

def p_condition(p):
    '''
    condition : value EQ value
              | value NEQ value
              | value LE value
              | value GE value
              | value LEQ value
              | value GEQ value
    '''
    p[0] = ("cond", p[1], p[2], p[3])

def p_identifier(p):
    '''
    identifier : PID LEFT PID RIGHT
               | PID LEFT NUM RIGHT
    '''
    p[0] = ("id", p[1], p[3], p.lineno(1))

def p_identifier_pid(p):
    '''
    identifier : PID
    '''    
    p[0] = ("id", p[1], p.lineno(1))

def p_error(p):
    if hasattr(p, 'type'):
        e_error("Syntax Err. Misplaced sign: "+str(p.value), p.lineno)
    else:
        e_error("Syntax Err: "+str(p.value), p.lineno)

# ---------------------------
parser = yacc.yacc()

# --------------------------------------------------- //
# ---------- Abstract Syntax Tree Building --------- //
# ------------------------------------------------- //

def run(p):
    if type(p) == int:
        return p
    else:
        if p[0] == 'PROGRAM':
            # Clears the Register at the start of a program
            asm.clear()
            a_clr("# [START] 'm' - means memory id | x - means value | L and R are left/right evaluations") 
            return run(p[1]), run(p[2]) 
        if p[0] == 'BEGIN':
            asm.clear()
            a_clr("# [START] 'm' - means memory id | x - means value | L and R are left/right evaluations") 
            return run(p[1])
        elif p[0] == 'decslr':
            return run(p[1]), g_declararr(p[len(p) - 3], p[len(p) - 2], p[len(p) - 1])
        elif p[0] == 'decs':
            return run(p[1]), g_declar(p[len(p) - 1])
        elif p[0] == 'decflr':
            return g_declararr(p[len(p) - 3], p[len(p) - 2], p[len(p) - 1])
        elif p[0] == 'decf':
            return g_declar(p[len(p) - 1])
        elif p[0] == 'cmd':
            return g_cmds(p[1], p[2])
        elif p[0] == 'cmdf':
            return g_cmdf(p[1])
        elif p[0] == 'ass':
            return g_ass(p[1], p[2])
        elif p[0] == 'id':
            return g_id(p)
        elif p[0] == 'exprv':
            return run(p[1])
        elif p[0] == 'exprc':
            return g_exprc(p[1], p[2], p[3])
        elif p[0] == 'val':
            return run(p[1])
        elif p[0] == 'if':
            return g_if(p[1], p[2])
        elif p[0] == 'ifelse':
            return g_ifelse(p[1], p[2], p[3])
        elif p[0] == 'cond':
            return g_cond(p[1], p[2], p[3], "")
        elif p[0] == 'read':
            return g_read(p[1])
        elif p[0] == 'write':
            return g_write(p[1])
        elif p[0] == 'while':
            return g_while(p[1], p[2])
        elif p[0] == 'forup':
            return g_for(p[1], p[2], p[3], p[4], 'up')
        elif p[0] == 'fordown':
            return g_for(p[1], p[2], p[3], p[4], 'down')
        elif p[0] == 'do':
            return g_do(p[1], p[2])
        else:
            return p

# ----------------------------
# Error Handler

def e_error(text, line):
    sys.exit(("Error: "+text+". Line: "+str(line)))

# ----------------------------
# Declarations & Identifiers

inited = []
dec = {}
count = 0

def orderCount():
    global count
    count = count + 1
    return count

def g_declar(p):
    dec[p] = orderCount()

def g_declararr(p1, p2, p3):      
    for w in dec:
        if type(w) == tuple:
            if w[0] == p1:
                e_error((str(p1)+" already declared"), '2')

    if (abs(p2) + abs(p3+1)) > 10000:
        e_declareLater(p1, p2, (p3+1))          # Inclusive Table
    else:
        offset = (count+1) - p2                 # Offset - how far the table starts form p(1)
        dec[p1+"off", offset+1] = orderCount()  # Offset Memory
        e_writeOffset(offset+1, p1)             # Immiedietly declare value for offset in mem at start
        for x in range(p2, (p3+1)):             # tab(2:5) -> tab(2), tab(3), tab(4), tab(5)
            dec[p1, x] = orderCount()           # I think so because in Program2, there's table tab(2:100) and we assign something to tab(100)    

def e_declareLater(arrname, start, end):
    dec[(arrname+"big", start, end)] = orderCount()

def e_writeOffset(offset, name):
    a_setValue(offset,                          "# [OFFSET] Array '"+str(name)+"' detected. Offset: "+str(offset)+". Storing at: '"+str(count)+"'")
    a_store(count,                              "# [OFFSET]")

currentVal = []
def g_id(p):
    global currentVal
    currentVal = [p[1], p[2]]

    if len(p) == 4:                             # Table
        search = (p[1], p[2])
        idf = dec.get(search)
        d = e_isBigTable(p)                     # Enourmous Table in Declarations Quick Fix
        if d != False:                          # ex: tab(-121212121:1234567890)
            return e_bigTableHotFix(p, d, idf)
        pointer = e_tabPoint(p[1], idf, p[2], p[3])   # Retrieves pointer to table in memory
        return [p[1], pointer, idf, p[3]]       # Return [variablename, pointerToMem, None if iterating on table or memory if not iterating on it]
    else:                                       # Not Table
        idf = dec.get(p[1])
        return [p[1], idf, p[2]]

def e_isBigTable(p):
    for d in dec:
        if type(d) == tuple:
            if p[1]+"big" == d[0]:
                return d
    return False

def e_bigTableHotFix(p, d, idf):
    if (idf == None):
        if type(p[2]) == int:
            if p[2] in range(d[1], d[2]):
                dec[(p[1], p[2])] = orderCount()
                idf = dec.get((p[1], p[2]))
                return [p[1], idf, p[2]]
        else:
            pointer = ev_fmr('pointer')
            iteratorMem = dec.get(p[2])             # Detected iterator in tab(i)
            a_load(iteratorMem,                     "# [BIGTAB] '"+str(p[1])+"("+str(p[2])+")' -> Loading ("+str(p[2])+") from: '"+str(iteratorMem)+"'")
            a_store(pointer,                        "# [BIGTAB] Pointer for '"+str(p[1])+"("+str(p[2])+")'. Storing at: '"+str(pointer)+"'")
            return [p[1], pointer, idf, p[3]]
    else:
        return [p[1], idf, p[2]]

def e_tabPoint(array, value, iterator, line):

    offsetMem = ev_getOffsetMem(array+"off")    # Offset of how far from p(0) is the table first declaration
    pointer = ev_fmr('pointer')                 # Pointer p(p(pointer)) = mem(tab(i))

    names = []
    for w in dec:
        if type(w) == tuple:
            names.append(w[0])
    if array not in names:
        e_error(("Variable "+str(array)+" is not an array"), line)    

    if value == None:                           # It has iterator in tab(i)
        iteratorMem = dec.get(iterator)         # Detected iterator in tab(i)
        a_load(iteratorMem,                     "# [ID TAB] '"+str(array)+"("+str(iterator)+")' -> Loading ("+str(iterator)+") from: '"+str(iteratorMem)+"'")
    else:
        a_setValue(iterator,                    "# [ID TAB] '"+str(array)+"("+str(iterator)+")' -> Creating index value.")

    a_add(offsetMem,                            "# [ID TAB] Adding offset of '"+str(array)+"' to ("+str(iterator)+") from: '"+str(offsetMem)+"'")
    a_store(pointer,                            "# [ID TAB] Pointer for '"+str(array)+"("+str(iterator)+")'. Storing at: '"+str(pointer)+"'")
    return pointer

# ----------------------------
# Memory (Free Memory lookup for...)

# Any
def ev_fmr(val):
    idx = orderCount()
    dec[idx] = val          # Sets it up in Declarations (next free place)
    return idx              # Returns its id in memory

# Iterator
def ev_fmrIt(it):
    idx = orderCount()
    dec[it] = idx           # Sets it up iterator in Declarations (next free place)
    return idx              # Returns its id in memory

# Permanent Value
def ev_fmrPerm(perm):
    if perm in dec:
        return dec[perm]
    else:
        idx = orderCount()
        dec[perm] = idx     # Sets it up in Declarations (next free place)
        initz.append(idx)
        return idx          # Returns its id in memory

def ev_valInMem(perm):
    if perm in dec:
        return True
    else:
        return False

# ----------------------------
# For & While & Do

def g_while(p1, p2):
    condAgain = (len(asm))          # Saved idx for condition
    run(p1)                         # Returns [cond (l ? r)]
    run(p2)             
    a_jump(condAgain,               "# [WHILE]") 
    e_jumpTo('here')    
    return 

iterators = []
coolStuff = []
unrolledLoop = False

def g_for(p1, p2, p3, p4, where):
    pid = run(p1)                               # Returns "None" if iterator was not declared                  
    toVal = run(p3)                             # Ending iteration val
    fromVal = run(p2)                           # Starting iteration val

    if where:   cmt = "# [FOR UP]"
    else:       cmt = "# [FOR DOWN]"

    if (not (isinstance(fromVal, list)) and not (isinstance(toVal, list))):
        if where == 'up':
            if fromVal > toVal:
                e_error(                        "FromValue ("+str(fromVal)+") in "+str(where)+"To Loop was bigger than in ToValue ("+str(toVal)+")", currentVal[1])
        else:
            if toVal > fromVal:
                e_error(                        "FromValue ("+str(fromVal)+") in "+str(where)+"To Loop was bigger than in ToValue ("+str(toVal)+")", currentVal[1])

    # Loop Unrolling
    if (type(fromVal) == int) and (type(toVal) == int):
        global unrolledLoop
        if unrolledLoop == False:
            print(                             "[Notice!] Unrolling a loop, it may take a while.")
        unrolledLoop = True

        a_setValue(fromVal,                     cmt+" ~~~~~~ For Loop. Setting start value: ("+str(fromVal)+")")
        if pid[1] == None:                      # Iterator was not declared 
            pid[1] = ev_fmrIt(pid[0])
            a_store(pid[1],                     cmt+" Iterator was not declared, storing: ("+pid[0]+")")
        else:                                   # Iterator was previously declared
            pid[1] = ev_fmrIt(pid[0])
            a_store(pid[1],                     cmt+" Iterator was declared, storing: "+str(pid[1])) 

        if where == 'up':
            while (fromVal-1) != toVal:
                run(p4)
                a_load(pid[1],                  cmt+"")
                a_inc(                          cmt+" Loop Step.")
                a_store(pid[1],                 cmt+"")
                fromVal += 1
            dec.pop(pid[0])
            return
        else:
            while (fromVal+1) != toVal:
                run(p4)
                a_load(pid[1],                  cmt+"")
                a_dec(                          cmt+" Loop Step.")
                a_store(pid[1],                 cmt+"")
                fromVal -= 1
            dec.pop(pid[0])
            return

    # Reading FROM
    if isinstance(fromVal, list):               # It was declared
        if len(fromVal) == 4:                   # Table
            a_load([fromVal[1]],                cmt+" ~~~~~~ For Loop. Start value was declared, loading: ("+fromVal[0]+")") 
        else:                                   # Not Table
            a_load(fromVal[1],                  cmt+" ~~~~~~ For Loop. Start value was declared, loading: ("+fromVal[0]+")") 
    else:                                       # Or its a number
        a_setValue(fromVal,                     cmt+" ~~~~~~ For Loop. Setting start value: ("+str(fromVal)+")")
    fromIt = ev_fmr(fromVal)
    a_store(fromIt,                             cmt+" ")

    # Creating ITERATOR (value loaded from ^)
    if pid[1] == None:                          # Iterator was not declared 
        pid[1] = ev_fmrIt(pid[0])
        a_store(pid[1],                         cmt+" Iterator was not declared, storing: ("+pid[0]+")")
        iterators.append([pid[0], pid[1], where])
    else:                                       # Iterator was previously declared
        pid[1] = ev_fmrIt(pid[0])
        a_store(pid[1],                         cmt+" Iterator was declared, storing: "+str(pid[1]))                   
        iterators.append([pid[0], pid[1], where])

    # Reading TO
    if isinstance(toVal, list):                 # It was declared
        if len(toVal) == 4:                     # Table
            a_load([toVal[1]],                  cmt+" End value was declared, loading: ("+toVal[0]+")") 
        else:                                   # Not Table
            a_load(toVal[1],                    cmt+" End value was declared, loading: ("+toVal[0]+")") 
    else:                                       # Or its a number
        a_setValue(toVal,                       cmt+" End value is a number: "+str(toVal)+")")
    toIt = ev_fmr(toVal)
    a_store(toIt,                               cmt+" ")

    if where == 'up':                           # For UP
        g_cond(pid, 'LEQ', ['', toIt], "")      # infinite loop otherwise
    else:                                       # For DOWN
        g_cond(pid, 'GEQ', ['', toIt], "")      # infinite loop otherwise

    a_load(pid[1],                              cmt+" ~~~~~~ Loop Beginning. Loading Iterator.")
    if where == 'up':                           # For UP
        a_dec(                                  cmt+" Inclusive Start")
        a_inc(                                  cmt+" Loop Step")
    else:                                       # For DOWN
        a_inc(                                  cmt+" Inclusive Start")
        a_dec(                                  cmt+" Loop Step")

    condAgain = (len(asm))-1 
    a_store(pid[1],                             cmt+" Storing Iterator")     
    run(p4)                                     # Run Stuff that we need to iterate on
    g_cond(pid, 'NEQ', ['', toIt], "")          # Condition
    a_load(pid[1],                              cmt+" Loading iterator: "+str(pid[1]))
    a_jump(condAgain,                           cmt+" Going to decrementation step")
    e_jumpTo('here')
    e_jumpTo('here')
    dec.pop(pid[0])
    # del dec[pid[0]]

def g_do(p1, p2):
    run(p1)                  
    condAgain = (len(asm))          # Saved idx for condition
    run(p2)                  
    a_jump(condAgain, "# [Do]")     # Test Again
    e_jumpTo('here')    
    return 

# ----------------------------
# Read & Write

def g_read(p):
    idx = run(p)
    a_get(                      "# [READ] Getting Value")
    if len(idx) == 4:           # Table Assigment
        a_store([idx[1]],       "# [READ] Storing: '"+str(p[1])+"("+str(p[2])+")'")
    else:
        a_store(idx[1],         "# [READ] Storing: '"+str(p[1])+"'")
    return idx

def g_write(p):
    p = run(p)
    if isinstance(p, list):     # Declaration
        if(len(p) == 4):        # Table
            a_load([p[1]],      "# [WRITE] Loading pointer to table: '"+str(p[1])+"'")
            a_put(              "# [WRITE]")
        else:                   # Not Table
            a_load(p[1],        "# [WRITE] Loading '"+str(p[1])+"'")
            a_put(              "# [WRITE]")
    else:                       # Int
        a_setValue(p,           "# [WRITE] Creating value to print: "+str(p))
        a_put(                  "# [WRITE]")
    return p

# ----------------------------
# Conditions & Determination

def g_cond(p1, p2, p3, cmt):

    l = run(p1)
    r = run(p3)   

    if isinstance(l, list):
        if len(l) == 4:
            l = [l[1]]
        else:
            l = l[1]
    else:
        a_setValue(l,           "# [COND "+p2+"] L was not declared, setting to p0 val: "+str(l))
        l = ev_fmr(l)
        a_store(l,              "# [COND "+p2+"] L storing at: '"+str(l)+"'")
       
    if isinstance(r, list):
        r = r[1]
    else:
        a_setValue(r,           "# [COND "+p2+"] R was not declared, setting to p0 val: "+str(r))
        r = ev_fmr(r)
        a_store(r,              "# [COND "+p2+"] R storing at: '"+str(r)+"'")

    if p2 == 'EQ':
        e_eq(l, r, cmt)
    elif p2 == 'NEQ':
        e_neq(l, r, cmt)
    elif p2 == 'LE':
        e_le(l, r, cmt)
    elif p2 == 'GE':
        e_ge(l, r, cmt)
    elif p2 == 'LEQ':
        e_leq(l, r, cmt)
    elif p2 == 'GEQ':
        e_geq(l, r, cmt)

def e_eq(p1, p3, cmt):            
    a_load(p1,      "# [EQ]"+cmt+" {'"+str(p1)+"' = '"+str(p3)+"'} so: (p0 = L - R) and if: (p0 = 0) jump over skip condition, else go into skip condition.")
    a_sub(p3,       "# [EQ]"+cmt)
    a_jzero("eqt",  "# [EQ]"+cmt)
    a_jump("swap",  "# [EQ]"+cmt+" Skip Condition")
    e_jumpName("eqt")

def e_neq(p1, p3, cmt):                             
    a_load(p1,      "# [NEQ]"+cmt+" {'"+str(p1)+"' != '"+str(p3)+"'} so: (p0 = L - R) and if: (p0 = 0) jump, else proceed.") 
    a_sub(p3,       "# [NEQ]"+cmt)
    a_jzero("swap", "# [NEQ]"+cmt)

def e_le(p1, p3, cmt):                                                                                                               
    a_load(p1,      "# [LE]"+cmt+" {'"+str(p1)+"' < '"+str(p3)+"'} so: (p0 = L - R + 1) and if: (p0 = 1 (+c)) jump, else proceed.") 
    a_sub(p3,       "# [LE]"+cmt)                                                                                                   
    a_inc(          "# [LE]"+cmt)
    a_jpos("swap",  "# [LE]"+cmt)

def e_ge(p1, p3, cmt): 
    a_load(p1,      "# [GE]"+cmt+" {'"+str(p1)+"' > '"+str(p3)+"'} so: (p0 = L - R - 1) and if: (p0 = -1 (-c)) jump, else proceed.")
    a_sub(p3,       "# [GE]"+cmt)
    a_dec(          "# [GE]"+cmt)
    a_jneg("swap",  "# [GE]"+cmt)

def e_leq(p1, p3, cmt):
    a_load(p1,      "# [LEQ]"+cmt+" {'"+str(p1)+"' <= '"+str(p3)+"'} so: (p0 = L - R) and if: (p0 > 0) jump, else proceed.") 
    a_sub(p3,       "# [LEQ]"+cmt)
    a_jpos("swap",  "# [LEQ]"+cmt)

def e_geq(p1, p3, cmt): 
    a_load(p1,      "# [GEQ]"+cmt+" {'"+str(p1)+"' >= '"+str(p3)+"'} so: (p0 = L - R) and if (p0 < 0) jump, else proceed.")
    a_sub(p3,       "# [GEQ]"+cmt) 
    a_jneg("swap",  "# [GEQ]"+cmt)

# ----------------------------
# Commands & Execution

def g_cmds(p1, p2):
    return run(p1), run(p2)

def g_cmdf(p):
    return run(p)

def g_if(p1, p2):
    cond = run(p1)
    cmd = run(p2)
    e_jumpTo('here')            # Change the swap to line number where if ends
    return cond, cmd

def g_ifelse(p1, p2, p3):
    cond = run(p1)
    cmd1 = run(p2)
    a_jump('else',              "# [IF ELSE] Jump to not proceed into else statement")
    e_jumpTo('here')            # Change the swap to line number where if ends
    cmd2 = run(p3)   
    e_jumpName('else')     
    return cond, cmd1, cmd2

def e_jumpName(keyword):        # Swaps on first found instance of "else"
    global asm
    for idx, w in reversed(list(enumerate(asm))):
        if keyword in w:
            asm[idx] = w.replace(keyword, str(len(asm)), 1)
            return

def e_jumpTo(where):            # Swaps on first found instance of "swap" or "skip" to place
    global asm
    if where == 'here':
        for idx, w in reversed(list(enumerate(asm))):
            if 'swap' in w:
                asm[idx] = w.replace('swap', str(len(asm)), 1)
                return
    else:
        for idx, w in reversed(list(enumerate(asm))):
            if 'skip' in w:
                asm[idx] = w.replace('skip', str(where), 1)
                return

# ----------------------------
# Assign

def g_ass(p1, p2):

    left = run(p1)
    if isinstance(left, list):
        if len(left) == 4:                  # Table
            if left[2] == None:             # Type: tab(i)
                initz.append([left[1]])
            else:                           # Type: tab(num)
                initz.append(left[2])
        else:                               # Old Value was a declaration
            initz.append(left[1])       
    right = run(p2)

    if type(right) == int:
        if type(right) != type(None):       # Assembly Code already in, just store
            a_setValue(right,               "# [ASSIGN] R is a value. Setting up value: "+str(right))
    else:
        if len(right) == 4:                 # Table
            if right[2] == None:            # Type: tab(i)
                a_load([right[1]],          "# [ASSIGN] R is iteration of '"+str(right[0])+"'. Loading w/ pointer: '"+str(right[1])+"'")
            else:                           # Type: tab(num)
                a_load(right[2],            "# [ASSIGN] R is value of '"+str(right[0])+"'. Loading directly: '"+str(right[2])+"'")
        elif isinstance(right, list):       # Declaration - if it was a tuple with two memories (like in: sito(n) ASSIGN sito(k) PLUS sito(k) then we already got value in added, so no need to load)
            a_load(right[1],                "# [ASSIGN] R is declaration. Loading: '"+str(right[1])+"'")

    if isinstance(left, list):
        if len(left) == 4:                  # Table
            if left[2] == None:             # Type: tab(i)
                a_store([left[1]],          "# [ASSIGN] L is iteration of '"+str(left[0])+"'. Storing w/ pointer: '"+str(left[1])+"'")
            else:                           # Type: tab(num)
                a_store(left[2],            "# [ASSIGN] L is value of '"+str(left[0])+"'. Storing directly: '"+str(left[2])+"'")
        else:                               # Old Value was a declaration
            a_store(left[1],                "# [ASSIGN] L was declaration. Storing: '"+str(left[1])+"'")            
    else:
        a_store(ev_fmr('nwm'),              "# [ASSIGN] It's not possible that L was a number, woah dude...")
        
def ev_getOffset(name):
    for w in dec:
        if type(w) == tuple:
            if w[0] == name:
                return w[1]                 # Return offset

def ev_getOffsetMem(name):
    for w in dec:
        if type(w) == tuple:
            if w[0] == name:
                return dec.get(w)           # Return offset

def ev_findInDec(val):
    for w in dec:
        if val == w:
            return dec.get(w)

# ----------------------------
# Expressions & Calculations

def g_exprc(p1, p2, p3):

    l = run(p1)
    r = run(p3)
    ll = False
    rr = False

    if isinstance(l, list):
        if len(l) == 4:         # Table
            l = l[1]
            a_load([l],         "# [EXPRC "+p2+"] L was declared - loading tab pointer: '"+str(l)+"' and storing it in temporary place.")
            l = ev_fmr(len(dec)+1)
            a_store(l,          "# [EXPRC "+p2+"]")
            ll = True            
        else:                   # Not Table
            l = l[1]
            a_load(l,           "# [EXPRC "+p2+"] L was declared - loading: '"+str(l)+"' and storing it in temporary place.")
            l = ev_fmr(len(dec)+1)
            a_store(l,          "# [EXPRC "+p2+"]")
            ll = True
        
    if isinstance(r, list):
        if len(r) == 4:
            r = r[1]
            a_load([r],         "# [EXPRC "+p2+"] R was declared - loading tab pointer: '"+str(r)+"' and storing it in temporary place.")
            r = ev_fmr(len(dec)+1)
            a_store(r,          "# [EXPRC "+p2+"]")
            rr = True            
        else:
            r = r[1]
            a_load(r,           "# [EXPRC "+p2+"] R was declared - loading: '"+str(r)+"' and storing it in temporary place.")
            r = ev_fmr(len(dec)+1)
            a_store(r,          "# [EXPRC "+p2+"]")            
            rr = True

    if p2 == 'PLUS':
        e_add(l, r, ll, rr)
    elif p2 == 'MINUS':
        e_sub(l, r, ll, rr)
    elif p2 == 'TIMES':
        b_mul(l, r, ll, rr)
    elif p2 == 'DIV':
        b_div(l, r, ll, rr, False)
    elif p2 == 'MOD':
        b_div(l, r, ll, rr, True)
    return l, r

# Better Multiplication for bigger inputs
def b_mul(l, r, ll, rr):

    lmem = ev_fmr('l')          # Allocate memory for l
    rmem = ev_fmr('r')          #                 for r

    if ll and rr:               # p(0) = right and p(l) = left
        a_store(rmem,           "# [MUL] (R in '0', L in '"+str(l)+"') -> storing R temp('"+str(rmem)+"'); loading L '"+str(l)+"' to temp('"+str(lmem)+"').")
        a_load(l,               "# [MUL]") 
        a_store(lmem,           "# [MUL]")

    elif ll:                    # p(0) = left
        if(r == 2):
            a_double(           "# [MUL] R == 2, doubling.")
            return 
        else:
            a_store(lmem,       "# [MUL] (L in p0, R is a val) -> storing L temp('"+str(lmem)+"'); setting val "+str(r)+" to temp('"+str(rmem)+"').")
            a_setValue(r,       "# [MUL]")
            a_store(rmem,       "# [MUL]")

    elif rr:                    # p(0) = right
        if(l == 2):
            a_double(           "# [MUL] L == 2, doubling.")
            return 
        else:
            a_store(rmem,       "# [MUL] (R in p0, L is a val) -> storing R in temp('"+str(rmem)+"'); setting val "+str(l)+" to temp('"+str(lmem)+"').")
            a_setValue(l,       "# [MUL] L was not declared, creating value "+str(l))
            a_store(lmem,       "# [MUL] Storing L in '"+str(lmem)+"'")

    else:                       # None loaded
        a_setValue(l,           "# [MUL] None Loaded -> setting L = "+str(l)+" and setting R = "+str(r))
        a_store(lmem,           "# [MUL]")
        a_setValue(r,           "# [MUL]")
        a_store(rmem,           "# [MUL]")

                                # Creating Result container and value 0 for condition check
    a_clr(                      "# [MUL] / Preparing /")
    res = ev_fmr('res')         # Allocate memory for result
    a_store(res,                "# [MUL] / - Result")

    a_load(rmem, "")                       
    a_jzero('zeror',            "# Check if right = 0")
    a_dec("") 
    a_jzero('oner',             "# Check if right = 1")
    a_dec("")
    a_jzero('twor',             "# Check if right = 2")

    a_load(lmem, "")                       
    a_jzero('zerol',            "# Check if left = 0")
    a_dec("") 
    a_jzero('onel',             "# Check if left = 1")
    a_dec("")
    a_jzero('twol',             "# Check if left = 2")
    a_inc("")
    a_inc("")
    a_jpos("swap",              "# Check if left = negative")
    
    a_clr("# Value was negative")                   # is negative
    a_sub(lmem, "")
    a_store(lmem, "")
    a_clr("")
    a_sub(rmem, "")
    a_store(rmem, "# else: Value was positive, going to loop.")

    a_inc("") 
    a_jzero('oner',             "# Check if right = -1")
    a_inc("")
    a_jzero('twor',             "# Check if right = -2")

    a_load(lmem, "")                       
    a_jzero('onel',             "# Check if left = -1")
    a_inc("")
    a_jzero('twol',             "# Check if left = -2")

    e_jumpTo('here')            # was positive

    # Begin While Loop
    a_load(lmem,                "# [MUL] ~~~~~~ Starting Multiplication Loop. Loading L.")
    condAgain = (len(asm))-1

    a_jzero("swap", "# [NEQ]")
    e_lowestBit(lmem)           # Checking if last bit is 1
    a_jzero("swap",             "# [MUL]----| if (a & 1) == true")
                                # Condition was true, if false skipping...
    a_load(res,                 "# [MUL]    | Result += right -> load(res) + add(r) = new(res)")
    a_add(rmem,                 "# [MUL]    |")
    a_store(res,                "# [MUL]____|----|")
                                # ... skipping up to here.
    e_jumpTo('here')
                                # a >> 1
    a_load(lmem,                "# [MUL] Right shifting L.")
    a_half(                     "# [MUL]")
    a_store(lmem,               "# [MUL]")

                                # b << 1 (which is b + b)
    a_load(rmem,                "# [MUL] Left shifting R. Same as: R += R")
    a_add(rmem,                 "# [MUL]")
    a_store(rmem,               "# [MUL]")
    a_jump(condAgain,           "# [MUL] Repeat until L == 0")

    e_jumpTo('here')            # Returning back to While Loop  
    a_load(res,                 "# [MUL] ~~~~~~ Finished Multiplication. Loading Result.")
    a_jump("fmcj",              "# [MUL] Jumping to finish.")

    e_jumpName('twol') 
    e_jumpName('twol')                    
    a_load(rmem,                "# [MUL] Result = double(R), becuase L = 2.")
    a_double(                   "")
    a_jump("smcj",              "# [MUL] Jumping to finish.")

    e_jumpName('twor')
    e_jumpName('twor')                    
    a_load(lmem,                "# [MUL] Result = double(L), becuase R = 2.")
    a_double(                   "")
    a_jump((len(asm))+6,        "# [MUL] Jumping to finish.")

    e_jumpName('onel')
    e_jumpName('onel')
    a_load(rmem,                "# [MUL] Result = R, becuase L = 1.")
    a_jump((len(asm))+4,        "# [MUL] Jumping to finish.")

    e_jumpName('oner')
    e_jumpName('oner')
    a_load(lmem,                "# [MUL] Result = L, becuase R = 1.")
    a_jump((len(asm))+2,        "# [MUL] Jumping to finish.")

    e_jumpName("zerol")
    e_jumpName("zeror")
    a_clr(                      "# [MUL] Result = 0, becuase R/L = 0.")
    e_jumpName("fmcj")
    e_jumpName("smcj")


def e_signCheck(num, zero, memrightcheck, memleftcheck, keepRightInfo, keepLeftInfo, cmt):
    # Check if num < 0              # If negative, store +1 to sign mem
    g_cond(['', num], 'LE', ['', zero], "")   
    if keepRightInfo == True:       # When mod -> reminder sign = divider sign
        a_load(memrightcheck,       cmt+" ~~~ R sign was negative.")
        a_inc(                      cmt+" ")
        a_store(memrightcheck,      cmt+" ")

    if keepLeftInfo == True:
        a_load(memleftcheck,        cmt+" ~~~ L sign was negative.")
        a_inc(                      cmt+" ")
        a_store(memleftcheck,       cmt+" ")    
    
    a_load(zero,                    cmt+" Reversing it: load(0) - sub(-x) = store(+x)")
    a_sub(num,                      cmt+"")
    a_store(num,                    cmt+"")
    e_jumpTo('here')

# Better Division for bigger inputs
def b_div(l, r, ll, rr, mod):

    scaleddiv = ev_fmr('l')     # Allocate memory for l
    remain = ev_fmr('r')        #                 for b
    multiple = ev_fmr('mul')    #                 for one
    zero = ev_fmrPerm('0perm')  #                 for zero
    res = ev_fmr('res')         #                 for result
    rightcheck = ev_fmr('rchk') #                 for right sign check
    leftcheck = ev_fmr('lchk')  #                 for left sign check

    if mod:
        cmt = "# [MOD]"
    else:
        cmt = "# [DIV]"            
                                        # Creating Result Container, value 1 for multiplier and value 0 for cond check
    a_clr(                              cmt+" / Preparing:")
    if not ev_valInMem('0perm'):
        a_store(zero,                   cmt+" / - Zero")
    a_store(res,                        cmt+" / - Result")
    a_store(rightcheck,                 cmt+" / - Right Sign Check")
    a_store(leftcheck,                  cmt+" / - Left Sign Check")
    a_inc(                              cmt+"")
    a_store(multiple,                   cmt+" / - Multiple")
    
    if not ev_valInMem('1perm'):
        lsign = ev_fmrPerm('1perm') 
        a_store(lsign,                  cmt+" / - Left Sign")
    lsign = ev_fmrPerm('1perm') 
    rsign = lsign

    # if not ev_valInMem('2perm'):
    #     two = ev_fmrPerm('2perm')
    #     a_inc(                          cmt+"")
    #     a_store(two,                    cmt+" / - Two")
    # two = ev_fmrPerm('2perm')

    if ll and rr:                       # p(0) = right and p(l) = left
        e_signCheck(r, zero, rightcheck, leftcheck, True, False, cmt)
        a_load(r, "")
        a_store(scaleddiv,              cmt+" (R in p0, L in '"+str(l)+"') -> storing R as a scaled divisor in '"+str(scaleddiv)+"'. Testing L sign.")
        e_signCheck(l, zero, rightcheck, leftcheck, not mod, True, cmt)
        a_load(l,                       cmt+" Loading in L: '"+str(l)+"' and storing in remain '"+str(remain)+"'") 
        a_store(remain,                 cmt+"")

    elif ll:                            # p(0) = left
        e_signCheck(l, zero, rightcheck, leftcheck, not mod, True, cmt)
        a_load(l, "")

        if mod:
            if (r == 2):
                e_lowestBit(l)
                a_jzero("swap",         "# [MOD] (a & 1)? Yes - jump, if not: remain = -1 +1 +1 = 1")
                a_inc(                  "")
                a_inc(                  "")
                e_jumpTo('here')
                a_store(remain,         cmt+"")
                e_customModulo(lsign, leftcheck, rsign, rightcheck, zero, remain, r)
                a_load(remain,          cmt+" ~~~~~~ Finished Modulo Operation. Loading Remain.")
                return
            if (r == -2):
                a_load(rightcheck,      cmt+" ~~~ R sign was negative.")
                a_inc(                  cmt+"")
                a_store(rightcheck,     cmt+"")
                e_lowestBit(l)
                a_jzero("swap",         "# [MOD] (a & 1)? Yes - jump, if not: remain = -1 +1 +1 = 1")
                a_inc(                  "")
                a_inc(                  "")
                e_jumpTo('here')
                a_store(remain,         cmt+"")
                e_customModulo(lsign, leftcheck, rsign, rightcheck, zero, remain, r)
                a_load(remain,          cmt+" ~~~~~~ Finished Modulo Operation. Loading Remain.")
                return

        if not mod:
            if(r == 2):
                a_half(                 cmt+" R == 2, halving.")
                a_store(res,            cmt+"")
                e_customDivision(rsign, rightcheck, zero, remain, res, cmt)
                a_load(res,             cmt+" ~~~~~~ Finished Division Operation. Loading Result.")
                return 
            elif(r == -2):
                a_half(                 cmt+" R == -2, halving.")
                a_store(res,            cmt+"")
                a_load(rightcheck,      cmt+" ~~~ R sign was negative.")
                a_inc(                  cmt+"")
                a_store(rightcheck,     cmt+"")
                e_customDivision(rsign, rightcheck, zero, remain, res, cmt)
                a_load(res,             cmt+" ~~~~~~ Finished Division Operation. Loading Result.")
                return 

        a_store(remain,                 cmt+" (L in p0, R is a val) -> storing L as a remain in '"+str(remain)+"'.")
        if r < 0:
            a_load(rightcheck,          cmt+" ~~~ R sign was negative.")
            a_inc(                      cmt+"")
            a_store(rightcheck,         cmt+"")
            a_setValue(abs(r),          cmt+" Setting R = "+str(r))
        else:
            a_setValue(r,               cmt+" Setting R = "+str(r))
        a_store(scaleddiv,              cmt+" Storing R in '"+str(scaleddiv)+"' (Scaled Divisor)")
        r = ev_fmr('Divisor')
        a_store(r,                      cmt+" Storing R in '"+str(r)+"' (Divisor)")

    elif rr:                            # p(0) = right
        e_signCheck(r, zero, rightcheck, leftcheck, True, False, cmt)
        a_load(r, "")
        a_store(scaleddiv,              cmt+" R in p0, L is a val -> storing R in '"+str(scaleddiv)+"' (Scaled Divisor)")
        if l < 0:
            if mod == False:
                a_load(rightcheck,      cmt+" Sign Test - num was negative")
                a_inc(                  cmt+" ")
                a_store(rightcheck,     cmt+" ")
            else:
                a_load(leftcheck,       cmt+" Sign Test - num was negative")
                a_inc(                  cmt+" ")
                a_store(leftcheck,      cmt+" ")    
            a_setValue(abs(l),          cmt+" L was not declared, creating value "+str(l))
        else:
            a_setValue(l,               cmt+" L was not declared, creating value "+str(l))
        a_store(remain,                 cmt+" Storing L in '"+str(remain)+"'")
        l = ev_fmr('Dividend')
        a_store(l,                      cmt+" Storing L in '"+str(l)+"' (Dividend)")

    else:                               # None loaded
        if l < 0:
            if mod == False:
                a_load(rightcheck,      cmt+" None Loaded -> setting L = "+str(l))
                a_inc(                  cmt+" L was negative btw")
                a_store(rightcheck,     cmt+" ")
            else:
                a_load(leftcheck,       cmt+" Sign Test - num was negative")
                a_inc(                  cmt+" ")
                a_store(leftcheck,      cmt+" ")   
            a_setValue(abs(l),          cmt+" L was not declared, creating value "+str(l))
        else:
            a_setValue(l,               cmt+" L was not declared, creating value "+str(l))

        a_store(remain,                 cmt+" Storing L at '"+str(remain)+"' (Remain)")
        l = ev_fmr('Dividend')
        a_store(l,                      cmt+" Storing L in '"+str(l)+"' (Dividend)")

        if r < 0:
            a_load(rightcheck,          cmt+" Setting R = "+str(r))
            a_inc(                      cmt+" it was negative btw")
            a_store(rightcheck,         cmt+" ")
            a_setValue(abs(r),          cmt+" Setting R = "+str(r))
        else:
            a_setValue(r,               cmt+" Setting R = "+str(r))

        a_store(scaleddiv,              cmt+" Storing R at '"+str(scaleddiv)+"' (Scaled Div)")
        r = ev_fmr('Divisor')
        a_store(r,                      cmt+" Storing R in '"+str(r)+"' (Divisor)")

    a_load(r, "")                       
    a_jzero('swap', cmt+" Check id Div by 0")
    a_dec("") 
    a_jzero('swap', cmt+" Check id Div by 1")
    a_dec("")
    a_jzero('swap', cmt+" Check id Div by 2")

                                        # Begin While Loop (#1)
    a_load(scaleddiv,                   cmt+" ~~~~~~ Starting #1 Division Loop. Loading scaled divisor.")
    condAgain = (len(asm))-1
    g_cond(['', scaleddiv], 'LE', ['', l], "----|")
                                        # Condition was true, if false skipping...
    a_load(scaleddiv,                   cmt+"    | ScaledDiv += scaledDiv -> loading(scaledDiv) + add(scaledDiv) = new(scaledDiv)")
    a_add(scaleddiv,                    cmt+"    |")
    a_store(scaleddiv,                  cmt+"    |")
    a_load(multiple,                    cmt+"    | Multiple += multiple -> loading(multiple) + adding(multiple) = new(multiple)")
    a_add(multiple,                     cmt+"    |")
    a_store(multiple,                   cmt+"    |")
    a_jump(condAgain,                   cmt+"____|----| Repeat until (scaledDiv < dividend).")
    e_jumpTo('here')                    # Returning back to While Loop (#1)

                                        # Begin Do Loop (#2)
    a_jump('skip',                      cmt+" 'Do' ahead. Skipping first NEQ. Jumping to GEQ.")
    a_load(remain,                      cmt+" ~~~~~~ Starting #2 Division Loop. Loading remain.")
    condAgain = (len(asm))-1
    g_cond(['', multiple], 'NEQ', ['', zero], "----|")
    e_jumpName('skip')
    g_cond(['', remain], 'GEQ', ['', scaleddiv], "    |----|")

                                        # Condition was true, if false skipping...
    a_load(remain,                      cmt+"    |    | Remain -= scaledDiv -> load(remain) + add(scaledDiv) = new(remain)")
    a_sub(scaleddiv,                    cmt+"    |    |")
    a_store(remain,                     cmt+"    |    |")
    a_load(res,                         cmt+"    |    | Result += multiple -> load(result) + add(multiple) = new(result)")
    a_add(multiple,                     cmt+"    |    |")
    a_store(res,                        cmt+"____|____|----|")
                                        # ... skipping up to here.
    e_jumpTo('here')

    a_load(scaleddiv,                   cmt+"    | Right shifting scaled divisor. -> half(scaledDiv) = new(scaledDiv)")
    a_half(                             cmt+"    |")
    a_store(scaleddiv,                  cmt+"    |")
    a_load(multiple,                    cmt+"    | Right Shifting multiple -> half(multiple) = new(multiple)")
    a_half(                             cmt+"    |")
    a_store(multiple,                   cmt+"    |")
    a_jump(condAgain,                   cmt+"____|----| Repeat until (multiple == 0)")

    e_jumpTo('here')                    # Returning back to While Loop

    if mod:
        e_customModulo(lsign, leftcheck, rsign, rightcheck, zero, remain, r)
        a_load(remain,                  cmt+" ~~~~~~ Finished Modulo Operation. Loading Remain.")
    else:                               # Sign negative only if sign == 1 
        e_customDivision(rsign, rightcheck, zero, remain, res, cmt)
        a_load(res,                     cmt+" ~~~~~~ Finished Division Operation. Loading Result.")
    if not mod:
        a_jump("fddc",                  cmt+" Jumping to finish.")
        e_jumpTo('here')                # Quiting the Division cuz Divisor = 2.
        a_load(l,                       cmt+" Result = shift(L), becuase R = 2.")
        a_half(                         cmt+"")
        a_jump((len(asm))+4,            cmt+" Jumping to finish.")
        e_jumpTo('here')                # Quiting the Division cuz Divisor = 1.
        a_load(l,                       cmt+" Result = L, becuase R = 1.")
        a_jump((len(asm))+2,            cmt+" Jumping to finish.")
        e_jumpTo('here')                # Quiting the Division cuz Divisor = 0.
        a_clr(                          cmt+" Result = 0, becuase R = 0.")
        e_jumpName("fddc")
    if mod:
        a_jump("fmdc",                  cmt+" Jumping to finish.")
        e_jumpTo('here')                # Quiting the Division cuz Divisor = 2.
        e_lowestBit(l)
        a_jzero("swap",                 "# [MOD] (a & 1)? Yes - jump, if not: remain = -1 +1 +1 = 1")
        a_inc(                          "")
        a_inc(                          "")
        e_jumpTo('here')
        a_store(remain,                 cmt+"")
        e_customModulo(lsign, leftcheck, rsign, rightcheck, zero, remain, r)
        a_load(remain,                  cmt+" ~~~~~~ Finished Modulo Operation. Loading Remain.")
        a_jump("smdc",                  cmt+" Jumping to finish.")
        e_jumpTo('here')                # Quiting the Division cuz Divisor = 1.
        a_clr(                          cmt+" Remain = 0, becuase R = 1.")
        a_jump((len(asm))+2,            cmt+" Jumping to finish.")
        e_jumpTo('here')                # Quiting the Division cuz Divisor = 0.
        a_clr(                          cmt+" Result = 0, becuase R = 0.")
        e_jumpName("fmdc")
        e_jumpName("smdc")



def e_lowestBit(mem):
    a_load(mem,                 "# [LASTBIT] ~~~~~~ Checking Lastbit")
    a_half(                     "# [LASTBIT]")
    a_double(                   "# [LASTBIT]")
    a_sub(mem,                  "# [LASTBIT] / Finished - p0 has 0 or 1. /")

def e_customDivision(rsign, rightcheck, zero, remain, res, cmt):
    g_cond(['', rsign], 'EQ', ['', rightcheck], "")  
    g_cond(['', zero], 'NEQ', ['', remain], "")
    a_load(zero,                cmt+" Result should be negative: load(0) - sub(result) = new(-result). Then flooring topwards to match documentation example.")
    a_sub(res,                  cmt+"")
    a_dec(                      cmt+"")
    a_store(res,                cmt+"")
    a_jump('skip',              cmt+" Jumping beyond the not floored condition.")
    e_jumpTo('here')
    a_load(zero,                cmt+" Result should be negative: load(0) - sub(result) = new(-result). Then NOT flooring topwards like in documentation because reminder = 0.")
    a_sub(res,                  cmt+"")
    a_store(res,                cmt+"")
    e_jumpName('skip')
    e_jumpTo('here')

def e_customModulo(lsign, leftcheck, rsign, rightcheck, zero, remain, r):
    g_cond(['', remain], 'NEQ', ['', zero], "")
    g_cond(['', lsign], 'EQ', ['', leftcheck], "")

    a_load(rsign,               "# [MOD] Checking right sign. {'"+str(lsign)+"' = '"+str(leftcheck)+"'} so: (p0 = L - R) and if: (p0 = 0) jump over skip condition, else go into skip condition.")
    a_sub(rightcheck,           "# [MOD]")
    a_jzero("eqt",              "# [MOD]")
    a_jump("inne",              "# [MOD] Skip Condition")
    e_jumpName("eqt")

    a_load(zero,                "# [MOD] -l mod -r (reversing)")
    a_sub(remain,               "# [MOD] ")
    a_store(remain,             "# [MOD] ")
    a_jump("finish",            "# [MOD] Finished.")

    e_jumpName("inne")
    a_load(remain,              "# [MOD] -l mod r (subtrackting and reversing)")
    a_sub(r,                    "# [MOD] ")
    a_store(remain,             "# [MOD] ")
    a_load(zero,                "# [MOD] ")
    a_sub(remain,               "# [MOD] ")
    a_store(remain,             "# [MOD] ")
    a_jump("finish",            "# [MOD] Finished.")

    e_jumpTo('here')    
    g_cond(['', rsign], 'EQ', ['', rightcheck], "") 
    a_load(remain,              "# [MOD] l mod -r (subtrackting)")
    a_sub(r,                    "# [MOD] ")
    a_store(remain,             "# [MOD] Finished.")
    e_jumpTo('here')
    e_jumpTo('here')
    e_jumpName("finish")
    e_jumpName("finish")


# Basic Calculations Evaluations
def e_add(left, right, ll, rr):
    if ll and rr:               # p0 = right and [1] has left
        a_add(left,             "# [ADD] R in p0, L in '"+str(left)+"' -> adding")

    elif ll:                    # [1] has left
        a_setValue(right,       "# [ADD] L in '"+str(left)+"' -> setting R = "+str(right))
        a_add(left,             "# [ADD] Adding L to p0")

    elif rr:                    # p0 = right
        a_setValue(left,        "# [ADD] R in p0 -> setting L = "+str(left))
        a_add(right,            "# [ADD] Adding R to p0") 

    else:                       # None loaded
        a_setValue(left,        "# [ADD] None Loaded -> setting L = "+str(left))
        memL = ev_fmr('left')
        a_store(memL,           "# [ADD] Storing L")
        a_setValue(right,       "# [ADD] Setting R = "+str(right))
        a_add(memL,             "# [ADD] Adding L to p0")

def e_sub(left, right, ll, rr):
    if ll and rr:               # p0 = right and in memory; and [1] has left
        a_load(left,            "# [SUB] Fix: Loading L first.")
        a_sub(right,             "# [SUB] R in p0, L in in '"+str(left)+"' -> subtrackting")

    elif ll:                    # [1] has left
        a_setValue(right,       "# [SUB] L in in '"+str(left)+"' -> setting R = "+str(right))
        memR = ev_fmr('right')
        a_store(memR,           "# [SUB] Fix: Storing R")
        a_load(left,            "# [SUB] Loading L.")
        a_sub(memR,             "# [SUB] Subtracking R from p0")

    elif rr:                    # p0 = right and in memory
        a_setValue(left,        "# [SUB] R in p0 -> setting L = "+str(left))
        a_sub(right,            "# [SUB] Subtracking R from p0")

    else:                       # None loaded
        a_setValue(right,       "# [SUB] None Loaded -> setting R = "+str(right))
        memR = ev_fmr('right')
        a_store(memR,           "# [SUB] Storing L")
        a_setValue(left,        "# [SUB] Setting L = "+str(left))
        a_sub(memR,             "# [SUB] Subtrackting L from p0")
        

# --------------------------------------------------- //
# ---------- Assembly Prinitng Commands ------------ //
# ------------------------------------------------- //
# Assembly functions starting with a_

optAsm = []
asm = []
cm = []

# Clear Memory
def a_clr(com): 
    asm.append("SUB 0")
    cm.append(com)

# Take half from current p0
def a_half(com):
    if(ev_valInMem('-1perm')):
        oneNeg = ev_fmrPerm('-1perm')
        asm.append("SHIFT "+str(oneNeg))
        cm.append(com)
    else:
        toHalf = ev_fmr('toHalf')
        a_store(toHalf,                 "# [HALF] ~ First Call of 'Half'. Creating -1 for equation: 2^(-1) = 1/2. Remembering what we have to half in '"+str(toHalf)+"'.")
        a_clr(                          "# [HALF] / Preparing:")
        a_dec(                          "# [HALF]")
        oneNeg = ev_fmrPerm('-1perm')
        a_store(oneNeg,                 "# [HALF] / - (-1) value")
        a_load(toHalf,                  "# [HALF] Loading back p0.")
        asm.append("SHIFT "+str(oneNeg))
        cm.append(com)

# Double current p0
def a_double(com):
    if(ev_valInMem('1perm')):
        one = ev_fmrPerm('1perm')
        asm.append("SHIFT "+str(one))
        cm.append(com)
    else:
        toDouble = ev_fmr('toDouble')
        a_store(toDouble,               "# [DOUBLE] ~ First Call of 'Double'. Creating 1 for equation: 2^(1) = 2. Remembering what we have to double at: '"+str(toDouble)+"'.")
        a_clr(                          "# [DOUBLE] / Preparing:")
        a_inc(                          "# [DOUBLE]")
        one = ev_fmrPerm('1perm')
        a_store(one,                    "# [DOUBLE] / - (1) value")
        a_load(toDouble,                "# [DOUBLE] Loading back p0.")
        asm.append("SHIFT "+str(one))
        cm.append(com)

valuesDefined = []

# Set Current Value
def a_setValue(val, com):
    valuesDefined.append(val)

    # if ev_valInMem(str(val)+'perm'):
    #     valMem = ev_fmrPerm(str(val)+'perm')
    #     a_load(valMem,                  "# [SETVAL] Found Value "+str(val)+" in memory: "+str(valMem))
    cm.append(com) 
    a_clr(                          "# [SETVAL] Creating: "+str(val))
    do = []                         # Do Commands to achieve Value
    cmnt = []
    if val > 0:
        i1 = "INC"                  # Add One
        i2 = "ADD 0"                # Double the Amount
        while val > 10:         
            if (val % 2 == 1):      # Odd Number - Increment One
                val -= 1        
                do.append(i1)
                cmnt.append("#")   
            else:                   # Even Number - Double It
                val = val // 2  
                do.append(i2)
                cmnt.append("#")   
        do += [i1] * int(val)
        cmnt += ["#"] * int(val) 
    else:
        subMem = ev_fmr('sub')
        i3 = "DEC"                  # Sub One
        i4 = "ADD "+str(subMem)     # Double the Amount (-) 
        i5 = "STORE "+str(subMem)  
        while val < -10:         
            if (val % 2 == 1):      # Odd Number - Increment One
                val += 1        
                do.append(i3)
                cmnt.append("#")   
            else:                   # Even Number - Double It
                val = val // 2  
                do.append(i4)
                do.append(i5)
                cmnt.append("#")   
                cmnt.append("#")
        do += [i3] * int(abs(val))
        cmnt += ["#"] * int(abs(val)) 
    do.reverse()                    # Since we did: Val -> 0,                              
    cmnt.reverse()                  #    reverse for 0 -> Val
    for d in do:
        asm.append(d)
    for c in cmnt:
        cm.append(c)
    cm.pop()

def a_get(com):                     # Gets input from user
    asm.append("GET")
    cm.append(com)

def a_put(com):                     # Prints p0 on the screen
    asm.append("PUT")
    cm.append(com)

initz = []
def a_load(mem, com):               # Loads value from p0 from Memory(i)
    if mem == None:
        e_error(                    "Undeclared variable: "+(str(currentVal[0])), currentVal[1])
    if type(mem) == int:            # Not Table: got int
        if mem not in initz:
            e_error(                "Variable "+(str(currentVal[0]))+" ("+str(mem)+") hasn't been initialized", currentVal[1])
        asm.append("LOAD "+str(mem))    
        cm.append(com)
    else:                           # Table: got [int] 
        if mem[0] not in initz:
            e_error(                "Variable "+(str(currentVal[0]))+" ("+str(mem)+") hasn't been initialized", currentVal[1])
        asm.append("LOADI "+str(mem[0]))    
        cm.append(com)

def a_store(mem, com):              # Stores p0 to Memory(i)
    if mem == None:
        e_error(                    "Undeclared variable: "+(str(currentVal[0])), currentVal[1])
    if type(mem) == int:
        initz.append(mem)
        asm.append("STORE "+str(mem))
        cm.append(com)
    else:
        initz.append(mem[0])
        asm.append("STOREI "+str(mem[0]))
        cm.append(com)

def a_add(mem, com):                # Adds to p0 from Memory(i)
    asm.append("ADD "+str(mem))
    cm.append(com)

def a_sub(mem, com):                # Subtracts from p0 from Memory(i)
    asm.append("SUB "+str(mem))
    cm.append(com)

def a_shift(mem, com):              # Shifts p0 by 2^(Memory(i))
    asm.append("SHIFT "+str(mem))
    cm.append(com)

def a_inc(com):                     # Increments p0 by one
    asm.append("INC")
    cm.append(com)

def a_dec(com):                     # Decrements p0 by one
    asm.append("DEC")
    cm.append(com)

def a_jump(to, com):                # Jumps to another line
    asm.append("JUMP "+str(to))
    cm.append(com)

def a_jpos(to, com):                # Jumps if p0 is positive
    asm.append("JPOS "+str(to))
    cm.append(com)

def a_jzero(to, com):               # Jumps if p0 is equal to 0
    asm.append("JZERO "+str(to))
    cm.append(com)

def a_jneg(to, com):                # Jumps if p0 is negative
    asm.append("JNEG "+str(to))
    cm.append(com)

def a_end(com):                     # Jumps if p0 is negative
    asm.append("HALT")
    cm.append(com)

# --------------------------------------------------- //
# ---------- Optimizing output code ---------------- //
# ------------------------------------------------- //

def cleanStupid():
    global optAsm
    running = True
    notfinished = True
        
    while notfinished:
        idx = -1
        asmcycle = cycle(optAsm)
        nextelem = next(asmcycle)
        nextnextelem = next(asmcycle)
        running = True
        while running and notfinished:
            thiselem = nextelem
            nextelem = nextnextelem
            nextnextelem = next(asmcycle)
            idx += 1
            if thiselem == nextelem and thiselem == 'SUB 0':
                if checkIfJumpedTo(str(idx)):
                    print("[Nthng]", thiselem, "idx:", idx)
                    decreaseJumps(idx, "because found twice sub 0")
                    optAsm.pop(idx)
                    running = False
                    break

            if thiselem == 'SUB 0' and 'LOAD' in nextelem:
                if checkIfJumpedTo(str(idx)):
                    print("[Nthng]", thiselem, "idx:", idx)
                    decreaseJumps(idx, "because found clear and then load")
                    optAsm.pop(idx)
                    running = False
                    break

            # If get value is never used - should it be compiled?
            # if thiselem == 'GET' and 'SUB 0' in nextelem:
            #     if checkIfJumpedTo(str(idx)):
            #         print("[Nthng]", thiselem, "idx:", idx)
            #         decreaseJumps(idx, "because found get and then sub 0")
            #         optAsm.pop(idx)
            #         running = False
            #         break

            # if thiselem == 'GET' and 'LOAD' in nextelem:
            #     if checkIfJumpedTo(str(idx)):
            #         print("[Nthng]", thiselem, "idx:", idx)
            #         decreaseJumps(idx, "because found get and then load")
            #         optAsm.pop(idx)
            #         running = False
            #         break

            if 'LOAD' in thiselem and 'STORE' in nextelem:
                if thiselem[4:] == nextelem[5:]:
                    if checkIfJumpedTo(str(idx+1)):
                        print("[Nthng]", nextelem, "idx:", idx+1)
                        decreaseJumps(idx+1, "because found load and then store same thing")
                        optAsm.pop(idx+1)
                        running = False
                        break

            if 'STORE' in thiselem and 'LOAD' in nextelem:
                if thiselem[5:] == nextelem[4:]:
                    if checkIfJumpedTo(str(idx+1)):
                        print("[Nthng]", nextelem, "idx:", idx+1)
                        decreaseJumps(idx+1, "because found store and then load same thing")
                        optAsm.pop(idx+1)
                        running = False
                        break

            if 'STORE' in thiselem and 'STORE' in nextelem:
                if thiselem[5:] == nextelem[5:]:
                    if checkIfJumpedTo(str(idx+1)):
                        print("[Nthng]", nextelem, "idx:", idx+1)
                        decreaseJumps(idx+1, "because found store and then load same thing")
                        optAsm.pop(idx+1)
                        running = False
                        break

            if thiselem == 'SUB 0' and nextelem == 'SUB 0':
                if checkIfJumpedTo(str(idx+1)):
                    print("[Nthng]", thiselem, "idx:", idx+1)
                    decreaseJumps(idx, "because found clear and then load")
                    optAsm.pop(idx)
                    running = False
                    break

            if 'LOAD' in thiselem and 'LOAD' in nextelem:
                if thiselem[4:] == nextelem[4:]:
                    if checkIfJumpedTo(str(idx+1)):
                        print("[Nthng]", nextelem, "idx:", idx+1)
                        decreaseJumps(idx+1, "because found load and then load, deleting second")
                        optAsm.pop(idx+1)
                        running = False
                        break
                    elif checkIfJumpedTo(str(idx)):
                        print("[Nthng]", thiselem, "idx:", idx)
                        decreaseJumps(idx, "because found load and then load, deleting first")
                        optAsm.pop(idx)
                        running = False
                        break

            if 'LOAD' in thiselem and 'PUT' in nextelem and 'LOAD' in nextnextelem:
                if thiselem[4:] == nextnextelem[4:]:
                    if checkIfJumpedTo(str(idx+2)):
                        print("[Nthng]", nextelem, "idx:", idx+2)
                        decreaseJumps(idx+2, "because found load - put - load, and deleting 3rd load")
                        optAsm.pop(idx+2)
                        running = False
                        break

            if(nextnextelem == 'HALT'):
                running = False
                notfinished = False
                break

def cleanGarbage():
    global optAsm
    
    junk = []
    for idx, w in enumerate(optAsm):
        if('INC' in w):
            junk.append((idx, w))
        elif('DEC' in w):
            junk.append((idx, w))
        elif('SUB' in w):
            junk.append((idx, w))
        elif('ADD' in w):
            junk.append((idx, w))
        elif('SHIFT' in w):
            junk.append((idx, w))
        elif('STORE' in w):
            junk.clear()
        elif('LOAD' in w):
            trashDetected(junk)
            junk.clear()
        elif('GET' in w):
            trashDetected(junk)
            junk.clear()
        elif('HALT' in w):
            trashDetected(junk)
            junk.clear()
        else:
            junk.clear()

def trashDetected(junk):
    global optAsm
    ld = 0
    junk.reverse()
    for j in junk:
        decreaseJumps(j[0], "trashDetected, "+str(j[0])+" popping "+str(optAsm[j[0]]))
        print("[Trash] Deleting ("+str(j[0])+"):", optAsm[j[0]], "(debug: "+str(j[1])+")")
        optAsm.pop(j[0])
        ld += 1 

def checkIfJumpedTo(idx):
    global optAsm
    for w in optAsm:
        # print("[TEST] idx:", idx, "w:", w)
        if 'JUMP '+str(idx) == w:
            return False
        if 'JPOS '+str(idx) == w:
            return False
        if 'JZERO '+str(idx) == w:
            return False
        if 'JNEG '+str(idx) == w:
            return False
    return True
 
def decreaseJumps(delidx, why):
    global optAsm
    for idx, w in enumerate(optAsm):
        if ('JUMP' in w) or ('JPOS' in w) or ('JNEG' in w):
            makeInt = int(w[4:])
            if delidx < makeInt:
                # print("Inc Jump: ", optAsm[idx], "to:", makeInt+1)
                optAsm[idx] = w.replace(str(makeInt), str(makeInt-1))
        if 'JZERO' in w:
            makeInt = int(w[5:])
            if delidx < makeInt:
                # print("Inc Jump: ", optAsm[idx], "to:", makeInt+1)
                optAsm[idx] = w.replace(str(makeInt), str(makeInt-1))

def increaseJumps(delidx, why):
    global optAsm
    for idx, w in enumerate(optAsm):
        if ('JUMP' in w) or ('JPOS' in w) or ('JNEG' in w):
            makeInt = int(w[4:])
            if delidx < makeInt:
                # print("Dec Jump: ", optAsm[idx], "to:", makeInt+1)
                optAsm[idx] = w.replace(str(makeInt), str(makeInt+1))
        if 'JZERO' in w:
            makeInt = int(w[5:])
            if delidx < makeInt:
                # print("Dec Jump: ", optAsm[idx], "to:", makeInt+1)
                optAsm[idx] = w.replace(str(makeInt), str(makeInt+1))

loops = []

def declarationsFirst():
    global optAsm

    stores = []
    for idx, w in enumerate(optAsm):
        if(w[:5] == 'STORE') and not (w[:6] == 'STOREI'):
            stores.append((idx, w))

    tempStores = []
    for s in stores:
        tempStores.append(s[1])
    uniq = occurredOnce(tempStores, len(tempStores))

    notJumpedStores = []
    jumpedStores = []
    for s in stores:
        if checkIfJumpedTo(s[0]):
            good = True
            for js in jumpedStores:
                if (int(js[0])+1) == int(s[0]):
                    good = False
                    break
            if good:
                notJumpedStores.append(s[1])

        else:
            jumpedStores.append(s)

    toTop = []
    for s in stores:
        if s[1] in uniq and s[1] in notJumpedStores:
            idx = s[0]
            counter = 0
            while int(idx) >= 0:
                if(optAsm[idx][:5] == 'STORE') and not (optAsm[idx][:6] == 'STOREI'):
                    idx -= 1
                    continue
                elif(optAsm[idx] == 'INC'):
                    counter += 1
                    idx -= 1
                    continue
                elif(optAsm[idx] == 'DEC'):
                    counter -= 1
                    idx -= 1
                    continue
                elif(optAsm[idx] == 'SUB 0'):
                    toTop.append([s[0], s[1], counter])
                    break
                else:
                    break

    toTop.sort(key = lambda toTop: toTop[2])
    # print("To Top:", toTop, "len", len(toTop))

    indx = 1
    count = 0
    la = 0
    for t in toTop:
        if t[2] != count:
            change = (t[2] - count)
            while t[2] != count:
                if change < 0:
                    increaseJumps(indx+la, "because toTop, dec")
                    optAsm.insert(indx+la, "DEC")
                    la += 1
                    count -= 1
                else:
                    increaseJumps(indx+la, "because toTop, inc")
                    optAsm.insert(indx+la, "INC")
                    la += 1
                    count += 1
        decreaseJumps(t[0]+la, "because toTop, popping "+str(t[0])+" from "+str(optAsm[t[0]+la]))
        print("[ToTop] Moving ("+str(t[0])+"):", optAsm[t[0]+la], "to ("+str(indx+la)+") (debug: "+str(t[1])+")")
        increaseJumps(indx+la, "because toTop, inserting "+str(t[1])+" at "+str(indx+la))
        optAsm.pop(t[0]+la)
        optAsm.insert(indx+la, t[1])
        indx += 1 

        wasIndex = t[0]
        for t in toTop:
            if wasIndex > t[0]:
                t[0] += 1

    increaseJumps(indx+la, "adding sub0 to prevent errors")
    optAsm.insert(indx+la, "SUB 0")

    cleanTop(toTop)

def putPerms():
    global optAsm
    perms = []
    for d in dec:
        if 'perm' in str(d):
            perms.append(d)

    pless = [['STORE '+str(dec[perms[idx]]), p.replace('perm', '')] for idx, p in enumerate(perms)]
    pless.sort(key = lambda toTop: toTop[1])

    for t in pless:
        count = 0
        firstSubFound = False
        for indx, a in enumerate(optAsm):
            # print("DOING:", t[0], "NEED:", str(t[1]), "HAVE:", count, "ATM AT:"+str(indx)+" on:", a)
            if 'INC' in a:
                count += 1
            elif 'DEC' in a:
                count -= 1
            if int(t[1]) == count:
                increaseJumps(int(indx+1), "added perm value")
                optAsm.insert(int(indx+1), t[0])
                print("[Permv] Added ("+str(indx+1)+"): ", t[0])
                break
            if ('INC' in a) or ('DEC' in a) or ('STORE' in a):
                continue
            elif ('SUB 0' in a) and not firstSubFound:
                firstSubFound = True
                continue
            else:
                if int(t[1]) >= 0:
                    while int(t[1]) != count:
                        increaseJumps(int(indx), "adding inc")
                        optAsm.insert(int(indx), "INC")
                        print("[Permv] Added ("+str(indx)+"): INC")
                        count += 1
                        indx += 1
                    increaseJumps(int(indx), "added perm value")
                    optAsm.insert(int(indx), t[0])
                    print("[Permv] Added ("+str(indx)+"): ", t[0])
                    break
                else:
                    while int(t[1]) != count:
                        increaseJumps(int(indx), "adding DEC")
                        optAsm.insert(int(indx), "DEC")
                        print("[Permv] Added ("+str(indx)+"): DEC")
                        count -= 1
                        indx += 1
                    increaseJumps(int(indx), "added perm value")
                    optAsm.insert(int(indx), t[0])
                    print("[Permv] Added ("+str(indx)+"): ", t[0])
                    break
                

def cleanTop(arr):
    global optAsm

    toClean = []
    idxs = []
    name = []

    for a in arr:
        if 'STORE' in a[1]:
            idx = a[1][6:]
            if a[2] not in idxs:
                idxs.append(a[2])
                name.append(idx)
            else:
                for d in dec.items():
                    if d[1] == int(a[1][6:]):
                        if type(d[0]) == tuple:
                            break
                        else:
                            toClean.append(a)
                            break

    for tc in toClean:
        for idx, am in enumerate(optAsm):
            if 'LOAD' in am:
                if am[5:] == tc[1][6:]:
                    for ktory, hehe in enumerate(idxs):
                        if tc[2] == hehe:
                            print("[Duplc] Swapping ("+str(idx)+"):", am, "for:", 'LOAD '+str(name[ktory]))
                            optAsm[idx] = 'LOAD '+str(name[ktory])
            if 'SUB' in am:
                if am[4:] == tc[1][6:]:
                    for ktory, hehe in enumerate(idxs):
                        if tc[2] == hehe:
                            print("[Duplc] Swapping ("+str(idx)+"):", am, "for:", 'SUB '+str(name[ktory]))
                            optAsm[idx] = 'SUB '+str(name[ktory])
            if 'ADD' in am:
                if am[4:] == tc[1][6:]:
                    for ktory, hehe in enumerate(idxs):
                        if tc[2] == hehe:
                            print("[Duplc] Swapping ("+str(idx)+"):", am, "for:", 'ADD '+str(name[ktory]))
                            optAsm[idx] = 'ADD '+str(name[ktory])
            if 'SHIFT' in am:
                if am[6:] == tc[1][6:]:
                    for ktory, hehe in enumerate(idxs):
                        if tc[2] == hehe:
                            print("[Duplc] Swapping ("+str(idx)+"):", am, "for:", 'SHIFT '+str(name[ktory]))
                            optAsm[idx] = 'SHIFT '+str(name[ktory])
            if 'LOADI' in am:
                if am[6:] == tc[1][6:]:
                    for ktory, hehe in enumerate(idxs):
                        if tc[2] == hehe:
                            print("[Duplc] Swapping ("+str(idx)+"):", am, "for:", 'LOADI '+str(name[ktory]))
                            optAsm[idx] = 'LOADI '+str(name[ktory])

    for tc in toClean:
        restart = True
        while restart:
            restart = False
            for idx, am in enumerate(optAsm):
                if am == tc[1]:
                    print("[NUTop] Deleting", tc[1], "from", idx, "(debug: "+str(am)+")")
                    decreaseJumps(idx, "Because we removed element.")
                    optAsm.pop(idx)
                    restart = True
                    break

def cleanLeftovers():
    global optAsm
    restart = True
    while restart:
        restart = False
        subTest = False
        foundSeries = False
        checkIndex = 0
        seriesEndAt = 0

        for idx, a in enumerate(optAsm):
            if ('SUB 0' in a) and subTest:
                restart = True
                foundSeries = True
                seriesEndAt = idx
                break
            elif ('INC' in a) or ('ADD' in a) or ('DEC' in a):
                checkIndex += 1
                subTest = True
            else:
                checkIndex = 0
                subTest = False
        
        seriesStart = seriesEndAt-checkIndex
        if foundSeries:
            while checkIndex != 0:
                print("[Lfovr] Found leftover junk at ("+str(seriesStart)+") to ("+str(seriesEndAt-1)+")")
                print("[Lfovr] Deleting "+str(optAsm[seriesStart])+".")
                decreaseJumps(seriesStart, "Because we removed an element.")
                optAsm.pop(seriesStart)
                checkIndex -= 1


def simplify():
    global optAsm
    restart = True
        
    subOne = ['LOAD', 'STORE', 'SUB 0', 'INC', 'STORE', 'LOAD', 'SUB', 'STORE']
    addOneAlt = ['LOAD', 'STORE', 'SUB 0', 'INC', 'ADD', 'STORE']

    checks = [subOne, addOneAlt]

    while restart:
        restart = False
        for arr in checks:
            foundSeries = False
            seriesEndAt = 0
            checkIndex = 0

            for idx, a in enumerate(optAsm):
                # print("Index:", idx, "A:", a, "Arr:", arr, "Checkindx:", checkIndex)
                if len(arr) != checkIndex:
                    if a[:3] in arr[checkIndex]:
                        checkIndex += 1
                    else:
                        checkIndex = 0
                else:
                    restart = True
                    seriesEndAt = idx
                    seriesStart = seriesEndAt-checkIndex
                    foundSeries = True
                    break
            
            if foundSeries:
                print("[Seris] Found for ", arr, "Starting at ("+str(seriesStart)+") to ("+str(seriesEndAt-1)+")")
                if arr == subOne:
                    load = optAsm[seriesStart]
                    store = optAsm[seriesEndAt-1]
                    cmd = [load, 'DEC', store]
                elif arr == addOneAlt:
                    load = optAsm[seriesStart]
                    store = optAsm[seriesEndAt-1]
                    cmd = [load, 'INC', store]

                while checkIndex != 0:
                    print("[Seris] Deleting "+str(optAsm[seriesStart])+".")
                    decreaseJumps(seriesStart, "Because we removed an element.")
                    optAsm.pop(seriesStart)
                    checkIndex -= 1
                for idnx, c in enumerate(cmd):
                    print("[Seris] Putting", c, "at ("+str(seriesStart+idnx)+").")
                    increaseJumps(seriesStart+idnx, "Because we removed an element.")
                    optAsm.insert(seriesStart+idnx, c)  

def occurredOnce(arr, n):  
    uniq = []
    arr.sort() 
    if len(arr) == 1:
        uniq.append([arr[0]])
    else:
        if arr[0] != arr[1]: 
            uniq.append(arr[0])
        for i in range(1, n - 1): 
            if (arr[i] != arr[i + 1] and arr[i] != arr[i - 1]): 
                uniq.append(arr[i])

        if arr[n - 2] != arr[n - 1]: 
            uniq.append(arr[n-1])
    return uniq

def checkStore(end, start):
    global optAsm
    idx = (start-1)
    
    knownValues = []
    while idx < end:
        idx += 1
        w = optAsm[idx]
        if w == "SUB 0":
            idx2 = (idx+1)
            w2 = optAsm[idx2]
            while idx2 < end:
                if (w2 == 'INC') or (w2 == 'DEC') or (w2[:5] == 'STORE'):
                    knownValues.append((idx2, w2))
                    idx2 += 1
                    w2 = optAsm[idx2]
                else:
                    break
    print("Known Values:", knownValues)   

def unique(values): 
    unique_list = [] 
    for x in values: 
        if x not in unique_list: 
            unique_list.append(x) 
    return unique_list

test1 = []

# --------------------------------------------------- //
# --------------- Program Starting ----------------- //
# ------------------------------------------------- //

def finishOutput(dec, valuesDefined, asm, cm, optAsm):
    # printFinish(dec, valuesDefined)
    for a in asm:
        optAsm.append(a)
    addComments(asm, cm)
    # saveFile(asm, 'cmnts')
    doOptimizations(asm, cm, optAsm)
    printLegend()
    print("\nSaved into '"+str(outputFile)+"'.")

def printFinish(dec, valuesDefined):
    print("Declarations: ", dec)
    print("Length of decs: ", len(dec))
    print("Values defined: ", valuesDefined)

def printLegend():
    print("\nLEGEND:")
    print("[Nthng] Did Nothing (was overwritten),")
    print("[NUTop] Was declared on top but never used,")
    print("[NUsed] Was declared but never used")
    print("[Duplc] Optimazed duplicate declarations,")
    print("[ToTop] Permanent One-time declaration - stored at start,")
    print("[Trash] Leftovers after ToTop optimalization")

def doOptimizations(asm, cm, optAsm):
    simplify()
    # saveFile(optAsm, 'simple')
    cleanStupid()
    # saveFile(optAsm, 'clean')
    if len(dec) != 0 and not unrolledLoop:
        declarationsFirst()
    # saveFile(optAsm, 'opt')
    putPerms()
    cleanStupid()
    cleanGarbage()
    cleanStupid()
    cleanGarbage()
    cleanStupid()
    cleanGarbage()
    # saveFile(optAsm, 'max')
    cleanLeftovers()
    # everUsed()
    # cleanStupid()
    saveFile(optAsm, '')

print("Starting compiler!")
inputFile = sys.argv[1]
outputFile = sys.argv[2]
with open(inputFile, 'r') as fp:
    content = fp.read()
    print("Read file:")
    print(content)
    parser.parse(content)
