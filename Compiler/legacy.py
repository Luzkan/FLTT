# # --------------------------------------------------- //
# # ------------------ Legacy Code  ------------------ //
# # ------------------------------------------------- //
# # Failed Optalizations :(
# 
# In g_ass: between left = run(p1) and right = run(p2)
# "Secret" iterators detection optimalization
# for it in iterators:
#     print("Fixuj błędy:", p2)
#     if len(p2) == 4:
#         if type(p2[3][1]) == int:
#             if len(p2[1][1]) == 3:
#                 if p2[1][1][1] in it:
#                     print("ID", p1[1], "is iterator", p2[1][1][1], p2[2], "id", p2[3][1])
#                     print("LEWA", left)
#                     a_load(it[1], "# Loading Loop Iterator")
#                     loop = 0
#                     if int(p2[3][1]) <= 11:
#                         if int(p2[3][1]) >= 0:
#                             if p2[2] == 'MINUS':
#                                 while loop != int(p2[3][1]):
#                                     a_dec("")
#                                     loop += 1
#                             elif p2[2] == 'PLUS':
#                                 while loop != int(p2[3][1]):
#                                     a_inc("")
#                                     loop += 1
#                         else:
#                             if p2[2] == 'MINUS':
#                                 while loop != int(p2[3][1]):
#                                     a_inc("")
#                                     loop -= 1
#                             elif p2[2] == 'PLUS':
#                                 while loop != int(p2[3][1]):
#                                     a_dec("")
#                                     loop -= 1
#                         a_store(left[1], "# Storing second iterator")
#                         initz.append(left[1])
#                         iterators.append([p1[1], left[1], [p2[1][1][1], p2[2], p2[3][1]]])
#                         return)



# # Naive Multiplication / Division
# # altough better for small 'left' inputs than current one

# def e_mul(left, right, ll, rr):
#     # Relevant info: Multiplication starts from "right" not "0" for performance
#     # If "right" in mem, then multiplier not needed, we won't change it's value so we can use it

#     iterator   = ev_fmr(len(dec)+1)     # [1] = Iterator
#     multiplier = ev_fmr(len(dec)+2)     # [2] = Multiplier (added in Loops)
#     result     = ev_fmr(len(dec)+3)     # [3] = Result Gathering
#     check      = ev_fmr(len(dec)+4)     # [4] = Zero (for EQ cond)

#     if ll and rr:           # p0 = right and [1] has left
#         a_store(result,     "# [MUL] R in p0 L in in '"+str(right)+"'")
#         a_clr(              "# [MUL] Getting '0'")
#         a_store(check,      "# [MUL] Storing check value")
#         a_load(left,        "# [MUL] Loading L")
#         a_dec(              "# [MUL]")
#         a_dec(              "# [MUL]")
#         a_store(iterator,   "# [MUL] Storing Iterator")
#         e_repeat(iterator, check, result, right, 'EQ', False)

#     elif ll:                # [1] has left
#         a_dec(              "# [MUL] L is in '"+str(right)+"', R not loaded")
#         a_dec(              "# [MUL]")
#         a_store(iterator,   "# [MUL] Storing Iterator")
#         a_clr(              "# [MUL] Getting '0'")
#         a_store(check,      "# [MUL] Storing Check")
#         a_setValue(right,   "# [MUL] Setting R: "+str(right))    
#         a_store(multiplier, "# [MUL] Storing Multiplier")
#         a_store(result,     "# [MUL] Storing Result")
#         e_repeat(iterator, check, result, multiplier, 'EQ', False)

#     elif rr:                # p0 = right
#         a_store(result,     "# [MUL] R in p0, Storing Result")
#         a_clr(              "# [MUL]")
#         a_store(check,      "# [MUL] Storing Check")
#         a_setValue(left-2,  "# [MUL] Setting Left (-2): "+str(right))
#         a_store(iterator,   "# [MUL] Storing Iterator")
#         e_repeat(iterator, check, result, right, 'EQ', False)
        
#     else:                   # None loaded
#         a_setValue(left-2,  "# [MUL] None Loaded, Setting Left: "+str(left))
#         a_store(iterator,   "# [MUL] Storing Iterator")
#         a_clr(              "# [MUL] Getting '0'")
#         a_store(check,      "# [MUL] Storing Check")
#         a_setValue(right,   "# [MUL] Setting Right: "+str(right))
#         a_store(multiplier, "# [MUL] Storing Multiplier")
#         a_store(result,     "# [MUL] Storing Result")
#         e_repeat(iterator, check, result, multiplier, 'EQ', False)

# def e_div(left, right, ll, rr, mod):

#     check      = ev_fmr(len(dec)+1)     # [1] = Check
#     multiplier = ev_fmr(len(dec)+2)     # [2] = Multiplier (added in Loops)
#     dividend   = ev_fmr(len(dec)+3)     # [3] = Cond
#     result     = ev_fmr(len(dec)+4)     # [4] = Result Value

#     if ll and rr:           # p0 = right and [1] has left
#         a_load(left,        "# [DIV] R in p0, L in in '"+str(left)+"', so storing Dividend")        
#         a_store(dividend,   "# [DIV] Storing Dividend")
#         a_clr(              "# [DIV] Getting 0")
#         a_store(check,      "# [DIV] Storing Check")
#         a_dec(              "# [DIV] Making -1")
#         a_store(result,     "# [DIV] Storing Result (iterator)")
#         e_repeat(result, dividend, check, right, 'LEQ', mod)

#     elif ll:                # [1] has left
#         a_store(dividend,   "# [DIV] L is in '"+str(left)+"', so storing Dividend")
#         a_clr(              "# [DIV] Getting 0")
#         a_store(check,      "# [DIV] Storing Check")
#         a_dec(              "# [DIV] Making -1")
#         a_store(result,     "# [DIV] Storing Result (iterator)")
#         a_setValue(right,   "# [DIV] Setting R: "+str(right))
#         a_store(multiplier, "# [DIV] Storing Multiplier")
#         e_repeat(result, dividend, check, multiplier, 'LEQ', mod)

#     elif rr:                # p0 = right
#         a_clr(              "# [DIV] Getting 0")
#         a_store(check,      "# [DIV] Storing Check")
#         a_dec(              "# [DIV] Making -1")
#         a_store(result,     "# [DIV] Result")
#         a_setValue(left,    "# [DIV] Setting L: "+str(left))
#         a_store(dividend,   "# [DIV] Dividend")
#         e_repeat(result, dividend, check, right, 'LEQ', mod)
        
#     else:                   # None loaded
#         a_setValue(left,    "# [DIV] None Loaded, setting L: "+str(left))
#         a_store(dividend,   "# [DIV] Dividend")
#         a_clr(              "# [DIV] Getting 0")
#         a_store(check,      "# [DIV] Storing Check")
#         a_dec(              "# [DIV] Making -1")
#         a_store(result,     "# [DIV] Storing Result (iterator)")
#         a_setValue(right,   "# [DIV] Setting R: "+str(right))
#         a_store(multiplier, "# [DIV] Storing Multiplier")
#         e_repeat(result, dividend, check, multiplier, 'LEQ', mod)

# def e_repeat(i, val, store, add, cond, mod):

#     a_load(i,               "# [REPEAT] Loading Iterator")          # Loop beginning
#     condAgain = (len(asm))-1             
    
#     if cond == 'EQ':                                                # Iteration Step
#         a_dec(              "# [REPEAT] -1")                        # Reach 0 if TIMES
#     else:
#         a_inc(              "# [REPEAT] +1")                        # Reach DIVIDEND if DIV
#     a_store(i,              "# [REPEAT] Save New Iterated Value")   # Save

#     a_load(store,           "# [REPEAT] Load Store")                # Load Result
#     a_add(add,              "# [REPEAT] Add to Store")              # Add the Value
#     a_store(store,          "# [REPEAT] Store Store")               # Save Result

#     if cond == 'EQ':
#         g_cond(['',i], cond, ['',val])                              # Condition - reached 0?
#     else:
#         g_cond(['',store], cond, ['',val])                          # Condition - overexceeded divider?

#     a_jump(condAgain,       "# [REPEAT] Jump to Condition")
#     e_jumpTo('here')

#     if cond == 'EQ':                                                # Load the Result
#         a_load(store,       "# [REPEAT] Loading Output for TIMES")               
#     else:
#         if mod:
#             a_load(store,   "# [REPEAT] Preparing Output (Store - Val)")
#             a_sub(val,      "# [REPEAT] Getting Output for MOD")
#         else:
#             a_load(i,       "# [REPEAT] Loading Output for DIV")



# # --------------------------------------------------- //
# # ------------------- Trash Code  ------------------ //
# # ------------------------------------------------- //

# def searchForFirst(name, s, e):
#     while s < e:
#         w = optAsm[s]
#         if w == name:
#             return s
#         s += 1

# def searchForFirstEnd(name, s, e):
#     while s < e:
#         if optAsm[s][:5] == name:
#             if(optAsm[s+1][:5] == name):
#                 s += 1
#                 continue
#             else:
#                 print("Found at ", s)
#                 return s
#         s += 1

# def gatherInLoop(end, idxstart):
#     global optAsm
#     global kanter
#     ready, wait = False, False
#     toMove = []
#     idx = idxstart-1
#     while idx < end:
#         idx += 1
#         w = optAsm[idx]
#         print("["+str(idx)+"] w:", w, ready)
#         if not idx == 0:
#             if w == "SUB 0" and wait == False:
#                 ready = True
#                 wait = True
#                 toMove.append([idx, w])     
#             # elif w == 'INC' and ready == True:
#             #     if checkIfJumpedTo(str(idx)):
#             #         toMove.append([idx, w])
#             #     else:
#             #         ready = False
#             #         wait = False
#             #         toMove.clear()
#             elif w[:5] == "STORE" and ready == True and w[:6] != "STOREI":
#                 if checkIfJumpedTo(str(idx)):
#                     toMove.append([idx, w])
#                 else:
#                     ready = False
#                     wait = False
#                     toMove.clear()
#             else:
#                 if ready == True:
#                     if len(toMove) > 1:
#                         print("[Deleting]: ["+str(toMove[0][0])+"] ", toMove[0][1])
#                         toMove.pop(0)
#                         for m in toMove:
#                             kanter += 1 
#                             name = str(m[1])
#                             sff = searchForFirst("SUB 0", idxstart, end)+1
#                             sff = searchForFirstEnd("STORE", sff, end)
#                             if name[:5] == 'STORE' and name[:6] != "STOREI":
#                                 print("[Moving]: ["+str(m[0])+"] ", m[1], "to index: ", sff)
#                                 if int(m[0]) != int(sff):
#                                     increaseJumps(m[0])
#                                 optAsm.pop(m[0])
#                                 optAsm.insert(sff, m[1])
#                     toMove.clear()
#                 ready = False
#                 wait = False
#         if(w == 'HALT'):
#             break


# def everUsed():
#     global optAsm

#     stores = []
#     usefull = []
#     notused = []

#     for idx, a in enumerate(optAsm):
#         if 'STORE' in a:
#             stores.append([idx, a])
#         if ('LOAD' in a) or ('SUB' in a) or ('ADD' in a) or ('SHIFT' in a):
#             usefull.append([idx, a])

#     print("Stores pre:", stores)
#     print("Usefull pre:", usefull)

#     for s in stores:
#         used = False
#         if s[1][:6] == 'STOREI':
#             num = s[1][7:]
#             for u in usefull:
#                 if num in u:
#                     used = True
#                     break
#             if not used:
#                 notused.append(s)
#         elif s[1][:6] == 'STORE ':
#             num = s[1][6:]
#             for u in usefull:
#                 if num in u:
#                     used = True
#                     break
#             if not used:
#                 notused.append(s)

#     print("Not Used:", notused)

#     restart = True
#     while restart:
#         print("XD")
#         restart = False
#         for idx, nu in reversed(list(enumerate(notused))):
#             if checkIfJumpedTo(nu[0]):
#                 print("[NUsed] Popping "+str(nu[1])+" from "+str(nu[0])+" (debug:"+str(optAsm[nu[0]])+")")
#                 decreaseJumps(nu[0], "Because NotUsed, popping "+str(nu[1])+" from "+str(nu[0])+" (debug:"+str(optAsm[nu[0]])+")")
#                 optAsm.pop(nu[0])
#                 notused.pop(idx)
#                 restart = True
#                 break