# Marcel Jerzyk - 22.10.2019

# Finite Automaton is a 5-tuple (Q, q0, A, Sigma, delta)
# Q     - Finite Set of States
# q0    - Starting      State
# A     - Accepting     States 
# Sigma - Finite        Input Alphabet
# delta - Transition    Function 
#                       (Q x Sigma  ->  Q) 

# We need to define sufix function (sigma) corresponding to a pattern P

# Ex:   P = "ababaca"
#
#       (1) --a--> (2) --b-->  (3)  --a-->  (4)  --b-->  (5)  --c-->  (6)  --a-->  [7] 
# 
#       Delta(5, b) =                                     /\
#                   = 4                      /\
#       
#       If Automaton will read "b" in state q = 5 then
#           Pqb = "ababab"
#       and the longest prefix of pattern P that is also a suffix of "ababab" is
#           P4 = "abab"

# Disclaimer: Cormen uses idx starting from "1" instead of "0".
#             this has been taken into account in the following algorithms


# 32.3 Cormen - Finite-Automaton-Matcher
# (Input Text, Transition Function, Accepting State)
#   Time on input "n": O(n + delta)
def finiteAutomatonMatcher(input, delta, pattern):
    inputLength = len(input)
    patternLength = len(pattern)
    q = 0
    occurence = []

    for i in range(inputLength):

        # Q is delta(<current state>, <currently read symbol>)
        #   Like in example: Delta(5, b) = 4  
        q = delta[q][input[i]]

        # If we reached accepting state 
        #   Like in example: Delta(6, a) = 7 
        if(q == patternLength):

            # Pattern occurs with shift (i - patternLength)
            shift = i - patternLength + 1
            occurence.append(shift)
    return occurence


# 32.3 Cormen - Compute-Transition-Function
# (Pattern, Alphabet)
#   Time on pattern "m": O(m^3 * |alphabet|)
def computeTransitionFunction(pattern, alphabet):

    # Transition Function
    delta = {}
    patternLength = len(pattern)

    for q in range(patternLength+1):
        delta[q] = {}
        for a in alphabet:

            # Starts running with bigget sensible "k" value...
            k = min(patternLength+1, q+2) - 1
            patternQ = pattern[:q]
            patternQ += a

            # print("Pattern: {}, Min (k): {}, Symbol (a): {}, Pattern[:k]: {}, PatternQ: {}".format(pattern, k, a, pattern[:k], patternQ))

            # ... and decreases until PatternQ+a suffix matches PatternK
            #     (endswitch returns True if strings ends with pattern suffix)
            while(not patternQ.endswith(pattern[:k])):
                k = k - 1
            delta[q][a] = k
    return delta

# 32.4 Cormen - Knuth-Morris-Pratt-Matcher
# (Input Text, Computed Prefix Function, Pattern)
#   Time on input "n": O(m + n)
#
#   Algorithm answers the question:
#   "Is the beginning part of the pattern appearing again somewhere in the pattern?"
#
#   Pattern examples:    a a a a b a a c d    |   a b c d e a b f a b c
#                        0 1 2 3 0 1 2 0 0    |   0 0 0 0 0 1 2 0 1 2 3
#
#   Upon failure we don't re-read, just move to the index that keeps our pattern in check.
def knuthMorrisPrattMatcher(input, pi, pattern):
    inputLength = len(input)
    patternLength = len(pattern)
    q = 0
    occurance = []

    for i in range(inputLength):
        while(q > 0 and pattern[q] != input[i]):
            q = pi[q - 1] + 1

        if(pattern[q] == input[i]):
            q += 1

        # If we reached accepting state 
        if(q == patternLength):
            shift = i - patternLength + 1
            occurance.append(shift)
            q = pi[q - 1] + 1
    return occurance

# 32.4 Cormen - Compute-Prefix-Function
# (Pattern)
#   Time on pattern "m": O(m)
def computePrefixFunction(pattern):
    patternLength = len(pattern)
    pi = {}
    pi[0] = -1
    k = -1

    # Starting from second
    for q in range(1, patternLength):

        while(k > -1 and pattern[k + 1] != pattern[q]):
            k = pi[k]

        if(pattern[k + 1] == pattern[q]):
            k += 1

        pi[q] = k
    return pi

def printResults(matcherType, pattern, occurance):
    print("[{}] Pattern {} occurs with shift: {}".format(matcherType, pattern, occurance))

testInput1 = "αβαβγβαβαβαβαβγ"
testAlphabet1 = ['α', 'β', 'γ', 'δ']

testInput2 = "tytytąąąąąątytytytytą"
testAlphabet2 = ['t', 'y', 'ą', '[']

testInput3 = "QÆQÆÆQÆÆÆQÆÆÆÆQÆQÆQÆQRÆQR"
testAlphabet3 = ['Æ', 'Q', 'R', 'T']

testInput4 = "1ąć1ąć1ąćą1ą1ć1ć1ć1ć"
testAlphabet4 = ['%', '1', 'ą', 'ć']

testInput5 = "roωrrρrωrωrωrrωrろωorrρρo"
testAlphabet5 = ['ρ', 'ω', 'r', 'o', 'ろ']

testCases = [
    (testInput1, "δ", testAlphabet1),
    (testInput1, "γδ", testAlphabet1),
    (testInput1, "αβ", testAlphabet1),
    (testInput1, "αβαβ", testAlphabet1),

    # (testInput2, "[", testAlphabet2),
    # (testInput2, "ą", testAlphabet2),
    # (testInput2, "tyt", testAlphabet2),
    # (testInput2, "ąą", testAlphabet2),
    # (testInput2, "ytą", testAlphabet2),
    # (testInput2, "ąty", testAlphabet2),

    # (testInput3, "T", testAlphabet3),
    # (testInput3, "ÆT", testAlphabet3),
    # (testInput3, "ÆQ", testAlphabet3),
    # (testInput3, "ÆÆ", testAlphabet3),
    # (testInput3, "QÆQ", testAlphabet3),
    # (testInput3, "QÆQÆ", testAlphabet3),

    # (testInput4, "%", testAlphabet4),
    # (testInput4, "%1", testAlphabet4),
    # (testInput4, "1ąć", testAlphabet4),
    # (testInput4, "1ć1", testAlphabet4),
    # (testInput4, "1ć1ć", testAlphabet4),
    # (testInput4, "ćą1", testAlphabet4),

    # (testInput5, "ろ", testAlphabet5),
    # (testInput5, "ろω", testAlphabet5),
    # (testInput5, "ωrωr", testAlphabet5),
    # (testInput5, "ρo", testAlphabet5),
    # (testInput5, "rr", testAlphabet5),
    # (testInput5, "rρrρ", testAlphabet5),
]

for test in testCases:
    (testInput, pattern, alphabet) = test
    testAutomaton = finiteAutomatonMatcher(testInput, computeTransitionFunction(pattern, alphabet), pattern)
    testKMP = knuthMorrisPrattMatcher(testInput, computePrefixFunction(pattern), pattern)
    printResults("ATMT", pattern, testAutomaton)
    printResults("KMPM", pattern, testKMP)