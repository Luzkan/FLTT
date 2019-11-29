all: rpcalc.tab lexer.l.c
	g++ -o RPCalc rpcalc.tab.cpp lexer.l.c -lm

lexer.l.c: lexer.l
	win_flex -o lexer.l.c lexer.l

rpcalc.tab: rpcalc.ypp
	win_bison -d rpcalc.ypp

clear:
	rm rpcalc.tab.* lexer.l.* RPCalc
