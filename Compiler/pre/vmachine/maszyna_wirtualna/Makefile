FLAGS = -W -pedantic -std=c++17 -O3

.PHONY = all clean cleanall

all: maszyna_wirtualna maszyna_wirtualna_cln

maszyna_wirtualna_cln: lexer.o parser.o mw_cln.o main.o
	$(CXX) $^ -o $@ -l cln
	strip $@

maszyna_wirtualna: lexer.o parser.o mw.o main.o
	$(CXX) $^ -o $@
	strip $@

%.o: %.cc
	$(CXX) $(FLAGS) -c $^

lexer.cc: lexer.l parser.hh
	win_flex -o $@ $<

parser.cc parser.hh: parser.y
	win_bison -Wall -d -o parser.cc $^

clean:
	rm -f *.o parser.cc parser.hh lexer.cc

cleanall: clean
	rm -f maszyna_wirtualna maszyna_wirtualna_cln
