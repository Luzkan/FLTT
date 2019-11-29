%option noyywrap
%{
#include "rpcalc.tab.hpp"
int yylex();
%}

%%
^#(.|\\\n)*\n ;
\\\n          ;
[[:blank:]]+  ;
[0-9]+        { yylval = atoi(yytext);
                return NUMBER;         }
"+"           { return PLUS;           }
"-"           { return MINUS;          }
"*"           { return MUL;            }
"/"           { return DIV;            }
"%"           { return MOD;            }
"^"           { return EXP;            }
"("           { return LBRACKET;       }
")"           { return RBRACKET;       }
.             { return DOT;            }
\n            { return NEWLINE;        }
EOF           { return 0;              }
%%
