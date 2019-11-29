# Flex & Bison RP Calculator
It's a simple program that translates algebraic expression into from inflixic to postfix form (*into polish notation*) and then calculates it.
Expressions are placed on separate lines. Program goes through all the lines and process them normally except all the lines that starts with # which treated as comments. In the case of long lines it's possible to divide them using the "\\" character (as in the c language).

## Features: 
  - Right operator priorities
  - Proper connectivity of operators
  - Proper Error Handling
  - Unary operator - for negative numbers.
  - The rest of division always has the same sign as the divisor

## What's Flex
**Flex** is scanner generator - scans all the input and check if input is correct or not. If it is then it sends a particular token (or send a signal) to other systems to let them know that the input is correct and what the input means. It is a lexical analizer. For example `int a;` is going to be break into `int` `a` and `;`.

## Installation
To run this program on Windows we need [Bison & Flex](https://sourceforge.net/projects/winflexbison/files/) and [G++](http://mingw-w64.org/doku.php/download/mingw-builds) compiler.
For Flex and Bison:

  - Unpack the *Bison & Flex* into `C:\FlexBison`
  - Add `C:\FlexBison` to system `PATH`
    - ```$ export PATH="C:\FlexBison```

For G++ compiler:

  - Install **mingw-w64** into default `Program Files` based path
    - Change *architecture* to `x86_64` *(64bit)* from `i686` *(32bit)*
    - Change *threads* to `win32`
  - Add the `bin` folder to the system `PATH`
    - ```$ export PATH="C:\Program Files\mingw-w64\<VERSION>\mingw64\bin":$PATH```

## Compilation
To compile the file type in *Git Bash Console* (make sure you are in proper folder):

```sh
$ win_flex -o lexer.l.c lexer.l
$ win_bison -d calc.ypp
$ g++ -o BisonCalculator calc.tab.cpp lexer.l.c -lm
```

This will create an executable *BisonCalculator.exe*.
To clean the folder after use:

```sh
$ rm calc.tab.* lexer.l.* BisonCalculator
```

### Makefile
If you have command `make` available you can run the makefile with command `make` to compile the program.
Use `clear` if you want to clean up all generated files through compilation. 
```make
all: rpcalc.tab lexer.l.c
	g++ -o RPCalc rpcalc.tab.cpp lexer.l.c -lm

lexer.l.c: lexer.l
	win_flex -o lexer.l.c lexer.l

rpcalc.tab: rpcalc.ypp
	win_bison -d rpcalc.ypp

clear:
	rm rpcalc.tab.* lexer.l.* RPCalc
```

##### Makefile installation
Install [Chocolatey](https://chocolatey.org/install) package manager for Windows and then in console run `choco install make`.

### Results:
| Input | Result | Calculated | Expected |
| ------ | ------ | ------ | ------ |
| 2^3^2 | 2 3 2 ^ ^ | 512 | 512 |
| 2-3-2 | 2 3 - 2 - | -3 | -3 |
| 2+3*(4-5 | Error: Syntax Error | Syntax Error | Syntax Error |
| 24/5%3 | 24 5 / 3 % | 1 | 1 |
| **-4/3** | 4 - 3 / | **-1** | **-2** |
| -4%3 | 4 - 3 % | 2 | 2 |
| 1--1 | 1 1 - - | 2 | 2 |
| # ala ma kota <br> 2-3-2 | 2 3 - 2 - | -3 | -3 |
| 2+3*(\\ <br> 4-5) | 2 3 4 5 - * + | -1 | -1 |

### Code Explanation:
[GNU Bison Manual](https://www.gnu.org/software/bison/manual/bison.html#Rpcalc-Main) was used to create the program.

#### `lexer.l`
```c
****** The %option noyywrap is used to inform the compiler that the function yywrap() has not been defined. ******
%option noyywrap

****** C Stuff ******
%{
#include "rpcalc.tab.hpp"
int yylex();
%}

****** Token Specifications ******
%%
*** Skip upon detecting line starting with # (a comment). ***
^#(.|\\\n)*\n ;
*** Skip upon detecting "\" (C-like linebreak). ***
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
```

#### `calc.ypp` (Rules Section)
```c
****** Complete input transcript ******
*** A complete input is either an empty string, or a complete input followed by an input line”. Notice that “complete input ***
input:
      %empty
    | input line
;

****** The line of input ******
*** First alternative is a token which is a newline character; this means that calc accepts a blank line (and ignores it, since there is no action). The second and third alternative is an expression or an error followed by a newline. ***
line:
    NEWLINE
    | exp NEWLINE {
        if (!error) {
            std::cout << rp_notation << std::endl << "=> " << $1 << std::endl;
        }
        error = false;
        rp_notation = "";
    }
    | error NEWLINE {
        std::cerr << "[Err] Syntax Error" << std::endl;
        error = false;
        rp_notation = "";
    }
;

****** Expressions that the calculator can consider ******
*** For example, in the rule for addition, $1 refers to the first component exp and $3 refers to the second one. The third component, '+', has no meaningful associated semantic value, but if it had one you could refer to it as $2 ***
exp:
      NUMBER                {   result += std::to_string($1) + " "; $$ = $1;}
    | exp DOT exp           {   yyerror ("Please use only integers.");      }
    | exp PLUS exp          {   result += "+ "; $$ = $1 + $3;               }
    | exp MINUS exp         {   result += "- "; $$ = $1 - $3;               }
    | exp MUL exp           {   result += "* "; $$ = $1 * $3;               }
    | exp DIV exp           {   result += "/ ";
                                if ($3 != 0) {
                                    $$ = $1 / $3;
                                } else {
                                    yyerror ("You can not divide by zero.");
                                }                                           } 
    | exp MOD exp           {   result += "% ";
                                if ($3 != 0) {
                                    $$ = ($1 % $3 + $3) % $3;
                                } else {
                                    yyerror ("You can not modulo by zero.");
                                }                                           }
    *** %prec simply instructs Bison that the rule ‘| '-' exp’ has the same precedence as NEG ***
    | MINUS exp %prec NEG   {   result += "- "; $$ = -$2;                   }
    | exp EXP exp           {   result += "^ "; $$ = pow ($1, $3);          }
    | LBRACKET exp RBRACKET {   $$ = $2;                                    }
;
```