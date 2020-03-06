# Compiler
This is a compiler for the language given on the final task of the "formal languages and translation theory" course.

**Version History: [CHANGELOG.md](CHANGELOG.md)** 

## The author's words about himself, the world and meaning of existance
This is actually my ***First*** spit-out both in writing anything (ever) in Python as well as writing my own assembler. After completing this project, I would took different approach to this project (mostly on detail-scale - use Python much more efficiently and properly). Even though the compiler is - indeed - compiling and it does it's job very well giving assembly code that works *fast*, you know, that's cool and all that, but I'm unhappy with the code quality I have written (talking about output code optimizations), especially when I was grinding on my own deadlines.
To end it with a positive note: however strange the code/comment structure looks like - it was actually a great, very bright light bulb that shined over my head that made me do it. And really I still think it was neat. Creating ASM Command + Comment functions which forced me to comment every step of the compiler made debugging (using output assembly code) **super easy**, speeding up the whole thing immensely. Not only that - returning to the code snippets had guaranteed explanation on every step of it. That's probably why I had this much time to write all the funky functions to manipulate output assembly code to squeeze extra xx% of performance. 

## Installation
### Windows
To run this program on Windows we need [Bison & Flex](https://sourceforge.net/projects/winflexbison/files/), [Python](https://www.python.org/downloads/) interpreter, [PLY](https://www.dabeaz.com/ply/) library with lex and yacc implementation for Python.
**Flex and Bison**:

  - Unpack the *Bison & Flex* into `C:\FlexBison`
  - Add `C:\FlexBison` to system `PATH`
    - ```$ export PATH="C:\FlexBison```

**Python**:

  - Download execetuable file for *Python 3.8.0* or newer 
  - Follow the steps in the installator

### Linux
The requirements are the same just like for Windows *(tested on [Ubuntu 18.04](https://ubuntu.com/download/desktop) with [Python](https://linuxize.com/post/how-to-install-python-3-8-on-ubuntu-18-04/) 3.6.9)*.
**Python Installation**:

  - First check if Python is pre-intalled on your Linux distribution: `python3 --version` or `python3.8 --version`
  - If not, follow these steps:
    - If you are on Ubuntu 16.10 or newer:
        - `sudo apt update`
        - `sudo apt install python3`
    - If you are using another version of Ubuntu (eg. LTS release):
        - `sudo apt-get install software-properties-common`
        - `sudo add-apt-repostiroy ppa:deadsnakes/ppa`
        - `sudo apt update`
        - `sudo apt install python3`

**PLY (and Python Pip)**:
  - `sudo apt install python3-pip`
  - `pip3 install ply`

## Usage
Use Python3 command with the main file and specify input text file with code to compile into output assembly code. Some example tests are in, so to use them add `pre/tests/` before input name.
Windows:
```sh
$ python main.py 'input' 'output'
```

Linux:
```sh
$ python3 main.py 'input' 'output'
```

## Files
 - [main.py](main.py) - Main compiler file, handles everything from input/output, through tokenization, gramatic, parser tree creation, syntax error detection, processing into assembly code, up to optimization while and after processing.
 - [CHANGELOG.md](CHANGELOG.md) - Contains version history of the compiler.
 - [README.md](README.md) - It's this file.
 - [Results.xlsx](Results.xlsx) - Excel File containing gathered time results from various iterations of optimization of this compiler.
 - [/pre/](/pre/)
     - [/tests/](/tests/) - Code files to test the compiler
     - [/vmachine/](/vmachine/) - Code files with various tests for error handling
        - [/maszyna_wirtualna/](/maszyna_wirtualna/) - Virtual Machine files that use assembly code created by the compiler
 - [legacy.py](legacy.py) - Code which is no longer used, broken (not working in all cases) or unfinished. It's not used by the compiler.

## Functionality (as specified in the task):
- Arithmetic operations on integers
    - Dividing by "0" gives answer and remain equal to "0"
    - PLUS, MINUS, TIMES, DIV, MOD expressions
- Conditions
    - EQ, NEQ, LE, GE, LEQ, GEQ relations
- Commands
    - ASSIGN attribution
    - WHILE loop
    - DO loop
    - FOR loops
        - with local iterator that cannot be changed within loop operations
        - FROM loop (+1)
        - DOWNTO loop (-1)
    - READ that reads users input
    - WRITE that prints that what's in current memory 0 (accumulator)
    - IF and IF ELSE statements
    - DECLARATIONS for values
        - that can be a single identifier
        - that can be a table(range)
- Files can be commented with beginning "[" and "]" indicating end of comment
- Big and small letters are distinguishable
- The rest of the program also works similiarly to other programming languages

## Author: *_Marcel Jerzyk_* 

