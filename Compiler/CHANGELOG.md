# Changelog
All notable changes to this project will be documented in this file.

Decided to create a changelog after #7th optimalization of the compiler.
Pre version [1.2.0] the history is roughly how the creation cycle went.

*Format legend [x.y.z] - x is for fully functional version, y is for major function implementation and z is for minor things added or bugfixes.* 

___________________________________

## [1.6.5] - 2019-01-31
### Optimized #13
- Faster checks for special values
- Checks for negative special values
- **This optimization went out even** in some tests it did improve the timing, but in some it reduced, overall test time improvement is **0,04%** due to nature of the tests (multiplications are done mostly positive integers)

## [1.6.4] - 2019-01-31
### Optimized
- Found one redundant check to zero with jump in multiplication and removed it
- Multiplication is also faster because of sign switch condition to make left always positive (see Fixed)
### Fixed
- Previous additional left side check of multiplication in special cases labeled in [1.6.3a] now costs just ~0 in most number of cases and on detection it makes whole algorithm faster instead of previous enormous amounts of time added with another way of solving that problem. Btw. it works better than the Signed Multiplication that is recommended in book - it does not use special sign flag, nor reverses up to 5 times, but only twice. Only one if condition (LE 0) is needed instead of three. Good shower thoughts!  

## [1.6.3b] - 2019-01-30
### Hotfix
- Infinite Loop Check in proper place

## [1.6.3a] - 2019-01-30
### Added
- Infinite Loop Check (if iterating to value that cannot be reached; added for 0 cost in most cases)
### Fixed
- One line code inputs are now properly handled.
- Handling programs without declarations
- Error when testing occuredOnce() when there is only one commandline to test.
- Syntax Errors now quit with different code.
- Added additional check when left side of multiplication is negative (sometimes it went into infinite loop due to '-1' lastbit operation - it costs additional time in asm sadly)
### Compilation Time
- Improved Compilation Time for loop unrolling without assembly time lost by ~14100%

## [1.6.2] - 2019-01-30
### Changed
- Input/Output filepaths and naming. 

## [1.6.1] - 2019-01-29
### Added
- [README.md](README.md) with informations about the compiler, installation, files and usage.
### Tested 
- Additionally tested the compiler succesfully on *Ubuntu 18.04, Python 3.6.9*.
- *Small Note: Recovered the compiler from a PC that was dead due to BSOD "BAD_SYSTEM_CONFIG_INFO" :(*

## [1.6.0] - 2019-01-24
### Optimalization
- Optimalized (#11) loops that can be unrolled, altough because of that the compilation time for nested loop suffers - the time performance using generated assembly file is quite significant though. 

## [1.5.0] - 2019-01-23
### Optimalization
- Optimalized (#10) addition and subtracking by "1".

## [1.4.0] - 2019-01-23
### Optimalization
- Optimalized (#9) STORES at the beginning (if possible - now one memory is used, unless it's for a table), removed redundants. Very small benefits but hey, it's something.

## [1.3.3] - 2019-01-23
### Optimalization
- Optimalized /DOUBLE/ and /HALF/ to do always do the operation in the lowest cost (cost: 10).

## [1.3.2] - 2019-01-23
### Fixed
Fixed /clean/ -> /opt/ optimalization problem which generated errors on jump missing cases ex:

    LOAD 1
    JUMP 4
    SUB 0
    STORE 2
    STORE 3

Here "STORE 3" would be removed out because it's not directly jumped on, but the value in this memory could be re-changed in a loop.

## [1.3.1] - 2019-01-23
### Changed
- The style of comments to be more readable in the output program. Now they dont display all the possible informations, just the most important.

### Fixed
- Fixed modulos as they seemed to work properly unless other inputs than in the original tests were provided. Now they are properly floored out and work in all possible cases.
- Fixed modulo wrong line jump problem.

## [1.3.0] - 2019-01-23
### Optimalization
- Optimalization for non-value (declarations / tables) operations on numbers with "0", "1" and "2"

## [1.2.0] - 2019-01-06
### Optimalization
- Optimalization for value based operations where the value can be determined without any calculation needed on the output assembly program. 

## [1.1.0] - 2019-01-05
### Optimalization
- Optimalization to gather all permanently, unchangeable declarations and numbers used in a program and put the values in memory at the start of the program.

## [1.0.0] - 2019-01-04
### Description:
Fully functional compiler for all the provided tests.
