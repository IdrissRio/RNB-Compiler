# Red is Not Blue (Compiler) [![Build Status](https://travis-ci.com/IdrissRio/RNB-Compiler.svg?branch=master)](https://travis-ci.com/IdrissRio/RNB-Compiler/jobs/197894997)



RNB-Compiler is an accademic project, developed during my Master Degree at the University of Udine.
The project has the purpose to build the front-end and the middle-end of a compiler.

## Main goal:
  - Design an imperative language
  - Develop the compiler front-end and middle-end in Haskell 

## Minor goal:
  - Design the grammar and the syntax of the languague
  - Design a readable Abstract Syntax Tree
  - Design a Typechecker
  - Design the Static Analysis module
  - Design the TAC generator


# Main Features
-  A very readable AST
-  Exhaustive diagnosis
-  Clear TAC code generatred


### Installation

RNB requires [GHC](https://www.haskell.org/ghc/), [Cabal](https://www.haskell.org/cabal/), [Happy](https://www.haskell.org/happy/) and [Alex](https://www.haskell.org/alex/).

```sh
$ sudo apt-get install ghc cabal-install
$ cabal update
$ cabal install alex happy
$ make
```
This will produce the binary file `./rnbc`.
### Usage
Running `rnbc` with the flag `--help` we will obtain

```
   ___  _  _____  _____ 
  / _ \/ |/ / _ )/ ___/ 
 / , _/    / _  / /__   
/_/|_/_/|_/____/\___/ompiler   

  --help                Display this help message.
  (files)               Parse content of files verbosely.
  --check (files)       Check the correctness of the RNBStaticAnalysis 
                        with the powerful Test-Suite.
  --verbose (files)     Show all prints.  
  --silent (files)      Remove all prints. 
  --checkTAC (files)    Check if the generated TAC is equal to the expected TAC.
  --nocolor (files)     Print output without colors.
  ```

# Contributors
In order :
- Anna Becchi
- Renato Eugenio Garavaglia
- Riouak Idriss


## Example
What follows is the output obtained executing `./rnbc tests/demo02`:
```
== Control flow and test:
const x : int := 5;
proc main () is
  initmain : int;
  loop
    beforeexiton : int;
    exiton (x = 5);
    afterexiton : int;
  end loop

  beforeloop : int;
  loop
   insideloop : int;
   proc f() is
     fvar : char;
     g();
     continue;
   end f
   exit;
   proc g() is
     f();
     proc f() is
       fgvar : string;
       g();
     end f
     gvar : string;
   end g
  end loop
  afterloop : int;
end main

const b : bool := True;
const b1 : bool := False;
b2 : bool := b <> b1;
b3 : bool := b = b2;

func test() -> (y : int) is
  const x : int := 6;
  y := 18;
  if (x < y) then
     thenvar : char;
     proc f () is
          fthenvar : int;
     end f
  elseif (3 < 6) then
    elseifvar : char;
    local
      return;
    end local
    f();
  else
     f();
     elsevar : int;
     proc f() is
          felsevar : int := x;
     end f
  end if

  func f() -> (result : int) is
       return;
       outerf : int;
       result  := outerf;
  end f
  const n : int := 5;

  proc test2 () is
    x : int := 6;
    y : int := 9;

    case x of
      5+4 -> ycase5 : int;
      7 -> ycase7 : int;
      8 -> ycase8 : int;
        loop
          y := 9;
          exiton (x = 8);
          y := 10;
          continue;
          y := 11;
        end loop
        endloopvar : int := 1994;
      6 -> ycase6 : int;
    end case
  end test2

end test
```
```
[PARSING]: PASS

[Info]: Abstract Syntax Tree
`- ConstDef 'x', <line:3,col:7>, int
| `- ExprVal Int  5, <line:3,col:18>
`- ProcDef'main', <line:5,col:6>
| `- StmtDecl 
| | `- VarDecl 'initmain', <line:7,col:3>, int
| `- StmtLoop 
| | `- StmtDecl 
| | | `- VarDecl 'beforeexiton', <line:10,col:5>, int
| | `- StmtLoopCmd 
| | | `- LoopExitOn
| | |   `- ExprBrack 
| | |     `- ExprRel
| | |       `- ExprLeft 
| | |       | `- LEId 'x', <line:11,col:13>
| | |       `- RelEq
| | |       `- ExprVal Int  5, <line:11,col:17>
| | `- StmtDecl 
| |   `- VarDecl 'afterexiton', <line:12,col:5>, int
| `- StmtDecl 
| | `- VarDecl 'beforeloop', <line:15,col:3>, int
| `- StmtLoop 
| | `- StmtDecl 
| | | `- VarDecl 'insideloop', <line:18,col:4>, int
| | `- StmtDecl 
| | | `- ProcDef'f', <line:19,col:9>
| | |   `- StmtDecl 
| | |   | `- VarDecl 'fvar', <line:20,col:6>, char
| | |   `- StmtExpr 
| | |   | `- ExprCall 'g', <line:21,col:6>
| | |   `- StmtLoopCmd 
| | |     `- LoopContinue
| | `- StmtLoopCmd 
| | | `- LoopExit
| | `- StmtDecl 
| |   `- ProcDef'g', <line:27,col:9>
| |     `- StmtExpr 
| |     | `- ExprCall 'f', <line:28,col:6>
| |     `- StmtDecl 
| |     | `- ProcDef'f', <line:29,col:11>
| |     |   `- StmtDecl 
| |     |   | `- VarDecl 'fgvar', <line:30,col:8>, string
| |     |   `- StmtExpr 
| |     |     `- ExprCall 'g', <line:31,col:8>
| |     `- StmtDecl 
| |       `- VarDecl 'gvar', <line:33,col:6>, string
| `- StmtDecl 
|   `- VarDecl 'afterloop', <line:38,col:3>, int
`- ConstDef 'b', <line:42,col:7>, bool
| `- ExprVal Bool True, <line:42,col:19>
`- ConstDef 'b1', <line:43,col:7>, bool
| `- ExprVal Bool False, <line:43,col:20>
`- VarDef 'b2', <line:44,col:1>, bool
| `- ExprRel
|   `- ExprLeft 
|   | `- LEId 'b', <line:44,col:14>
|   `- RelNotEq
|   `- ExprLeft 
|     `- LEId 'b1', <line:44,col:19>
`- VarDef 'b3', <line:45,col:1>, bool
| `- ExprRel
|   `- ExprLeft 
|   | `- LEId 'b', <line:45,col:14>
|   `- RelEq
|   `- ExprLeft 
|     `- LEId 'b2', <line:45,col:18>
`- FuncDef 'test', <line:47,col:6>
  `- 'y', <line:47,col:17>, int
  `- StmtDecl 
  | `- ConstDef 'x', <line:48,col:9>, int
  |   `- ExprVal Int  6, <line:48,col:20>
  `- StmtAssign 
  | `- LEId 'y', <line:49,col:3>
  | `- ExprVal Int  18, <line:49,col:8>
  `- StmtIf 
  | `- ExprBrack 
  | | `- ExprRel
  | |   `- ExprLeft 
  | |   | `- LEId 'x', <line:50,col:7>
  | |   `- RelLess
  | |   `- ExprLeft 
  | |     `- LEId 'y', <line:50,col:11>
  | `- StmtDecl 
  | | `- VarDecl 'thenvar', <line:51,col:6>, char
  | `- StmtDecl 
  | | `- ProcDef'f', <line:52,col:11>
  | |   `- StmtDecl 
  | |     `- VarDecl 'fthenvar', <line:53,col:11>, int
  | `- StmtElseIf
  |   `- ExprBrack 
  |   | `- ExprRel
  |   |   `- ExprVal Int  3, <line:55,col:11>
  |   |   `- RelLess
  |   |   `- ExprVal Int  6, <line:55,col:15>
  |   `- StmtDecl 
  |   | `- VarDecl 'elseifvar', <line:56,col:5>, char
  |   `- StmtLocal 
  |   | `- StmtReturn 
  |   `- StmtExpr 
  |   | `- ExprCall 'f', <line:60,col:5>
  |   `- StmtElse
  |     `- StmtExpr 
  |     | `- ExprCall 'f', <line:62,col:6>
  |     `- StmtDecl 
  |     | `- VarDecl 'elsevar', <line:63,col:6>, int
  |     `- StmtDecl 
  |       `- ProcDef'f', <line:64,col:11>
  |         `- StmtDecl 
  |           `- VarDef 'felsevar', <line:65,col:11>, int
  |             `- ExprLeft 
  |               `- LEId 'x', <line:65,col:29>
  `- StmtDecl 
  | `- FuncDef 'f', <line:69,col:8>
  |   `- 'result', <line:69,col:16>, int
  |   `- StmtReturn 
  |   `- StmtDecl 
  |   | `- VarDecl 'outerf', <line:71,col:8>, int
  |   `- StmtAssign 
  |     `- LEId 'result', <line:72,col:8>
  |     `- ExprLeft 
  |       `- LEId 'outerf', <line:72,col:19>
  `- StmtDecl 
  | `- ConstDef 'n', <line:74,col:9>, int
  |   `- ExprVal Int  5, <line:74,col:20>
  `- StmtDecl 
    `- ProcDef'test2', <line:76,col:8>
      `- StmtDecl 
      | `- VarDef 'x', <line:77,col:5>, int
      |   `- ExprVal Int  6, <line:77,col:16>
      `- StmtDecl 
      | `- VarDef 'y', <line:78,col:5>, int
      |   `- ExprVal Int  9, <line:78,col:16>
      `- StmtCase 
        `- ExprLeft 
        | `- LEId 'x', <line:80,col:10>
        `- CaseVal
        | `- ExprSum 
        | | `- ExprVal Int  5, <line:81,col:7>
        | | `- ExprVal Int  4, <line:81,col:9>
        | `- StmtDecl 
        |   `- VarDecl 'ycase5', <line:81,col:14>, int
        `- CaseVal
        | `- ExprVal Int  7, <line:82,col:7>
        | `- StmtDecl 
        |   `- VarDecl 'ycase7', <line:82,col:12>, int
        `- CaseVal
        | `- ExprVal Int  8, <line:83,col:7>
        | `- StmtDecl 
        | | `- VarDecl 'ycase8', <line:83,col:12>, int
        | `- StmtLoop 
        | | `- StmtAssign 
        | | | `- LEId 'y', <line:85,col:11>
        | | | `- ExprVal Int  9, <line:85,col:16>
        | | `- StmtLoopCmd 
        | | | `- LoopExitOn
        | | |   `- ExprBrack 
        | | |     `- ExprRel
        | | |       `- ExprLeft 
        | | |       | `- LEId 'x', <line:86,col:19>
        | | |       `- RelEq
        | | |       `- ExprVal Int  8, <line:86,col:23>
        | | `- StmtAssign 
        | | | `- LEId 'y', <line:87,col:11>
        | | | `- ExprVal Int  10, <line:87,col:16>
        | | `- StmtLoopCmd 
        | | | `- LoopContinue
        | | `- StmtAssign 
        | |   `- LEId 'y', <line:89,col:11>
        | |   `- ExprVal Int  11, <line:89,col:16>
        | `- StmtDecl 
        |   `- VarDef 'endloopvar', <line:91,col:9>, int
        |     `- ExprVal Int  1994, <line:91,col:29>
        `- CaseVal
        | `- ExprVal Int  6, <line:92,col:7>
        | `- StmtDecl 
        |   `- VarDecl 'ycase6', <line:92,col:12>, int
        `- CaseEps


[Warning]: Expression result unused. (line: 60, column: 5) [ControlFlowAnalysis].
    f();
~~~~^
[Warning]: The return variable 'result' may be never initialized. (line: 69, column: 16) [ControlFlowAnalysis].
  func f() -> (result : int) is
~~~~~~~~~~~~~~~^
[Warning]: Dead code after 'return' statement. (line: 70, column: 8) [ControlFlowAnalysis].
       return;
~~~~~~~^

[Warning]: Found 3 warnigs.
[Info]: TAC Generator
              // Constant x@3,7 has value 5
              // Constant b@42,7 has value True
              // Constant b1@43,7 has value False
              b2@44,1             =bool  True
              ifRel (True not_eq_bool b2@44,1) goto l20
              t0                  =bool  True
              goto l19
 l20:         t0                  =bool  False
 l19:         b3@45,1             =bool  t0
              call main@5,6
              exit 
 
 main@5,6:    // Preamble main
              // Body main
              initmain@7,3        =int   0
 l1:
 l3:          beforeexiton@10,5   =int   0
 l5:          goto l2
 l6:          afterexiton@12,5    =int   0
 l4:          goto l3
 l2:          beforeloop@15,3     =int   0
 l7:
 l9:          insideloop@18,4     =int   0
 l11:         goto l8
 l10:         goto l9
 l8:          afterloop@38,3      =int   0
 l0:          // Postamble main
              return 
 
 f@19,9:      // Preamble f
              // Body f
              fvar@20,6           =char  '\u0000'
 l13:         call g@27,9
 l14:         goto l9
 l12:         // Postamble f
              return 
 
 g@27,9:      // Preamble g
              // Body g
              call f@29,11
 l16:         gvar@33,6           =addr  str0
 l15:         // Postamble g
              return 
 
 f@29,11:     // Preamble f
              // Body f
              fgvar@30,8          =addr  str0
 l18:         call g@27,9
 l17:         // Postamble f
              return 
 
 test@47,6:   // Preamble test
              y@47,17             =int   0
              // Body test
              // Constant x@48,9 has value 6
 l22:         y@47,17             =int   18
 l23:         ifRel (6 gr_eq_int y@47,17) goto l24
              thenvar@51,6        =char  '\u0000'
              goto l21
 l24:         elseifvar@56,5      =char  '\u0000'
 l27:         goto l21
 l28:         t1                  =int   call f@69,8
              goto l21
 l26:         call f@64,11
 l29:         elsevar@63,6        =int   0
 l31:         // Constant n@74,9 has value 5
 l21:         // Postamble test
              return y@47,17
 
 f@52,11:     // Preamble f
              // Body f
              fthenvar@53,11      =int   0
 l25:         // Postamble f
              return 
 
 f@64,11:     // Preamble f
              // Body f
              felsevar@65,11      =int   6
 l30:         // Postamble f
              return 
 
 f@69,8:      // Preamble f
              result@69,16        =int   0
              // Body f
              goto l32
 l33:         outerf@71,8         =int   0
 l34:         result@69,16        =int   outerf@71,8
 l32:         // Postamble f
              return result@69,16
 
 test2@76,8:  // Preamble test2
              // Body test2
              x@77,5              =int   6
 l36:         y@78,5              =int   9
 l37:         t2                  =int   x@77,5
              ifRel (9 not_eq_int t2) goto l38
              ycase5@81,14        =int   0
              goto l35
 l38:         ifRel (7 not_eq_int t2) goto l39
              ycase7@82,12        =int   0
              goto l35
 l39:         ifRel (8 not_eq_int t2) goto l40
              ycase8@83,12        =int   0
 l41:
 l43:         y@78,5              =int   9
 l45:         t3                  =int   x@77,5
              ifRel (t3 equal_int 8) goto l42
 l46:         y@78,5              =int   10
 l47:         goto l43
 l48:         y@78,5              =int   11
 l44:         goto l43
 l42:         endloopvar@91,9     =int   1994
              goto l35
 l40:         ifRel (6 not_eq_int t2) goto l49
              ycase6@92,12        =int   0
              goto l35
 l49:
 l35:         // Postamble test2
              return 
############   Static Data  ############
 str0:        ""
 ```
