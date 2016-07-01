# PP-project-2016

Implementation the University of Twente Programming Paradigms' final project.

Aim is to provide a simple implementation, using Haskell, from tokenizing to code generation. 

We use Trello to track our progress: 
https://trello.com/b/2L7Oxxjg/pp-project-2016

## Running a program
To run a program, you can use the compiled Main, or you might need to compile main:

`ghc --make Main.hs`

*To be able to compile the compiler you need to have a version of haskell installed. We have tested it with haskell-minimal (https://www.haskell.org/downloads). You will also need to install FPPrac.Trees and possibly Eventloop, these can be installed as described in BlackBoard>Courses>Programming Paradigms (2015-2B)>General>Resources for Functional Programming>Installation Manual and Reference Functional Programming.pdf*

Now simply run main and follow the instructions:

`./Main`

These commands have only been tested on Ubuntu.

All our test files are located in `test/`

Make sure your program has the `.shl` extension
