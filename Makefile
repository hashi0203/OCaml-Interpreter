SOURCES = tySyntax.ml constraintSolver.ml syntax.ml lexer.mll parser.mly infer.ml eval.ml main.ml
RESULT  = main

YFLAGS = -v 

all: byte-code byte-code-library

-include OCamlMakefile
