# Object Oriented Compiler (CPPish -> Cish)

Implemented an extension to the in-class compiler that maps CPPish down to Cish. CPPish supports the following language features: 
1. Objects (with named fields storing integers)
2. Classes & Inheritance
3. Class methods and method invocation

The file `cppish_ast.ml` gives the abstract syntax for CPPish programs, and the files `cppish_lex.mll` and `cppish_parse.mly` provide a rudimentary lexer and parser for CPPish. The file cppish_compile.ml has the core logic to translate CPPish AST to Cish AST and the file `cish_eval.ml` provides a direct interpreter for Cish.

The directory `tests/` contains the exhaustive testcases we have used to test out toy compiler. Some example output are listed in `expected_outputs/` directory.

Running make in the current directory will generate 2 executables 
1. my_cppish 
2. my_cish 

that can be used to test our compiler. The former expects a CPPish file and emits Cish code. The latter reads in a stream of Cish code from stdin and evaluates it using cish_eval.ml. To test our compiler, you can run the below command to quicky get an evaluated answer:

`./my_cppish <PATH_TO_CPPish_FILE> | ./my_cish`