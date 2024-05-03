# We are not really tracking dependencies because everything is small
# enough to recompile at will.

# change to a different ocamlc if you prefer (e.g., ocamlopt)
COMPILER=ocamlc
 
all: clean cish cppish

mips:
	$(COMPILER) -c word32.ml
	$(COMPILER) -c mips.ml

cish: mips
	$(COMPILER) -c cish_ast.ml
	ocamlyacc cish_parse.mly
	$(COMPILER) -c cish_parse.mli
	$(COMPILER) -c cish_parse.ml
	ocamllex cish_lex.mll
	$(COMPILER) -c cish_lex.ml
	$(COMPILER) -c cish_eval.ml
	$(COMPILER) -c cish_compile.ml
	$(COMPILER) -c cish.ml
	$(COMPILER) -o my_cish cish_ast.cmo cish_parse.cmo cish_lex.cmo cish_eval.cmo word32.cmo mips.cmo cish_compile.cmo cish.cmo

cppish: mips
	$(COMPILER) -c cish_ast.ml
	$(COMPILER) -c cppish_ast.ml
	ocamlyacc cppish_parse.mly
	$(COMPILER) -c cppish_parse.mli
	$(COMPILER) -c cppish_parse.ml
	ocamllex cppish_lex.mll
	$(COMPILER) -c cppish_lex.ml
	$(COMPILER) -c cppish_compile.ml
	$(COMPILER) -c cppish.ml
	$(COMPILER) -o my_cppish cish_ast.cmo cppish_ast.cmo cppish_parse.cmo cppish_lex.cmo word32.cmo mips.cmo cppish_compile.cmo cppish.cmo

clean:
	-rm *.cmo *.cmi my_cish my_cppish cish_parse.ml cish_parse.mli cish_lex.ml cppish_parse.ml cppish_parse.mli cppish_lex.ml
