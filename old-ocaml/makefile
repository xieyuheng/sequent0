.ONESHELL:

all: lexer parser stack

lexer:
	ocamlc -c lexer.mli
	ocamlc -c lexer.ml

parser: lexer

	ocamlc -c parser.mli
	ocamlc -c parser.ml

stack:

	ocamlc -c stack.mli
	ocamlc -c stack.ml

clean:
	rm -f *.cmo *.cmi
