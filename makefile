.ONESHELL:

all: lexer parser

lexer:
	ocamlc -c lexer.mli
	ocamlc -c lexer.ml

parser: lexer

	ocamlc -c parser.mli
	ocamlc -c parser.ml

clean:
	rm -f *.cmo *.cmi
