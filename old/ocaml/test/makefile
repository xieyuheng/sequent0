.ONESHELL:

all: lexer_test parser_test

lexer_test:
	cd ..
	make lexer
	cd test
	ocamlc -I .. -c lexer_test.ml
	ocamlc -o lexer_test ../lexer.cmo lexer_test.cmo

parser_test:
	cd ..
	make lexer
	make parser
	cd test
	ocamlc -I .. -c parser_test.ml
	ocamlc -o parser_test ../lexer.cmo ../parser.cmo parser_test.cmo

clean:
	rm -f *.cmo *.cmi
	rm -f *_test
