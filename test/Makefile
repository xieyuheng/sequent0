all: lexer_test parser_test

lexer_test:
	ocamlc -I .. -c ../lexer.mli
	ocamlc -I .. -c ../lexer.ml
	ocamlc -I .. -c lexer_test.ml
	ocamlc -o lexer_test ../lexer.cmo lexer_test.cmo

parser_test:
	ocamlc -I .. -c ../lexer.mli
	ocamlc -I .. -c ../lexer.ml
	ocamlc -I .. -c ../parser.mli
	ocamlc -I .. -c ../parser.ml
	ocamlc -I .. -c parser_test.ml 
	ocamlc -o parser_test ../lexer.cmo ../parser.cmo parser_test.cmo

clean:
	rm -f *_test
