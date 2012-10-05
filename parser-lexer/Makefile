TARGET = main

main: lex.cmo parser.cmo main.cmo
	ocamlc -o $@ lex.cmo parser.cmo main.cmo

lex.ml: parser.cmi

clean:
	rm -rf *.cmi *.cmo $(TARGET)
	rm -rf parser.ml parser.mli lex.ml lex.mli

.SUFFIXES: .ml .mli .mll .mly .cmo .cmi

.ml.cmo:
	ocamlc -c $<
.mli.cmi:
	ocamlc -c $<
.mll.ml:
	ocamllex $<
.mly.ml:
	ocamlyacc $<
.mly.mli:
	ocamlyacc $<


