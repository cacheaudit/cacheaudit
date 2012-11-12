OCAMLC= ocamlc.opt -g
OCAMLYACC= ocamlyacc -v
OCAMLLEX= ocamllex

OCAMLINCLUDE:= -I x86_frontend -I concrete_interpreter -I iterator

#OCT_INCLUDE= $(shell oct-config --mlflags | sed 's/_iag//')
OCT_INCLUDE= $(shell oct-config --mlflags)

ML_FILES := \
	x86_frontend/asmUtil.ml \
	x86_frontend/x86Util.ml \
	x86_frontend/x86Types.ml \
	x86_frontend/x86Print.ml \
	x86_frontend/x86Parse.ml \
	x86_frontend/execInterfaces.ml \
	x86_frontend/elf.ml \
	x86_frontend/macho.ml \
	x86_frontend/x86Headers.ml \
	concrete_interpreter/option.ml \
	iterator/cfg.ml\
	iterator/signatures.ml\
	iterator/stackAD.ml\
	iterator/valAD.ml\
	iterator/simpleValAD.ml\
	iterator/flagAD.ml\
	iterator/octAD.ml\
	iterator/simpleOctAD.ml\
	iterator/ageFunction.ml\
	iterator/ageFunctionSet.ml\
	iterator/relSetMap.ml\
	iterator/simpleRelSetAD.ml\
	iterator/simpleProfilingValAD.ml\
	iterator/cacheAD.ml\
	iterator/relCacheAD.ml\
	iterator/memAD.ml\
	iterator/iterator.ml\
	concrete_interpreter/registers.ml \
	concrete_interpreter/flags.ml \
	concrete_interpreter/vecStack.ml \
	concrete_interpreter/cache.ml\
	concrete_interpreter/interpreter.ml\
	config.ml


AUTOGEN = parser-lexer/lex.ml parser-lexer/parser.ml

all: cachecow

%.ml: %.mll
	$(OCAMLLEX) $*.mll

%.output %.ml %.mli: %.mly
	$(OCAMLYACC) $*.mly

%.cmo: %.ml %.cmi
	$(OCAMLC) $(OCAMLINCLUDE) $(OCT_INCLUDE) -c $*.ml

%.cmi: %.mli
	$(OCAMLC) $(OCAMLINCLUDE) -c $*.mli

%.cmo: %.ml
	$(OCAMLC) $(OCAMLINCLUDE) $(OCT_INCLUDE) -c $*.ml

CMO_FILES= $(ML_FILES:%.ml=%.cmo)

cachecow: $(CMO_FILES) cachecow.ml
	$(OCAMLC) $(OCAMLINCLUDE) str.cma $(OCT_INCLUDE) -o $@ $+

clean:
	rm -f depend cachecow */*.cmo */*.cmi */*~ *.cmo *.cmi *~

depend: $(AUTOGEN)
	ocamldep $(OCAMLINCLUDE) iterator/*.ml iterator/*.mli x86_frontend/*.ml x86_frontend/*.mli concrete_interpreter/*.ml concrete_interpreter/*.mli parser-lexer/*.ml parser-lexer/*.mli *.ml *.mli > depend

dep:
	rm -f depend
	$(MAKE) depend

include depend

doc:
	-ocamldoc -latex -o documentation.tex $(OCAMLINCLUDE) $(ML_FILES)
	pdflatex documentation.tex
	pdflatex documentation.tex


.PHONY: all clean dep
