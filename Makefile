PREPROCESSOR= gcc -E -P -C -w

ifneq ($(or $(oct),$(OCT)),)
        PREPROCESSOR += -DINCLUDE_OCT
endif

PREPROCESSOR += -x c

OCAMLC= ocamlc.opt -dtypes -pp "${PREPROCESSOR}"
OCAMLDEP= ocamldep -pp "${PREPROCESSOR}"
OCAMLYACC= ocamlyacc -v
OCAMLLEX= ocamllex

OCAMLINCLUDE:= -I x86_frontend -I iterator

OCAMLLIB= nums.cma

#OCT_INCLUDE= $(shell oct-config --mlflags | sed 's/_iag//')
OCT_INCLUDE= $(shell oct-config --mlflags)

ifneq ($(or $(debug),$(DEBUG)),)
        OCAMLC += -g
endif


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
	iterator/asynchronousAttacker.ml\
	iterator/memAD.ml\
	iterator/iterator.ml\
	iterator/architectureAD.ml\
	config.ml


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
	$(OCAMLC) $(OCAMLINCLUDE) $(OCAMLLIB) str.cma $(OCT_INCLUDE) -o $@ $+

clean:
	rm -f depend cachecow */*.cmo */*.cmi */*~ *.cmo *.cmi *~ *.annot */*.annot */*.html */*.css

depend: 
	$(OCAMLDEP) $(OCAMLINCLUDE) iterator/*.ml iterator/*.mli x86_frontend/*.ml x86_frontend/*.mli > depend

dep:
	rm -f depend
	$(MAKE) depend

include depend

doc:
	-ocamldoc -html -colorize-code  -d Documentation/ $(OCAMLINCLUDE) $(ML_FILES)



.PHONY: all clean dep
