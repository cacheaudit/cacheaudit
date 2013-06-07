PREPROCESSOR= camlp4o pa_macro.cmo

ifneq ($(or $(oct),$(OCT)),)
        PREPROCESSOR += -DINCLUDEOCT
endif

#OCT_INCLUDE= $(shell oct-config --mlflags | sed 's/_iag//')

ifneq ($(or $(opt),$(OPT)),)
	OCAMLC = ocamlopt.opt
	OCT_INCLUDE= $(shell oct-config --mlflags --with-ocamlopt)
	OCAMLLIB= $(OCAMLLIB_STD:.cma=.cmxa)
	CMO_FILES= $(ML_FILES:%.ml=%.cmx)
	DEP_FLAGS= -native
else
	OCAMLC = ocamlc.opt
	OCT_INCLUDE= $(shell oct-config --mlflags)
	OCAMLLIB= $(OCAMLLIB_STD)
	CMO_FILES= $(ML_FILES:%.ml=%.cmo)
endif

OCAMLC += -dtypes -pp "${PREPROCESSOR}"
OCAMLDEP= ocamldep -pp "${PREPROCESSOR}" $(DEP_FLAGS)
#OCAMLC += -dtypes
#OCAMLDEP= ocamldep $(DEP_FLAGS)

OCAMLYACC= ocamlyacc -v
OCAMLLEX= ocamllex

OCAMLINCLUDE:= -I x86_frontend -I iterator
OCAMLLIB_STD= nums.cma str.cma

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
	iterator/traceAD.ml\
	iterator/cacheAD.ml\
	iterator/relCacheAD.ml\
	iterator/asynchronousAttacker.ml\
	iterator/memAD.ml\
	iterator/iterator.ml\
	iterator/architectureAD.ml\
	config.ml

ifneq ($(MAKECMDGOALS),clean)
    -include depend
endif

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

%.cmx: %.ml
	$(OCAMLC) $(OCAMLINCLUDE) $(OCT_INCLUDE) -c $*.ml

cachecow: $(CMO_FILES) cachecow.ml
	$(OCAMLC) $(OCAMLINCLUDE) $(OCAMLLIB) $(OCT_INCLUDE) -o $@ $+

clean: 
	rm -f depend cachecow */*.cmo */*.cmx */*.cmi */*~ *.cmo *.cmx *.cmi *~ *.annot */*.annot */*.html */*.css output_non_rel.latte output_final_state output_rel.latte

depend: 
	$(OCAMLDEP) $(OCAMLINCLUDE) iterator/*.ml iterator/*.mli x86_frontend/*.ml x86_frontend/*.mli > depend

doc:
	-ocamldoc -html -colorize-code  -d Documentation/ $(OCAMLINCLUDE) $(ML_FILES)

help:
	@echo "usage:"
	@echo "  - make         : Compile with ocamlc compiler without debug and without octagon library."
	@echo "  - make opt=1   : Compile with the optimized native code compiler."
	@echo "  - make debug=1 : Compile with debug flags."
	@echo "  - make oct=1   : Compile including the octagon abstract domain library."
	@echo "  - make doc     : Compile documentation."
	@echo "  - make help    : Show this dialog."

.PHONY: all clean dep help
