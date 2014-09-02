PREPROCESSOR= camlp4o pa_macro.cmo

EXECUTABLE= cacheaudit 

ifneq ($(or $(opt),$(OPT)),)
	OCAMLC = ocamlopt.opt
	OCAMLLIB= $(OCAMLLIB_STD:.cma=.cmxa)
	CMO_FILES= $(ML_FILES:%.ml=%.cmx)
	DEP_FLAGS= -native
else
	OCAMLC = ocamlc.opt
	OCAMLLIB= $(OCAMLLIB_STD)
	CMO_FILES= $(ML_FILES:%.ml=%.cmo)
endif


OCAMLC += -dtypes -pp "${PREPROCESSOR}"
OCAMLDEP= ocamldep -pp "${PREPROCESSOR}" $(DEP_FLAGS)
#OCAMLC += -dtypes
#OCAMLDEP= ocamldep $(DEP_FLAGS)

OCAMLYACC= ocamlyacc -v
OCAMLLEX= ocamllex

OCAMLINCLUDE:= -I . -I x86_frontend -I iterator -I abstract_domains -I abstract_domains/cache -I abstract_domains/numeric
OCAMLLIB_STD= nums.cma str.cma

ifneq ($(or $(debug),$(DEBUG)),)
        OCAMLC += -g
endif

ML_FILES := \
	logger.ml\
	x86_frontend/asmUtil.ml \
	x86_frontend/x86Util.ml \
	x86_frontend/x86Types.ml \
	x86_frontend/x86Print.ml \
	x86_frontend/x86Parse.ml \
	x86_frontend/execInterfaces.ml \
	x86_frontend/elf.ml \
	x86_frontend/x86Headers.ml \
	iterator/cfg.ml\
  iterator/abstrInstr.ml\
  abstract_domains/AD.ml\
  abstract_domains/stackAD.ml\
	abstract_domains/numeric/numAD.ml\
	abstract_domains/numeric/valAD.ml\
	abstract_domains/utils.ml\
	abstract_domains/cache/ageAD.ml\
	abstract_domains/flagAD.ml\
	abstract_domains/cache/traceAD.ml\
	abstract_domains/cache/cacheAD.ml\
	abstract_domains/cache/asynchronousAttacker.ml\
	abstract_domains/cache/accessAD.ml\
	abstract_domains/memAD.ml\
	iterator/iterator.ml\
	abstract_domains/architectureAD.ml\
	config.ml

all: $(EXECUTABLE)

%.ml: %.mll
	$(OCAMLLEX) $*.mll

%.output %.ml %.mli: %.mly
	$(OCAMLYACC) $*.mly

%.cmo: %.ml %.cmi
	$(OCAMLC) $(OCAMLINCLUDE) -c $*.ml

%.cmi: %.mli
	$(OCAMLC) $(OCAMLINCLUDE) -c $*.mli

%.cmo: %.ml
	$(OCAMLC) $(OCAMLINCLUDE) -c $*.ml

%.cmx: %.ml
	$(OCAMLC) $(OCAMLINCLUDE) -c $*.ml

$(EXECUTABLE): $(CMO_FILES) $(addsuffix .ml, $(EXECUTABLE))
	$(OCAMLC) $(OCAMLINCLUDE) $(OCAMLLIB) -o $@ $+

clean: 
	rm -f depend $(EXECUTABLE) */*/*.cmo */*/*.cmx */*/*.cmi */*/*~ */*/*.annot */*.cmo */*.cmx */*.cmi */*~ */*.annot *.cmo *.cmx *.cmi *~ *.annot */*.html */*.css */*.o */*/*.o

depend: 
	$(OCAMLDEP) $(OCAMLINCLUDE) iterator/*.ml iterator/*.mli x86_frontend/*.ml x86_frontend/*.mli *.mli abstract_domains/*.ml abstract_domains/*.mli abstract_domains/*/*.ml abstract_domains/*/*.mli *.ml *.mli> depend

ifneq ($(MAKECMDGOALS), clean)
   -include depend
endif

MLI_DOC_FILES = *.mli iterator/*.mli abstract_domains/*.mli abstract_domains/numeric/*.mli abstract_domains/cache/*.mli x86_frontend/*.mli

#ml files without mli that should be in the doc
ML_DOC_FILES = abstract_domains/AD.ml iterator/abstrInstr.ml abstract_domains/numeric/numAD.ml

doc: all
	-ocamldoc -pp "${PREPROCESSOR}" -html -colorize-code -I /opt/local/lib/ocaml  -d documentation/ \
	$(OCAMLINCLUDE) -t "CacheAudit: Static Analysis of Cache Side-Channels" \
	$(MLI_DOC_FILES) $(ML_DOC_FILES)

test:	$(EXECUTABLE)
	cd tests; ./run.sh;

help:
	@echo "usage:"
	@echo "  - make         : Compile with ocamlc compiler without debug."
	@echo "  - make opt=1   : Compile with the optimized native code compiler."
	@echo "  - make debug=1 : Compile with debug flags."
	@echo "  - make doc     : Compile documentation."
	@echo "  - make test    : Run tests."
	@echo "  - make help    : Show this dialog."

.PHONY: all clean test help
