CC=gcc
CFLAGS=-g -m32 -fno-stack-protector

LIBS=


FILES_C = bubble.c insertion.c selection.c
BASENAMES = $(patsubst %.c,%,$(FILES_C))
ARRSIZES = 8 16 32 64
EXECUTABLES = $(foreach s, $(ARRSIZES), \
			$(foreach f, $(BASENAMES), \
			sort-$(f)-$(s)))

all: $(EXECUTABLES)

sort-%: $(FILES_C)
	$(CC) $(word 2, $(subst -, ,$@)).c $(CFLAGS) -o $@ \
	-DARRAY_SIZE=$(word 3, $(subst -, ,$@))


.PHONY: clean

clean:
	rm -f *~ $(EXECUTABLES)
