IDIR =estream/include/
CC=gcc
CFLAGS=-I$(IDIR) -g -m32 -fno-stack-protector

LIBS=

TESTSRC=test_estream.c
ECRYPTH=ecrypt-sync.h

CIPHERS_C := HC-128/hc-128.c Rabbit/rabbit.c Salsa20/salsa20.c Sosemanuk/sosemanuk.c
EXECUTABLES = $(CIPHERS_C:%.c=%)

all: $(EXECUTABLES)
	@echo CacheAudit Warning: Analysis may not work with newly compiled files. Please refer to ../../../README

.SECONDEXPANSION:
%: %.c $(TESTSRC) $$(dir $$@)*.h $(IDIR)*.h
	$(CC) $(TESTSRC) $< -o $@ $(CFLAGS) -DECRLIB=\"$(dir $@)$(ECRYPTH)\"


.PHONY: clean

clean:
	rm -f *~ $(EXECUTABLES)
