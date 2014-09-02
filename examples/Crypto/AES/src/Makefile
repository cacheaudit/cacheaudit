CC=gcc
CFLAGS=-g -m32 -fno-stack-protector

LIBS=

TESTSRC = test_aes.c
HEADERS = config.h


CIPHERS_C = aes.c 
BASENAME = $(patsubst %.c,%,$(CIPHERS_C))
VARIANTS = rom preloading
KEYSIZES = 128 192 256
EXECUTABLES = $(foreach s, $(KEYSIZES), $(foreach v,$(VARIANTS), $(BASENAME)-$(s)-$(v)))

all: $(EXECUTABLES)
	@echo CacheAudit Warning: Analysis may not work with newly compiled files. Please refer to ../../../README

$(BASENAME)-%: $(TESTSRC) $(HEADERS) $(CIPHERS_C)
	$(CC) $(TESTSRC) $(CIPHERS_C) $(CFLAGS) -o $@ \
	-DKEYSIZE=$(word 2, $(subst -, ,$@)) \
	-D$(shell echo $(word 3, $(subst -, ,$@)) | tr a-z A-Z)


.PHONY: clean

clean:
	rm -f *~ $(EXECUTABLES)
