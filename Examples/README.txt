Quick guide for generating examples for Mac OSX.


Generating executable from C code:
**********************************

gcc -m32 -o example example.c

Note: -m32 option is required because CacheCow is restricted to 32-bit binaries.



Generating assembly from C code:
********************************

gcc -m32 -S -o example.s example.c

Note: 
- if -o filename is omitted, output is written to example.s
- syntax follows AT&T standard, i.e. source before destination. Observe that CacheCow uses Intel syntax, i.e. destination before source!



Generating executable from assembly:
************************************

gcc -m32 -o example example.s

Note: push %ebp in case code terminates with leave to avoid pop on empty stack

Generating CacheCow config-file from executables:
*************************************************

otool -v -t example

Note: 
- prints the disassembled .text section of example
- take the address of _main minus 0x1000 as START offset in the config file named example.config



