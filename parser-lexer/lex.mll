{
	open Printf
	open Parser
	open Char
}

let size = ['q' 'l' 'w' 'b']
let alpha =  ['a'-'z''A'-'Z']
let number = ['0'-'9' '-']+
let alphanum = (alpha|number|'_'|':')*
let ws = [' ' '\t']
let eol = '\n'
let punct = ['"' '.' '@' ',' '/' '(' ')']

rule line = parse
	| ws|eol		{ line lexbuf }
	| ".section\t" ("." (alphanum|punct)* as sec)	{ SECTION sec }
	| ".text" as sec	{ SECTION sec }
	| (alphanum|punct)* as lbl ":"	{ LABEL lbl }
	| "." (alphanum|ws|punct)* as str	{ DOT str }
	(* Operators *)
	| "leave"	{ LEAVE }
	| "ret"		{ RET }
	| "sub" (size as s)	{ SUB(escaped s) }
	| "push" (size as s)	{ PUSH(escaped s) }
	| "mov" (alpha* size+ as s)	{ MOV s }
	| "and" (size as s)	{ AND(escaped s) }
	| "xor" (size as s)	{ XOR(escaped s) }
	| "j" (alpha+ as s)	{ JMP s }
	| alpha+ as name	{ INSTR name }
	(* Operands *)
	| (number? as offset) '('? "%" (alphanum as str) ')'?	{ REGISTER (str, offset) }
	| "$" (number as str)	{ NUMBER(int_of_string str) }
	| ("$"|"_") alphanum as str	{ ADDRESS str }
	| alphanum '(' (punct|alphanum|'%')* ')' as str { ADDRESS str } (* Operands like FT0(,%eax,4) in aes.s *)
	| _		{ line lexbuf }
	| eof		{ EOF }
