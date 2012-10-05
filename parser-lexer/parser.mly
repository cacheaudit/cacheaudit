%{
open Printf
open String
%}

%token <string> MOV SUB PUSH AND XOR JMP
%token LEAVE RET
%token <string> INSTR LABEL SECTION ADDRESS DOT
%token <string * string> REGISTER
%token <int> NUMBER

%token EOF

%start main
%type <unit> main
%%
main:
	| { }
	| instr main	{ }
	| main SECTION instr_seq 	{ printf "Section %s\n" $2; } 
;
instr_seq:
	| {}
	| instr instr_seq	{ }
;
instr:
	| LABEL		{ printf "Label %s\n" $1 }
	| DOT		{ printf "Directive %s\n" $1 }
	| MOV reg_seq	{ printf "MOV[%s]\t%s\n" $1 $2 }
	| SUB reg_seq	{ printf "SUB[%s]\t%s\n" $1 $2 }
	| AND reg_seq	{ printf "AND[%s]\t%s\n" $1 $2 }
	| XOR reg_seq	{ printf "XOR[%s]\t%s\n" $1 $2 }
	| PUSH reg_seq	{ printf "PUSH[%s]\t%s\n" $1 $2 }
	| JMP reg_seq	{ printf "J[%s] %s\n" $1 $2 }
	| LEAVE		{ printf "LEAVE\n" }
	| RET		{ printf "RET\n" }
	| INSTR reg_seq	{ printf "%s %s\n" (uppercase $1) $2 }
	| EOF		{ exit 0 }
;

reg_seq:
	{ "\n" }
	| reg { "\t"^$1 }
	| reg reg { "\t"^$1^"\t"^$2 }
;

reg:
	| REGISTER	{ "REG "^(fst $1)^" "^(snd $1) }
	| NUMBER	{ "NUM "^(string_of_int $1) }
	| ADDRESS	{ "ADDR "^$1 }
	| DOT		{ "LABEL "^$1 }
;
%%
