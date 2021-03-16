#Added for '==' (dbleq) and 'for' loop

###lexer.mll:
Add to rule get_token: 
```Ocaml  line 67-68
    | "for" {FOR}
    | "==" {DBLEQ}
```

###LMJ.mli
Add to binop
```Ocaml line 27
	| OpDbleq
```

Add to instruction
```Ocaml line 35
	| IFor of expression * instruction
```

###lmj2mj.ml
Add to translate_instruction
```Ocaml line 19
	| LMJ.IFor (c, i) -> MJ.For (translate_expression c, translate_instruction i)
```

###MJ.mli
Add to binop
```Ocaml line 25
	| OpDbleq
```

Add to instruction
```Ocaml line 33
	| IFor of expression * instruction
```

###parser.mly
Add tokens
```Ocaml line 19-20
%token DBLEQ
%token FOR
```

Add an association rule
```Ocaml line 25
	%left DBLEQ
```

Add to instruction
```Ocaml line 101
	| FOR LPAREN ex = expression RPAREN i = instruction { IFor(ex, i) }
```

Add to raw_expression
```Ocaml line 131
	| e1 = expression DBLEQ e2 = expression { EBinOp(OpDbleq, e1, e2) }
```

###printMJ.ml
Add to binop
```Ocaml line 16
	| OpDbleq -> "=="
```

Add to let rec expr0:
```Ocaml line 53-57
	and expr7 () = function
	  | EBinOp (OpDbleq as op, e1, e2) -> sprintf "%a %s %a" expr7 e1 (binop op) expr7 e2
	  | e -> expr6 () e

	and expr () e = expr7 () e
```

Add to instr()
```OCaml line 74
  | IFor (c, i) ->
      sprintf "for (%a) %a"
        expr c
        instr i
```




# DO WHILE loop
###lexer.mll:
Add to rule get_token: 
```Ocaml  line 69
    | "do" {DO}
```

###LMJ.mli
Add to instruction
```Ocaml line 36
	| IDowhile of expression * instruction
```

###lmj2mj.ml
Add to translate_instruction
```Ocaml line 20
	| LMJ.IDowhile (c, i) -> MJ.IDowhile (translate_expression c, translate_instruction i)
```
###MJ.mli
Add to instruction
```Ocaml line 33
	| IDowhile of expression * instruction
```

###parser.mly
Add token
```Ocaml line 21
%token DO
```

Add to instruction
```Ocaml line 103
	| DO i = instruction WHILE LPAREN ex = expression RPAREN SEMICOLON 
		{ IDowhile(ex, i) }
```

###printMJ.ml
Add to instr()
```OCaml line 74
  | IDowhile (c, i) ->
      sprintf "do (%a) while %a"
        instr i
        expr c
```











