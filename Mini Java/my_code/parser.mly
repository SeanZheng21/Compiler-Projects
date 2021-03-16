%{
  open LMJ
  let swap = List.map (fun (x, y) -> (y, x))
%}

%token <int32> INT_CONST
%token <bool> BOOL_CONST
%token INTEGER BOOLEAN
%token <string Location.t> IDENT
%token CLASS PUBLIC STATIC VOID MAIN STRING EXTENDS RETURN
%token PLUS MINUS TIMES NOT LT AND
%token COMMA SEMICOLON
%token ASSIGN
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token THIS NEW DOT LENGTH
%token SYSO
%token IF ELSE WHILE
%token EOF
%token DBLEQ
%token FOR
%token DO

%left AND
%nonassoc LT
%left PLUS 
%left MINUS
%left TIMES
%left DBLEQ
%nonassoc NOT
%nonassoc DOT
%nonassoc LBRACKET

%start program

%type <LMJ.program> program

%%

program:
| m = main_class d = defs EOF
   {
     let c, a, i = m in
     {
       name = c;
       defs = d;
       main_args = a;
       main = i
     }
   }

main_class:
| CLASS c = IDENT
   LBRACE
   PUBLIC STATIC VOID MAIN LPAREN STRING LBRACKET RBRACKET a = IDENT RPAREN
   LBRACE
   i = instruction
   RBRACE
   RBRACE
   { (c, a, i) }

clas:
| CLASS c = IDENT i = option(preceded(EXTENDS, IDENT))
   LBRACE att = list(pair(typ, terminated(IDENT, SEMICOLON)))
   lm = list(metho) RBRACE
   {    c, {
       extends = i;
       attributes = swap att;
       methods = lm;
     }
   }

typ:
| id = IDENT { Typ id }
| INTEGER { TypInt }
| INTEGER LBRACKET RBRACKET { TypIntArray }
| BOOLEAN { TypBool }


metho:
| PUBLIC t = typ i = IDENT LPAREN
   f = separated_list(COMMA, pair(typ, IDENT)) RPAREN LBRACE
       ds = declarations_and_statements
       RETURN r = expression SEMICOLON
   RBRACE
   {
     let d, s = fst ds, snd ds in
     i, {
       formals = swap f;
       result  = t;
       locals  = d;
       body    = s;
       return  = r;
    }
   }

defs:
| c = list(clas) { c }

instruction:
| SYSO LPAREN e = expression RPAREN SEMICOLON { ISyso e }
| WHILE LPAREN ex = expression RPAREN i = instruction { IWhile(ex, i) }
| FOR LPAREN ex = expression RPAREN i = instruction { IFor(ex, i) }
| DO i = instruction WHILE LPAREN ex = expression RPAREN SEMICOLON { IDowhile(ex, i) }
| IF LPAREN e = expression RPAREN i = instruction ELSE ii = instruction
    { IIf(e, i, ii) }
| i = IDENT ASSIGN e = expression SEMICOLON { ISetVar(i, e) }
| i = IDENT LBRACKET ex = expression RBRACKET ASSIGN e = expression SEMICOLON
    { IArraySet(i, ex, e) }
| b = block { b }

block:
| LBRACE i = list(instruction) RBRACE { IBlock i }

expression:
|  e = raw_expression
   { Location.make $startpos $endpos e }
| LPAREN e = expression RPAREN
   { e }

raw_expression:
| i = INT_CONST { EConst (ConstInt i) }
| THIS  { EThis }
| b = BOOL_CONST { EConst (ConstBool b) }
| NOT e = expression  {EUnOp(UOpNot, e)}
| id = IDENT { EGetVar id }
| NEW INTEGER LBRACKET e = expression RBRACKET { EArrayAlloc e }
| NEW id = IDENT LPAREN RPAREN { EObjectAlloc id }
| e1 = expression AND e2 = expression { EBinOp(OpAnd, e1, e2) }
| e1 = expression PLUS e2 = expression { EBinOp(OpAdd, e1, e2) }
| e1 = expression MINUS e2 = expression { EBinOp(OpSub, e1, e2) }
| e1 = expression TIMES e2 = expression { EBinOp(OpMul, e1, e2) }
| e1 = expression LT e2 = expression { EBinOp(OpLt, e1, e2) }
| e1 = expression DBLEQ e2 = expression { EBinOp(OpDbleq, e1, e2) }
| ex = expression DOT i = IDENT LPAREN 
    lst = separated_list(COMMA, expression) 
    RPAREN { EMethodCall (ex, i, lst) }
| e = expression LBRACKET e2 = expression RBRACKET { EArrayGet (e, e2) }
| e = expression DOT LENGTH { EArrayLength e }



declarations_and_statements:
| t = typ id = IDENT SEMICOLON 
  r = declarations_and_statements
    { let d, s = r in ((id, t)::d, s) }
| s = list(instruction)
   { ([], s) }

