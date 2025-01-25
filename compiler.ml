(*! ocamlc -o compiler.exe compiler.ml      puis      ./compiler.exe *)

type type_symbole = INT | FUNCTION ;;
type symbol = {
  name: string;
  typ: type_symbole;
  pos: int;
} ;;

type tokType = TOK_EOF | IDENT of string | CONST of int | PLUS | MINUS | MUL | DIV | NOT | AND | OR | LT | GT | LEQ 
| GEQ | EQU | DIF | ASSIGN | COMMA | SEMICOLON | LPAREN | RPAREN | LBRACE | RBRACE | LBRACKET | RBRACKET | AMPERSAND 
| PERCENT | IF | ELSE | WHILE | FOR | DO | BREAK | CONTINUE | INT | RETURN | SEND | RECV | DEBUG | INIT ;;
type token = { tokType : tokType; ligne : int; } ;;
type nodeType = ND_NOT | ND_UNARY_PLUS | ND_UNARY_MINUS | ND_CONST of int | ND_BINARY_PLUS | ND_BINARY_MINUS 
| ND_MUL| ND_DIV| ND_AND| ND_OR| ND_LT| ND_GT| ND_LEQ| ND_GEQ| ND_EQU| ND_DIF| ND_ASSIGN| ND_MODULO | ND_DEBUG 
| ND_BLOC | ND_DECL of string | ND_DROP | ND_REF of (string*(int ref)) | ND_COND | ND_SEQ | ND_LOOP | ND_BREAK 
| ND_CONTINUE | ND_ANCOR | ND_FONC of (string*(int ref)) | ND_APPEL | ND_RET | ND_IND | ND_ADR | ND_SEND | ND_RECV;;

type operation = 
| Inconnu
| Packet of opPackage 
and opPackage =
{
  prio:int;
  assoc:int;
  nodeType:nodeType
} ;;
type code =
| CodeInconnu
| Code of string ;;

type node = {
  nodeType: nodeType;
  childs: node list
}

let creerNoeud nodeType childs = {
  nodeType: nodeType;
  childs: node list
} ;;

exception Syntax_Error of string

(*GLOBALS*)
let linenum = ref 0 ;;
let filestring = ref "" ;;
let pointer = ref 0 ;;
let ct = ref {tokType=INIT; ligne=(-1)} ;;
let lt = ref {tokType=INIT; ligne=(-1)} ;;
let nvar = ref 0 ;;
let update_nvar () =
  let ret = !nvar in
  nvar := !nvar + 1;
  ret ;;

let nlabel = ref 0 ;;
let label_boucle = ref 0 ;;
(*ENDGLOBALS*)

(*SYMBOLTABLE*)
module SymbTable = struct
  let pile : ((string, symbol) Hashtbl.t list ref)= ref [] ;;

  let begin_bloc () = pile := (Hashtbl.create 123456) :: !pile ;;
  let end_bloc () = pile := List.tl !pile ;;
  let declare (name:string) (t:type_symbole) =
    match Hashtbl.find_opt (List.hd !pile) name with
    | Some value -> raise (Failure ("The symbol <"^name^"> already exists in this scope"));
    | None -> ();
    let pos = update_nvar () in (*UPDATE MEME QUAND C4EST FONC, CHELOU MAIS DEVRAIT ALLER*)
    let symb = {name=name; typ=t; pos=pos} in
    try
      Hashtbl.add (List.hd !pile) name symb
    with
    | Failure s -> Printf.printf "Problème accès head of pile dans declare : %s (name: %s)" s name; raise (Failure "hd");;
  let find name typ =
    let rec find_bis name pile =
      match pile with
      [] -> raise (Syntax_Error ("Variable "^name^" not found in symbol table"))
      | h::t -> try
                  let candidat = Hashtbl.find h name in
                  if (candidat.typ == typ) then
                    candidat
                  else
                    find_bis name (t)
                with
                  Not_found -> find_bis name (t) in
    find_bis name !pile
              ;;
end ;;
(*ENDSYMBOLTABLE*)

let string_of_tok tok = match tok with
| TOK_EOF -> "eof"
| IDENT id -> Printf.sprintf "ident(%s)" id
| CONST n -> Printf.sprintf "const(%d)" n
| PLUS -> "+"
| MINUS -> "-"
| MUL -> "*"
| DIV -> "/"
| NOT -> "!"
| AND -> "&&"
| OR -> "||"
| LT -> "<"
| GT -> ">"
| LEQ -> "<="
| GEQ -> ">="
| EQU -> "=="
| DIF -> "!="
| ASSIGN -> "="
| COMMA -> ","
| SEMICOLON -> ";"
| LPAREN -> "("
| RPAREN -> ")"
| LBRACE -> "{"
| RBRACE -> "}"
| LBRACKET -> "["
| RBRACKET -> "]"
| AMPERSAND -> "&"
| PERCENT -> "%"
| IF -> "if"
| ELSE -> "else"
| WHILE -> "while"
| FOR -> "for"
| DO -> "do"
| BREAK -> "break"
| CONTINUE -> "continue"
| INT -> "int"
| RETURN -> "return"
| SEND -> "send"
| RECV -> "recv"
| DEBUG -> "debug"
| _ -> raise (Syntax_Error "String for this token was not defined") ;;

let digitGrabber () =
  let rec digitGrabberHelper str =
    try
      let c = (!filestring).[!pointer] in
      match c with
      '0'..'9' -> pointer:=!pointer+1; (digitGrabberHelper (str^(String.make 1 c))) 
      | _ -> pointer:=(!pointer-1); str 
    with Invalid_argument "index out of bounds" -> str;
    in
  digitGrabberHelper "" ;;

  let wordGrabber () =
    let rec wordGrabberHelper str =
      try
        let c = (!filestring).[!pointer] in
        match c with
        '0'..'9' | 'a'..'z' | 'A'..'Z' | '_' -> pointer:=!pointer+1; (wordGrabberHelper (str^(String.make 1 c))) 
        | _ -> pointer:=(!pointer-1); str 
      with Invalid_argument "index out of bounds" -> str;
      in
      wordGrabberHelper ""
  ;;

let next () = 
  (* Printf.printf "next : %s " (string_of_int !pointer) ; *)
  let flag = ref true in
  let avance t = (flag:=false); lt:=!ct ; ct:=(t) ; () in
  let twoCharsMaybe c tokType1 tokType2 = 
    (*todo : mettre un try catch de fin du string*)
    try
      let nextTempoChar = (!filestring).[!pointer + 1] in 
        if (nextTempoChar == c) then
        (
          (pointer := ((!pointer)+1));
          (avance {tokType=tokType1; ligne=(!linenum)});
        )
        else
          avance {tokType=tokType2; ligne=(!linenum)}
      with Invalid_argument "index out of bounds" -> avance {tokType=tokType2; ligne=(!linenum)};
        in
  let treat_char c =
    (match c with
      '\n' -> linenum:=((!linenum)+1);
      | '+' -> avance {tokType=PLUS; ligne=(!linenum)}
      | '-' -> avance {tokType=MINUS; ligne=(!linenum)}
      | '/' -> begin
        try
          let nextTempoChar = (!filestring).[!pointer + 1] in
          if nextTempoChar = '*' then
            begin
              pointer := !pointer + 2;
              let rec skip_comment () =
                try
                  let c1 = (!filestring).[!pointer] in
                  let c2 = (!filestring).[!pointer + 1] in
                  if c1 = '*' && c2 = '/' then
                    pointer := !pointer + 2
                  else
                    (pointer := !pointer + 1; skip_comment ())
                with Invalid_argument "index out of bounds" -> ()
              in
              skip_comment ()
            end
          else
            avance {tokType=DIV; ligne=(!linenum)}
        with Invalid_argument "index out of bounds" -> avance {tokType=DIV; ligne=(!linenum)}
        end
      | '*' -> avance {tokType=MUL; ligne=(!linenum)}
      | '%' -> avance {tokType=PERCENT; ligne=(!linenum)}
      | '(' -> avance {tokType=LPAREN; ligne=(!linenum)}
      | ')' -> avance {tokType=RPAREN; ligne=(!linenum)}
      | '[' -> avance {tokType=LBRACKET; ligne=(!linenum)}
      | ']' -> avance {tokType=RBRACKET; ligne=(!linenum)}
      | '{' -> avance {tokType=LBRACE; ligne=(!linenum)}
      | '}' -> avance {tokType=RBRACE; ligne=(!linenum)}
      | ',' -> avance {tokType=COMMA; ligne=(!linenum)}
      | ';' -> avance {tokType=SEMICOLON; ligne=(!linenum)}
      | '&' -> twoCharsMaybe '&' AND AMPERSAND 
      | '!' -> twoCharsMaybe '=' DIF NOT
      | '=' -> twoCharsMaybe '=' EQU ASSIGN
      | '>' -> twoCharsMaybe '=' GEQ GT
      | '<' -> twoCharsMaybe '=' LEQ LT
      |'|' -> (try 
                (let nextTempoChar = (!filestring).[!pointer + 1] in 
                  if (nextTempoChar == '|') then
                    ((pointer := ((!pointer)+1));
                      avance {tokType=OR; ligne=(!linenum)})
                  else 
                    (raise (Syntax_Error ("Erreur de syntaxe en ligne L."^(string_of_int !linenum)^" : "^(String.make 1 nextTempoChar)^" lu alors que '|' était attendu")))
                )
              with 
              Invalid_argument "index out of bounds" -> (raise (Syntax_Error ("Erreur de syntaxe en ligne L."^(string_of_int !linenum)^" : '|' était attendu"))) ;)
      | '0'..'9' -> avance {tokType=(CONST (int_of_string ((digitGrabber ())))); ligne=(!linenum)}
      | 'a'..'z' | 'A'..'Z' | '_' -> (match wordGrabber () with 
                                    "for"->avance {tokType=(FOR); ligne=(!linenum)}
                                    |"while" -> avance {tokType=(WHILE); ligne=(!linenum)}
                                    |"if" -> avance {tokType=(IF); ligne=(!linenum)}
                                    |"else" -> avance {tokType=(ELSE); ligne=(!linenum)}
                                    |"do" -> avance {tokType=(DO); ligne=(!linenum)}
                                    |"int" -> avance {tokType=(INT); ligne=(!linenum)}
                                    |"break" -> avance {tokType=(BREAK); ligne=(!linenum)}
                                    |"continue" -> avance {tokType=(CONTINUE); ligne=(!linenum)}
                                    |"return" -> avance {tokType=(RETURN); ligne=(!linenum)}
                                    |"send" -> avance {tokType=(SEND); ligne=(!linenum)}
                                    |"recv" -> avance {tokType=(RECV); ligne=(!linenum)}
                                    |"debug" -> avance {tokType=(DEBUG); ligne=(!linenum)}
                                    |w->avance {tokType=(IDENT (w)); ligne=(!linenum)})
      (*/!\ const si si le char suivant est space OU BIEN \n !!!!!!*)
      | _ -> ();) (*todo end of file, fait askip*)
    in 
  while (!flag); do
    (try
      let newchar = (!filestring).[!pointer] in
        treat_char newchar
    with Invalid_argument "index out of bounds" ->
      avance {tokType=TOK_EOF; ligne=(!linenum)} ) ;
    pointer:=((!pointer)+1)
  done; () ;;


let accept typ = 
  if typ != !ct.tokType then
    raise (Syntax_Error ("Erreur de syntaxe en ligne L."^(string_of_int !ct.ligne)^" : "^(string_of_tok !ct.tokType)^" lu alors que "^(string_of_tok typ)^" était attendu"))
  else
    next () ;;

let check typ = if (!ct.tokType != typ) then
                    (false)
                else
                    (next () ; true) ;;
let analex filename = 
  filestring := (let chan = (open_in filename) in (*todo*)
  let read_channel_to_string ic =
    let rec read_lines acc =
      try
        let line = input_line ic in
        read_lines (line :: acc)  (* Add each line to the accumulator list *)
      with
      | End_of_file ->
        String.concat "\n" (List.rev acc)  (* Reverse the list and join it into a single string *)
    in
    read_lines [] in
  read_channel_to_string chan) ;
  next ();
  (* let rec f () =
    let k = read_line () in
    match k with
    "q" -> ()
    | _ -> next (); Printf.printf "%s" (string_of_tok ((!ct).tokType)) ; f (); in
  f () *)
  (*ct, lt init*)
  (* let ct = {tokType = ...; ligne = -1 } in
  let lt = ref {tokType = ...; ligne = -1 } in *) ;;


(*FONCTION TEMPORAIRE POUR LE MOMENT, ELLE EST VOUÉ A DISPARAITRE DANS LES MÉANDRES DE a s p e etc..*)
let nodeType_of_tokType_prefix tokType =
  match tokType with
  PLUS -> ND_UNARY_PLUS
  | MINUS -> ND_UNARY_MINUS 
  | NOT -> ND_NOT
  | CONST a -> ND_CONST a
  | _ ->   Printf.printf "| ERREUR [%s] |" (string_of_tok tokType) ; ND_CONST (-69)

let operation_of_tokType_opBin tokType =
  let creerOp prio1 assoc1 nodeType1:operation = Packet { prio=prio1;
    assoc=assoc1;
    nodeType=nodeType1
  } in 
  match tokType with
  | PLUS -> creerOp 6 1 ND_BINARY_PLUS
  | MINUS -> creerOp 6 1 ND_BINARY_MINUS
  | MUL -> creerOp 7 1 ND_MUL
  | DIV -> creerOp 7 1 ND_DIV
  | AND -> creerOp 3 1 ND_AND
  | OR -> creerOp 2 1 ND_OR
  | LT -> creerOp 5 1 ND_LT
  | GT -> creerOp 5 1 ND_GT
  | LEQ -> creerOp 5 1 ND_LEQ
  | GEQ -> creerOp 5 1 ND_GEQ
  | EQU -> creerOp 4 1 ND_EQU
  | DIF -> creerOp 4 1 ND_DIF
  | ASSIGN -> creerOp 1 0 ND_ASSIGN
  | PERCENT -> creerOp 7 1 ND_MODULO
  | _ -> Inconnu

let string_of_node typeNode = match typeNode with
  ND_UNARY_PLUS -> "plus unaire"
  | ND_UNARY_MINUS -> "moins unaire"
  | ND_NOT -> "not"
  | ND_CONST a -> "const "^(string_of_int a)
  | ND_BINARY_PLUS -> "plus binaire"
  | ND_BLOC -> "bloc"
  | ND_DROP -> "drop"
  | ND_DECL a -> "decl "^a
  | ND_RET -> "ret"
  | ND_REF (nom, pos) -> "REF("^nom^"), pos("^(string_of_int !pos)^")"
  | ND_FONC (nom, nbvarloc) -> "fun("^nom^"), nb var loc("^(string_of_int !nbvarloc)^")"
  | ND_LOOP -> "loop"
  | ND_ANCOR -> "ancre"
  | ND_BREAK -> "break"
  | ND_CONTINUE -> "continue"
  | ND_DEBUG -> "debug"
  | ND_SEQ -> "seq"
  | ND_COND -> "cond"
  | ND_ASSIGN -> "assign"
  | ND_LT -> "less than"
  | _ -> "Je sais pas faire, voir string of node"
;;

let gencode_of_typeNode typeNode :code = match typeNode with
  ND_BINARY_PLUS -> Code "add\n"
  | ND_BINARY_MINUS -> Code "sub\n"
  | ND_MUL -> Code "mul\n"
  | ND_DIV -> Code "div\n"
  | ND_AND -> Code "and\n"
  | ND_OR -> Code  "or\n"
  | ND_LT -> Code "cmplt\n"
  | ND_GT -> Code "cmpgt\n"
  | ND_LEQ -> Code "cmple\n"
  | ND_GEQ -> Code "cmpge\n"
  | ND_EQU -> Code "cmpeq\n"
  | ND_DIF -> Code "cmpne\n"
  | ND_DEBUG -> Code "dbg\n"
  | ND_DROP -> Code "drop 1\n"
  | ND_MODULO -> Code "mod\n"
  | ND_BLOC -> Code ""
  | _ -> CodeInconnu
;;


let rec printArbre node =
  match List.length node.childs with
  0 -> Printf.printf "node = %s \n"(string_of_node node.nodeType)
  | _ -> Printf.printf "node = %s \n"(string_of_node node.nodeType) ; List.iter printArbre (node.childs)
;;



let rec a () =
  (* Printf.printf "##ATOM##\n"; *)
  match !ct.tokType with
  CONST a ->  next () ; creerNoeud (nodeType_of_tokType_prefix (!lt).tokType) []
  | LPAREN -> next () ; let n = e () in (accept RPAREN ; n)
  | IDENT a -> next () ; creerNoeud (ND_REF (a, ref (-1))) []
  | RECV -> next() ; creerNoeud ND_RECV []
  | _ -> raise (Syntax_Error ("Erreur lors de l'analyse d'une expression (L."^(string_of_int !ct.ligne)^", token de type "^string_of_tok !ct.tokType^")"))
    (* if(check (CONST _)) then
      creerNoeud (nodeType_of_tokType_prefix (!ct).tokType) []
    else if (check LPAREN) then
    e ()
    else
    accept RPAREN *)
and  s () =
  (* Printf.printf "##SUF##\n"; *)
  let atom = a () in
  match !ct.tokType with
  LPAREN ->( next () ; (*appel fonction*)
    let rec boucle (l:node list) : node list=
            (match !ct.tokType with
            | RPAREN -> next(); (l);
            | COMMA -> next(); boucle l ; (*Permet int f(int a,,,,,,,,, int b,,,,,) ; pour desac, faudrait match en post e () juste en dessous*)
            | _ -> boucle ((e ())::l)
            )
    in
    creerNoeud (ND_APPEL) ((atom)::(List.rev (boucle [])))
  )
  | LBRACKET -> (next () ; let r = e () in (accept RBRACKET) ; creerNoeud ND_IND [creerNoeud ND_BINARY_PLUS [atom;r]])
  | _ -> atom
and p () =
  (* Printf.printf "##PREF##\n"; *)
  match !ct.tokType with
    PLUS -> next () ; p ()
    | MINUS -> next () ; creerNoeud (nodeType_of_tokType_prefix MINUS) [p ()] 
    | NOT -> next () ; creerNoeud (nodeType_of_tokType_prefix NOT) [p ()]
    | MUL -> next () ; creerNoeud ND_IND [p ()]
    | AMPERSAND -> next () ; creerNoeud ND_ADR [p ()]
    | _ -> s()

and e () = 
  (* Printf.printf "##EXPR##\n"; *)
  let rec eHelper pMin:node=
    (* let a1 = ref p() in *)
    let rec boucleInterne (a1:node) =
      let op = operation_of_tokType_opBin (!ct.tokType) in
      match op with
      Packet p when p.prio<pMin -> a1
      | Inconnu -> a1
      | Packet p -> (* Printf. printf "opbin %s %d \n" (string_of_tok (!ct).tokType) (!ct).ligne ;*) next () ; let a2 = eHelper (p.prio + p.assoc) in boucleInterne (creerNoeud p.nodeType [a1; a2])
    in
    boucleInterne (p ())
  in
  eHelper (-1) (*pour toujours prendre l'operation qu'on croise*)
and i () = 
  (* Printf.printf "##INSTR##\n"; *)
  match !ct.tokType with
    DEBUG -> next () ; let r = e () in accept SEMICOLON  ; (creerNoeud ND_DEBUG [r])
    | LBRACE -> next(); let rec blocConstruct (r:node) =
                match !ct.tokType with
                RBRACE -> accept RBRACE; r 
                | _ -> blocConstruct (creerNoeud ND_BLOC (r.childs @ [i ()])) in 
                let r = blocConstruct (creerNoeud ND_BLOC []) in
                (* Printf.printf "\n ----  BLOC : " ;
                printArbre r ; Printf.printf ("\n ----  FIN BLOC ") ;  *)
                r
    | INT -> next () ; (*accept IDENT*)
                  let rec skip_etoile () = 
                  (match !ct.tokType with (*this match is here to replace the accept above that had to be deleted*)
                  IDENT a -> next () ; let n = creerNoeud ( ND_DECL(a) ) [] in accept (SEMICOLON) ; n
                  | MUL -> next () ; skip_etoile ()
                  | _ ->  raise (Syntax_Error ("Erreur de syntaxe en ligne L."^(string_of_int !ct.ligne)^" : "^(string_of_tok !ct.tokType)^" lu alors qu'un identifiant d'une variable était attendu")))
                  in 
                  skip_etoile ()
    | IF -> next () ; ( accept LPAREN ; 
              (* Printf.printf "expr cond deb \n"; *)
              let cond = e() in
              (* Printf.printf "expr cond fin \n"; *)
              accept RPAREN ;
              let instr = i () in 
              if(!ct.tokType == ELSE) then (next (); (creerNoeud ND_COND [cond ; instr ; i()]))
              else (creerNoeud ND_COND [cond ; instr]))
    | WHILE -> (next() ; accept(LPAREN) ; 
              let r = e() in accept(RPAREN) ; 
              let instr = i() in 
              let node = (creerNoeud ND_LOOP [
                creerNoeud ND_ANCOR [];
                creerNoeud ND_COND [r;instr;(creerNoeud ND_BREAK [])
                ]
              ]) 
              in node)
    | DO -> (next () ; let instr = i() in 
              accept(WHILE) ; accept(LPAREN) ; 
              let r = e() in accept(RPAREN) ; 
              accept(SEMICOLON) ; 
              let node = creerNoeud ND_LOOP [
                creerNoeud ND_ANCOR [];
                instr ;
                creerNoeud ND_COND [(creerNoeud ND_NOT [r]);
                (creerNoeud ND_BREAK [])
                ]
              ] 
              in node)
    | FOR -> (next() ; accept(LPAREN) ;
              let e1 = e() in accept(SEMICOLON) ; 
              let e2 = e() in accept(SEMICOLON) ; 
              let e3 = e() in
              accept(RPAREN) ; let instr = i() in
              let node = creerNoeud ND_SEQ [
                e1;
                creerNoeud ND_LOOP [
                  creerNoeud ND_COND [
                    e2;
                    instr;
                    (creerNoeud ND_BREAK [])
                  ];
                  creerNoeud ND_ANCOR [];
                  e3
                ]
              ]
              in node)
    | BREAK ->  (next() ; let ret = creerNoeud ND_BREAK [] in accept SEMICOLON ; ret)
    | CONTINUE ->  (next() ; let ret = creerNoeud ND_CONTINUE [] in accept SEMICOLON ; ret)
    | RETURN -> (next() ; let e = e () in (accept SEMICOLON) ; creerNoeud ND_RET [e])
    | SEND -> (next() ; let e = e () in accept SEMICOLON ; creerNoeud ND_SEND [e])
    | _ -> let r = e () in (accept SEMICOLON) ; (creerNoeud ND_DROP [r])
and f () :node=
 (* Printf.printf "##FUN## \n"; *)
  accept INT ;
  (match !ct.tokType with
        IDENT nomFun -> ( next ();
          (* Printf.printf "fun def %s \n" nomFun; *)
          accept LPAREN ; 
          let rec boucle (l:node list) : node list=
            (match !ct.tokType with
            INT -> ( next () ; (*on passe le INT*)
              match !ct.tokType with
              IDENT nomParam -> next () (*On passe le IDENT*) ; boucle ((creerNoeud (ND_DECL nomParam) [])::l)
              | _ -> raise (Syntax_Error ("Erreur de syntaxe en ligne L."^(string_of_int !ct.ligne)^" : "^(string_of_tok !ct.tokType)^" lu alors qu'un 'INT' ou une ')' était attendu"))
              )
            | RPAREN -> next(); ((i ())::l); (*Prend en charge l'instruction après la fonc*)
            | COMMA -> next(); boucle l ; (*Permet int f(int a,,,,,,,,, int b,,,,,) ; pour desac, faudrait match en post ident param*)
            | _ -> raise (Syntax_Error ("Erreur de syntaxe en ligne L."^(string_of_int !ct.ligne)^" : "^(string_of_tok !ct.tokType)^"lu alors qu'un identifiant d'une variable était attendu"))
            )
          in 
          creerNoeud (ND_FONC (nomFun , (ref (-1)))) (List.rev (boucle []))
        )
        |_ -> raise (Syntax_Error ("Erreur de syntaxe en ligne L."^(string_of_int !ct.ligne)^" : "^(string_of_tok !ct.tokType)^"lu alors qu'un identifiant d'une variable était attendu"))
  )
;;

let analyseurSyntaxique () = 
  f ()
;;

let rec anaSem node: unit =
  (* Printf.printf "##################\n" ; printArbre node; Printf.printf "##################\n"; *)
  (* Printf.printf "nb gosses %s :  %d %!\n" (string_of_node node.nodeType) (List.length node.childs); *)

  match node.nodeType with
  ND_ASSIGN ->  (match (List.hd node.childs).nodeType with
                  ND_REF _ | ND_IND-> List.iter anaSem (node.childs);
                  | _  -> raise (Syntax_Error ("erreur")))
  | ND_DECL a -> SymbTable.declare a INT
  | ND_REF (name, pos) -> (let s = SymbTable.find (name) INT in 
                          match s.typ with
                          INT -> pos := s.pos ; ()
                          | _ -> raise (Syntax_Error ("Variable "^name^" is not an integer")))
  | ND_BLOC -> SymbTable.begin_bloc (); List.iter anaSem (node.childs); SymbTable.end_bloc () ;
  | ND_ADR -> (match (List.hd node.childs).nodeType with
                ND_REF _ | ND_IND -> List.iter anaSem (node.childs)
                | _ -> raise (Syntax_Error ("Cannot get adress of something that is not a variable")))
  | (ND_FONC (nomFunc, varloc)) -> (
    SymbTable.declare nomFunc FUNCTION ; 
    SymbTable.begin_bloc () ;
    nvar := 0 ;

    (* Printf.printf " --- %!\n" ;
    printArbre node ;
    Printf.printf " --- %!\n" ; *)

    List.iter anaSem (node.childs);
    SymbTable.end_bloc () ;
    varloc:=!nvar-((List.length node.childs)-1))
  | ND_APPEL -> (match (List.hd node.childs).nodeType with
              ND_REF (name,pos) -> (
                let s = SymbTable.find name FUNCTION in 
                (if (s.typ != FUNCTION) then
                  raise (Syntax_Error (name^" n'est pas une référence vers une fonction")));
                List.iter anaSem (List.tl node.childs)
              )
              | _ -> raise (Syntax_Error ("Un appel de fonction était attendu"))
              )
  | _ -> List.iter anaSem (node.childs) ; ()
;;

let optimisateur node:node =
  node
;;


let rec genCode (node:node) =
  let parcours_arbre_suffix node=
  (* degueu mais on sait que ça fonctionne, on prend pas de risques, désolé *)
  (* Printf.printf "node parcours suffixe : %s\n" (string_of_node node.nodeType) ; *)
  let rec f = (fun n -> match gencode_of_typeNode n.nodeType with Code c -> List.iter f n.childs; Printf.printf "%s" c ; 
                                                            | _ -> genCode n) in
  match gencode_of_typeNode node.nodeType with 
  Code c -> (f node) 
  | CodeInconnu -> Printf.printf "Si j'apparais t'as un problème\n" ; (genCode node) (* N'arrive jamais mais on le garde par securité *)
in
  match (gencode_of_typeNode node.nodeType) with
   Code _ ->  (*Printf.printf "----" ; printArbre node;  Printf.printf "----" ;*)
              parcours_arbre_suffix node
   | CodeInconnu -> (
       (* Printf.printf "----" ; printArbre node;  Printf.printf "----" ; *)
      match node.nodeType with
      ND_CONST a -> Printf.printf "push %s \n" (string_of_int a)
      | ND_NOT -> genCode (List.hd node.childs) ; Printf.printf "not \n"
      | ND_UNARY_MINUS -> Printf.printf("push 0 \n"); genCode (List.hd node.childs); Printf.printf "sub \n"
      | ND_DECL a -> ()
      | ND_REF (name, pos) -> Printf.printf "get %s        ; nom variable : %s\n" (string_of_int (!pos) ) name
      | ND_ASSIGN -> ( 
        (match (List.hd node.childs).nodeType with
        ND_REF (name, pos) -> genCode (List.nth node.childs 1) ;
                         Printf.printf "dup\n" ;
                         Printf.printf "set %s" ((string_of_int !pos)^"      ; "^name^"\n") 
        | ND_IND -> (   
                        genCode (List.hd (List.tl node.childs)); (* child[1] est la valeur *)
                        Printf.printf "dup\n" ; (* /!\ pas indiqué dans la doc *)
                        genCode (List.hd (List.hd node.childs).childs); (* child of child est l'adresse*)
                        Printf.printf "write\n" )
        | _ -> raise (Syntax_Error ("Noeud assign sans noeud ref à gauche ("^string_of_node (List.hd node.childs).nodeType ^" à gauche)"))
        )
      )
      | ND_COND -> (
        let l = !nlabel in
        nlabel:=(!nlabel+1) ;
        (*PAS FINI (ou si) *)
        genCode (List.hd node.childs) ;
        Printf.printf "jumpf else_%d       ; if false \n" l ;   (*nlabel*)
        genCode (List.nth node.childs 1) ;
        Printf.printf "jump end_cond_%d    ; end if, begin else\n" l; (*nlabel*)
        Printf.printf ".else_%d            ; end else \n" l ;
        if(List.length node.childs == 3) then
          genCode (List.nth node.childs 2) ;
        Printf.printf ".end_cond_%d\n" l)    (*nlabel*)
      | ND_LOOP -> 
      (let l = !nlabel in
      nlabel:=(!nlabel+1);
      let tmp = !label_boucle in
      label_boucle:=l; 
      Printf.printf ".loop1_%d\n" l; 
      List.iter genCode node.childs;
      Printf.printf "jump loop1_%d\n" l;
      Printf.printf ".loop2_%d\n" l;
      label_boucle:= tmp )
      | ND_BREAK -> (
        Printf.printf "jump loop2_%d\n" !label_boucle
      )
      | ND_ANCOR -> (
        Printf.printf ".loop3_%d\n" !label_boucle
      )
      | ND_CONTINUE -> (
        Printf.printf "jump loop3_%d\n" !label_boucle
      )
      | ND_RET -> (genCode (List.hd node.childs) ; Printf.printf "ret\n")
      | ND_APPEL -> (match (List.hd node.childs).nodeType with
           ND_REF (name,pos) -> (
            Printf.printf "prep %s\n" name ;
            List.iter genCode (List.tl node.childs) ;
            Printf.printf "call %d\n" (List.length node.childs-1) )
          | _ -> raise (Syntax_Error ("Cette erreur n'est pas sensée arriver, voir ND_APPEL"))
      )
      | ND_FONC (name,varloc) -> (
        Printf.printf ".%s\n" name ;
        Printf.printf "resn %d\n" !varloc ;
        genCode (List.hd (List.rev node.childs));
        Printf.printf "push 0\n" ;
        Printf.printf "ret\n" ;
      )
      | ND_SEQ -> (List.iter genCode node.childs)
      | ND_IND -> (genCode (List.hd node.childs); Printf.printf "read\n")
      | ND_ADR -> (match (List.hd node.childs).nodeType with
                    ND_REF (name,pos)-> 
                      Printf.printf "prep start   ; calcul de l'adresse de %s\n" name ;
                      Printf.printf "swap\n" ;
                      Printf.printf "drop 1\n" ;
                      Printf.printf "push %d\n" ((!pos)+1)  ;
                      Printf.printf "sub          ; Fin de calcul d'adresse (ND_ADR)\n" ;
                    | ND_IND -> (genCode (List.hd (List.hd node.childs).childs)) 
                    | _ -> raise (Syntax_Error ("Cette erreur n'est pas sensée arriver, voir ND_ADR")))
      | ND_SEND -> (genCode (List.hd node.childs) ; Printf.printf "send\n")
      | ND_RECV -> (Printf.printf "recv\n")
      | _ -> ();
      (* Printf.printf "----" ; *)
   )
;;

let input_files = ref []
let speclist =
  [
    (* ("-o", Arg.Set_string output_file, "Set output file name") *)
  ] ;;  

let anon_fun filename =
  input_files := filename :: !input_files ;;

let usage_msg = "./compiler <file1> [<file2>] ..."

let () =
  Arg.parse speclist anon_fun usage_msg;
  input_files := List.rev !input_files;
  SymbTable.begin_bloc () ;
  for i = 0 to List.length !input_files -1 do
    (* reset between each file global variables *)
    ct := {tokType=INIT; ligne=(-1)};
    lt := {tokType=INIT; ligne=(-1)};
    linenum := 0 ;
    filestring := "" ;
    pointer := 0 ;
    analex (List.nth !input_files i) ;
    let rec boucle () =
      if(!ct.tokType == TOK_EOF) then
        ()
      else 
        (let nodeInMain = analyseurSyntaxique () in
        (* printArbre nodeInMain; 
        Printf.printf "--------- \n";  *)
        anaSem nodeInMain ;
        (* Printf.printf "--------- \n";  *)
        genCode nodeInMain ;
        (* Printf.printf "--------- \n";  *)
        boucle ()) 
    in
    boucle () ;
  done;
  SymbTable.end_bloc () ;
  Printf.printf(".start\n");
  Printf.printf("prep main\n");
  Printf.printf("call 0\n");
  Printf.printf("halt \n");
  
;;

(* GIGA Poubelle
let () =
  SymbTable.begin_bloc () ;
  analex "helloworld.c";
  let rec boucle () =
    if(!ct.tokType == TOK_EOF) then
      ()
    else 
      (let nodeInMain = analyseurSyntaxique () in
      (* printArbre nodeInMain; 
      Printf.printf "--------- \n";  *)
      anaSem nodeInMain ;
      (* Printf.printf "--------- \n";  *)
      genCode nodeInMain ;
      (* Printf.printf "--------- \n";  *)
      boucle ()) 
  in
  boucle () ;
  SymbTable.end_bloc () ; *)

    (*seman*)
    (*N = optim*)
    (*gencode*)

  (* let rec f () =
    let k = read_line () in
    match k with
    "q" -> ()
    | _ -> next (); Printf.printf "%s" (string_of_tok ((!ct).tokType)) ; f (); in
  f () *)
  (* let nodeFin = optimisateur nodeInMain; *)
  (* genCode nodeFin; *)
  (* while(!ct.tokType != TOK_EOF); do
    let nodeN = analyseurSyntaxique;
    nodeN = analyseurSemantique nodeN;
    nodeN = optimisateur nodeN;
    genCode nodeN;
  done; *)
  (* Printf.printf("dbg \n"); *)