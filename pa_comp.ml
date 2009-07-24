open Camlp4.PreCast

(** Comprehension (syntaxic form) structure *)
type comp = row located * comp_item located list
and row =
  | Row of ident
  | Tuple of tuple
and tuple = reference binding located list
and comp_item =
  | Bind of table binding
  | Cond of reference
and table = Ast.expr
and reference =
  | Row_ref of row
  | Field of field
  | Value of value
  | Op of string * reference located list
and value = Ast.expr
and field = ident located * ident located list
and 'a binding = (ident * 'a located)
and 'a located = (Loc.t * 'a)
and ident = string

(** Syntaxic form parsing *)
module CompGram = MakeGram(Lexer)
let comp = CompGram.Entry.mk "comprehension"
let value = CompGram.Entry.mk "sql_value"

let () =
  Camlp4_config.antiquotations := true;
  let quote _loc str =
    Syntax.Gram.parse_string Syntax.expr_eoi _loc str in

  let compentry entry = CompGram.Entry.of_parser
    (Syntax.Gram.Entry.name entry)
    (fun input ->
       try Syntax.Gram.parse_tokens_after_filter entry input
       with _ -> raise Stream.Failure) in

  let infixop4 = compentry Syntax.infixop4 in
  let infixop3 = compentry Syntax.infixop3 in
  let infixop2 = compentry Syntax.infixop2 in
  let infixop1 = compentry Syntax.infixop1 in
  let infixop0 = compentry Syntax.infixop0 in
  let prefixop = compentry Syntax.prefixop in

  let make_op _loc id = <:expr< $lid:id$ >> in
  
  let operation _loc op operands =
    let op_id = match op with
      | <:expr< $lid:id$ >> -> id
      | _ -> assert false in
    (_loc, Op (op_id, operands)) in

  EXTEND CompGram
   GLOBAL: comp value;
   comp: [[ result = row; "|"; items = LIST0 comp_item SEP ";"; `EOI ->
              (_loc, (result, items)) ]];
   row: [[ tup = tuple -> Tuple tup
         | id = LIDENT -> Row id ]];
   tuple: [[ "{"; named_fields = LIST0 binding SEP ";"; "}" -> named_fields ]];
   binding: [[ id = LIDENT; "="; v = reference -> (_loc, (id, v)) ]];
   comp_item: [[ handle = LIDENT; "<-"; table = table ->  (_loc, Bind (handle, table))
               | (_, cond) = reference -> (_loc, Cond cond) ]];
   table: [[ `ANTIQUOT((""|"table"), t) -> (_loc, quote _loc t) ]];
   reference:
     [ "top" RIGHTA [ ]
     | "||" RIGHTA [ e1 = SELF; op = infixop6; e2 = SELF -> operation _loc op [e1; e2] ]
     | "&&" RIGHTA [ e1 = SELF; op = infixop5; e2 = SELF -> operation _loc op [e1; e2] ]
     | "<"  LEFTA [ e1 = SELF; op = infixop0; e2 = SELF -> operation _loc op [e1; e2] ]
     | "^"  RIGHTA [ e1 = SELF; op = infixop1; e2 = SELF -> operation _loc op [e1; e2] ]
     | "+"  LEFTA [ e1 = SELF; op = infixop2; e2 = SELF -> operation _loc op [e1; e2] ]
     | "*"  LEFTA [ e1 = SELF; op = infixop3; e2 = SELF -> operation _loc op [e1; e2] ]
     | "**" RIGHTA [ e1 = SELF; op = infixop4; e2 = SELF -> operation _loc op [e1; e2] ]
     | "apply" LEFTA [ id = ["nullable"]; e = SELF ->
                         operation _loc (make_op _loc id) [e] ]
     | "~-" NONA  [ op = prefixop; e = SELF -> operation _loc op [e] ]
     | "." LEFTA
         [ row = LIDENT; "."; path = LIST0 [id = LIDENT -> (_loc, id)] SEP "." ->
             (_loc, Field ((_loc, row), path)) ]
     | "simple"
         [ v = value -> (_loc, Value v)
         | r = row -> (_loc, Row_ref r)
         | "("; e = SELF; ")" -> (_loc, snd e)
         | id = ["null"] -> operation _loc (make_op _loc id) [] ]];

   infixop6: [[ x = ["||"] -> <:expr< $lid:x$ >> ]];
   infixop5: [[ x = ["&&"] -> <:expr< $lid:x$ >> ]];

   value: [[ `ANTIQUOT("", v) -> quote _loc v
           | `INT(i, _) -> <:expr< Sql.Value.int $`int:i$ >>
           | `STRING(_, s) -> <:expr< Sql.Value.string $`str:s$ >>
           | "true" -> <:expr< Sql.Value.bool True >>
           | "false" -> <:expr< Sql.Value.bool False >>
           | `ANTIQUOT( (* PGOcaml data types *)
               ( "int" | "int16" | "int32" | "int64"
               | "unit" | "bool" | "point" | "float"
               | "bytea"| "string" | "int32_array"
               | "date" | "time" | "timestamp" | "timestampz" | "interval" )
               as type_name, v) -> <:expr< Sql.Value.$lid:type_name$ $quote _loc v$ >> ]];
 END;;

(** Code emission from the syntaxic form *)
let new_tvar =
  let count = ref (-1) in
  let rec tvar n =
    if n < 26 then Printf.sprintf "%c" (char_of_int (n + int_of_char 'a'))
    else tvar (n / 26 - 1) ^ tvar (n mod 26) in
  fun _loc ->
    incr count;
    <:ctyp< '$"fresh_" ^ tvar !count$ >>

let unify _loc a_typer b_typer =
  let tvar = new_tvar _loc in
  <:expr< do { $a_typer tvar$; $b_typer tvar$ } >>

let generic_typer _loc v t =
  <:expr< ignore ($v$ : $t$) >>

let type_unifier _loc ta tb =
  <:expr< ignore ((None : option $ta$) : option $tb$) >>

let camlp4_list _loc =
  let rec to_list = function
    | [] -> <:expr< [] >>
    | hd::tl -> <:expr< [ $hd$ :: $to_list tl$ ] >> in
  to_list

let camlp4_path _loc path =
  let str (_loc, s) = <:expr< $str:s$ >> in
  camlp4_list _loc (List.map str path)

module Env : sig
  type env
  val empty : env
  val new_row : ident -> env -> ident * env
  val row : ident -> env -> string
end = struct
  module SSet = Set.Make(String)
  module SMap = Map.Make(String)
  type env = { used : SSet.t;
               map : string SMap.t }

  let unique name env =
    let rec unique name env =
      if not (SSet.mem name env.used) then name
      else unique (name ^ "'") env in
    let name' = unique name env in
    name', { used = SSet.add name' env.used;
             map = SMap.add name name' env.map }

  let empty = { used = SSet.empty; map = SMap.empty }
  let row row {map=env} =
    try SMap.find row env
    with Not_found -> row

  let new_row = unique
end


let rec query_of_comp (_loc, (row, items)) =
  let row = (_loc, row) in
  let comp_item (from, where, env, code_cont) (_loc, item) = match item with
    | Cond cond ->
        let where_item =
          <:expr< Sql.Value.get_reference $reference_of_comp env (_loc, cond)$ >> in
        (from, where_item :: where, env, code_cont)
    | Bind (name, table) ->
        let name, env = Env.new_row name env in
        let from_item = <:expr< ($str:name$, $lid:name$.Sql.concrete) >> in
        let code_cont k = code_cont
          <:expr< let $lid:name$ = $table_of_comp table$ in $k$ >> in
        (from_item :: from, where, env, code_cont) in
  let (from, where, env, code_cont) =
    List.fold_left comp_item
      ([], [], Env.empty, (fun k -> k)) items in
  let result_type = new_tvar _loc in
  code_cont
    <:expr< do { $typer_of_row env row result_type$;
                 let descr = $descr_of_row env row$ in
                 { Sql.descr = descr;
                   Sql.result_parser = ($parser_of_row env row$
                                       : Sql.result_parser $result_type$);
                   Sql.concrete = Sql.Query
                     { Sql.select = $reference_of_row env row$;
                       Sql.from = $camlp4_list _loc (List.rev from)$;
                       Sql.$lid:"where"$ = $camlp4_list _loc (List.rev where)$ }}} >>
and descr_of_row env (_loc, row) = match row with
  | Row row -> <:expr< $lid:Env.row row env$.Sql.descr >>
  | Tuple tup ->
      let descr_of_item (_loc, (id, value)) =
        <:expr< ($str:id$, $descr_of_comp env value$) >> in
      camlp4_list _loc (List.map descr_of_item tup)
and reference_of_row env (_loc, row) = match row with
  | Row row ->
      let row = Env.row row env in
      <:expr< Sql.Row ($str:row$, $lid:row$.Sql.descr) >>
  | Tuple tup ->
      let reference_of_item (_loc, (id, value)) =
        <:expr< ($str:id$, Sql.Value.get_reference $reference_of_comp env value$) >> in
      <:expr< Sql.Tuple $camlp4_list _loc (List.map reference_of_item tup)$ >>
and typer_of_row env (_loc, row) t = match row with
  | Row row ->
      generic_typer _loc <:expr< $lid:Env.row row env$ >> <:ctyp< Sql.view $t$ >>
  | Tuple tup ->
      let item_typers, tuple_t =
        let item (_loc, (id, value)) =
          let item_t = new_tvar _loc in
          typer_of_comp env value item_t,
          <:ctyp< $lid:id$ : $item_t$ >> in
        List.split (List.map item tup) in
      <:expr< do { $Ast.exSem_of_list item_typers$;
                   $type_unifier _loc t <:ctyp< < $Ast.tySem_of_list tuple_t$ > >>$ } >>
and parser_of_row env (_loc, row) = match row with
  | Row row -> <:expr< $lid:Env.row row env$.Sql.result_parser >>
  | Tuple tup ->
      let obj =
        let meth (_loc, (id, _)) = <:class_str_item< method $lid:id$ = $lid:id$ >> in
        <:expr< object $Ast.crSem_of_list (List.map meth tup)$ end >> in
      let decl (_loc, (id, _)) decls =
        <:expr< let $lid:id$ = Sql.call descr $str:id$ input in $decls$ >> in
      <:expr< fun input -> $List.fold_right decl tup obj$ >>
and reference_of_comp env (_loc, r) = match r with
  | Row_ref row -> <:expr< Sql.Row_ref $reference_of_row env (_loc, row)$ >>
  | Value v -> v
  | Op (op, operands) ->
      let operation expr e = <:expr< $expr$ $reference_of_comp env e$ >> in
      List.fold_left operation <:expr< Sql.Value.$lid:op$ >> operands
  | Field ((row_loc, row_name), path) -> 
      let row_val = <:expr@row_loc< $lid:Env.row row_name env$ >> in
      let call obj (_loc, meth_id) = <:expr< $obj$ # $lid:meth_id$ >> in
      <:expr< Sql.Value.field $str:row_name$ $camlp4_path _loc path$
        $row_val$ (fun t -> $List.fold_left call <:expr< t >> path$) >>
and typer_of_comp env (_loc, r) t = match r with
  | Row_ref row -> typer_of_row env (_loc, row) t
  | Value v -> generic_typer _loc v <:ctyp< Sql.Value.t $t$ _ >>
  | Op (_, _) -> generic_typer _loc (reference_of_comp env (_loc, r)) <:ctyp< Sql.Value.t $t$ _ >>
  | Field (row, path) ->
      let row = let _loc, row = row in <:expr< $lid:Env.row row env$ >> in
      let meth (_loc, id) t = <:ctyp< < $lid:id$ : $t$; .. > >> in
      generic_typer _loc row <:ctyp< Sql.view $List.fold_right meth path t$ >>
and descr_of_comp env (_loc, r) = match r with
  | Value _
  | Op _
  | Field _ -> <:expr< Sql.Value.get_type $reference_of_comp env (_loc, r)$ >>
  | Row_ref row ->
      let parser_expr = <:expr< Sql.unsafe_parser $parser_of_row env (_loc, row)$ >> in
      <:expr< let descr = $descr_of_row env (_loc, row)$ in
              Sql.Non_nullable (Sql.TRecord (descr, $parser_expr$ )) >>
and table_of_comp (_loc, table) = table

(** Quotations setup *)
let () =
  Syntax.Quotation.add "sql" Syntax.Quotation.DynAst.expr_tag
    (fun loc _ quote -> query_of_comp (CompGram.parse_string comp loc quote));
  Syntax.Quotation.add "sql_val" Syntax.Quotation.DynAst.expr_tag
    (fun loc _ quote -> CompGram.parse_string value loc quote);
  Syntax.Quotation.default := "sql"
