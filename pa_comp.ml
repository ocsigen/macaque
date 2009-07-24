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
     | "apply" LEFTA [ LIDENT "nullable"; e = SELF ->
                         operation _loc (make_op _loc "nullable") [e] ]
     | "~-" NONA  [ op = prefixop; e = SELF -> operation _loc op [e] ]
     | "." LEFTA
         [ row = LIDENT; "."; path = LIST0 [id = LIDENT -> (_loc, id)] SEP "." ->
             (_loc, Field ((_loc, row), path)) ]
     | "simple"
         [ v = value -> (_loc, Value v)
         | r = row -> (_loc, Row_ref r)
         | "("; e = SELF; ")" -> (_loc, snd e)
         | LIDENT "null" -> operation _loc (make_op _loc "null") [] ]];

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
  code_cont
    <:expr< 
    let descr = $descr_of_row env row$ in               
    Sql.Value.view
      $reference_of_row env row$
      descr
      $parser_of_row env row$
      $camlp4_list _loc (List.rev from)$
      $camlp4_list _loc (List.rev where)$ >>
and descr_of_row env (_loc, row) = match row with
  | Row row -> <:expr< $lid:Env.row row env$.Sql.descr >>
  | Tuple tup ->
      let descr_of_item (_loc, (id, value)) =
        <:expr< ($str:id$, $descr_of_comp env value$) >> in
      camlp4_list _loc (List.map descr_of_item tup)
and reference_of_row env (_loc, row) = match row with
  | Row row ->
      let row = Env.row row env in
      <:expr< Sql.Value.row (Sql.Value.unsafe $str:row$) $lid:row$ >>
  | Tuple tup ->
      let obj =
        let field_meth (_loc, (name, ref)) =
          <:class_str_item< method $lid:name$ = $reference_of_comp env ref$ >> in
        <:expr< object $Ast.crSem_of_list (List.map field_meth tup)$ end >> in
      let fields =
        (* field_item depends on an 'obj' object in the scope *)
        let field_item (_loc, (name, _)) =
          <:expr< ($str:name$, Sql.Value.untyped obj#$lid:name$) >> in
        camlp4_list _loc (List.map field_item tup) in
      let checker =
        (* checker_meth depends on 'obj' and an 'extractor' function *)
        let checker_meth (_loc, (name, _)) =
          <:class_str_item< method $lid:name$ = extractor.Sql.Value.extract (obj#$lid:name$) >> in
        <:expr< object $Ast.crSem_of_list (List.map checker_meth tup)$ end >> in
      <:expr< let obj = $obj$ in
              Sql.Value.tuple (Sql.Value.unsafe $fields$)
                (fun extractor ->
                   (* camlp4 "func $object ... end$" bug workaround *)
                   let checker = $checker$ in Sql.Value.unsafe checker) >>
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
  | Row_ref row -> reference_of_row env (_loc, row)
  | Value v -> v
  | Op (op, operands) ->
      let operation expr e = <:expr< $expr$ $reference_of_comp env e$ >> in
      List.fold_left operation <:expr< Sql.Value.$lid:op$ >> operands
  | Field ((row_loc, row_name), path) -> 
      let row_val = <:expr@row_loc< $lid:Env.row row_name env$ >> in
      let call obj (_loc, meth_id) = <:expr< $obj$ # $lid:meth_id$ >> in
      <:expr< Sql.Value.field (Sql.Value.unsafe ($str:row_name$, $camlp4_path _loc path$))
        $row_val$ (fun t -> (Sql.Value.unsafe $List.fold_left call <:expr< t >> path$)) >>
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
