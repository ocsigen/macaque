open Camlp4.PreCast

(** Comprehension (syntaxic form) structure *)
type comp = result located * comp_item located list
and result =
  | Select of reference
  | Group_by of tuple located * tuple located
and comp_item =
  | Bind of table binding
  | Cond of reference
and table = Ast.expr
and reference =
  | Field of field
  | Value of value
  | Op of op
  | Row of ident
  | Tuple of tuple
  | Accum of reference located
and tuple = reference binding located list
and value = Ast.expr
and op = string * reference located list
and field = reference located * ident located list
and 'a binding = (ident * 'a located)
and 'a located = (Loc.t * 'a)
and ident = string

(** Syntaxic form parsing *)
module CompGram = MakeGram(Lexer)
let comp = CompGram.Entry.mk "comprehension"
let reference = CompGram.Entry.mk "sql_value"

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
   GLOBAL: comp reference;
   comp: [[ result = result; "|"; items = LIST0 comp_item SEP ";"; `EOI ->
              (_loc, (result, items)) ]];
   result: [[ (_, select) = reference -> (_loc, Select select)
            | "group"; group = tuple; "by"; by = tuple ->
                (_loc, Group_by (group, by)) ]];
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
     | "apply" LEFTA
         [ LIDENT "nullable"; e = SELF ->
             operation _loc (make_op _loc "nullable") [e]
         | LIDENT "count"; e = SELF -> (* TODO generalize ! *)
             operation _loc (make_op _loc "count") [e] ]
     | "~-" NONA  [ op = prefixop; e = SELF -> operation _loc op [e] ]
     | "." LEFTA
         [ row = SELF; "."; path = LIST0 [id = LIDENT -> (_loc, id)] SEP "." ->
             (_loc, Field (row, path)) ]
     | "simple"
         [ v = value -> (_loc, Value v)
         | (_, tup) = tuple -> (_loc, Tuple tup)
         | LIDENT "null" -> operation _loc (make_op _loc "null") []
         | r = LIDENT -> (_loc, Row r)
         | "("; (_, e) = SELF; ")" -> (_loc, e)
         | "["; e = SELF; "]" -> (_loc, Accum e) ]];

   infixop6: [[ x = ["||"] -> <:expr< $lid:x$ >> ]];
   infixop5: [[ x = ["&&"] -> <:expr< $lid:x$ >> ]];

   tuple: [[ "{"; (_, named_fields) = binding_list; "}" -> (_loc, named_fields) ]];
   binding_list: [[ bindings = LIST0 binding SEP ";" -> (_loc, bindings) ]];
   binding: [[ id = LIDENT; "="; v = reference -> (_loc, (id, v)) ]];

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
  val bound_vars : env -> string list
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

  let bound_vars env = SSet.elements env.used
end


let rec query_of_comp (_loc, (result, items)) =
  let comp_item (from, where, env, code_cont) (_loc, item) = match item with
    | Cond cond ->
        let where_item =
          <:expr< Sql.Value.get_reference $reference_of_comp env (_loc, cond)$ >> in
        (from, where_item :: where, env, code_cont)
    | Bind (name, table) ->
        let name, env = Env.new_row name env in
        let from_item = <:expr< ($str:name$, $table_of_comp table$.Sql.concrete) >> in
        let code_cont k =
            let runtime_name = <:expr< Sql.Value.unsafe $str:name$ >> in
            code_cont
              <:expr< let $lid:name$ = Sql.Value.row $runtime_name$ $table_of_comp table$ in $k$ >> in
        (from_item :: from, where, env, code_cont) in
  let (from, where, env, code_cont) =
    List.fold_left comp_item
      ([], [], Env.empty, (fun k -> k)) items in
  code_cont <:expr< Sql.Value.view
                      $result_of_comp env result$
                      $camlp4_list _loc (List.rev from)$
                      $camlp4_list _loc (List.rev where)$ >>
and result_of_comp env (_loc, r) = match r with
  | Select row -> reference_of_comp env (_loc, row)
  | Group_by (group, by) ->
      let bindings_of_comp bindings_comp =
        let bind (_loc, (id, v)) =
          <:binding< $lid:id$ = $reference_of_comp env v$ >> in
        Ast.biAnd_of_list (List.map bind bindings_comp) in
      let by_bindings = snd by in
      let rebound_by =
        let rebind (out_loc, (id, (in_loc, _))) =
          (out_loc, (id, (in_loc, Value <:expr@in_loc< $lid:id$ >>))) in
        List.map rebind by_bindings in
      let rebound_group, accum_bindings =
        let (_loc, group) = group in
        let res = ref [] in
        let count = ref 0 in
        let accum (_loc, expr) =
          let name = Printf.sprintf "accum_%d" !count in
          incr count;
          res := (_loc, (name, (_loc, expr))) :: !res;
          Value <:expr< Sql.Value.accumulate $lid:name$ >> in
        let (!!) map (_loc, v) = (_loc, map v) in
        let rec map_tuple tup = List.map !!map_binding tup
        and map_binding (k, v) = (k, !!map_ref v)
        and map_ref = function
          | Field (row, path) -> Field (!!map_ref row, path)
          | Op (op, operands) -> Op (op, List.map !!map_ref operands)
          | Tuple tup -> Tuple (map_tuple tup)
          | Accum expr -> accum expr
          | (Value _ | Row _) as v -> v in
        let group = map_tuple group in (* side effect on `res` *)
        group, List.rev !res in
      let env_bindings =
        let rebind id = (_loc, (id, (_loc, Value <:expr< Sql.Value.grouped_row >>))) in
        List.map rebind (Env.bound_vars env) in
      let use_bindings bindings =
        let use (_loc, (id, _)) = <:binding< _ = $lid:id$ >> in
        Ast.biAnd_of_list (List.map use bindings) in
      let by_tuple = (_loc, Tuple rebound_by) in
      let result_tuple = (_loc, Tuple (rebound_by @ rebound_group)) in
      let with_bindings bindings cont =
        <:expr< let $bindings_of_comp bindings$ in
                let $use_bindings bindings$ in
                $cont$ >> in
      List.fold_right with_bindings
        [by_bindings; accum_bindings; env_bindings]
        <:expr< Sql.Value.group
                  $reference_of_comp env by_tuple$
                  $reference_of_comp env result_tuple$ >>
and reference_of_comp env (_loc, r) = match r with
  | Value v -> v
  | Row row -> <:expr< $lid:Env.row row env$ >>
  | Op (op, operands) ->
      let operation expr e = <:expr< $expr$ $reference_of_comp env e$ >> in
      List.fold_left operation <:expr< Sql.Value.$lid:op$ >> operands
  | Field (row, path) ->
      let call obj (_loc, meth_id) = <:expr< $obj$ # $lid:meth_id$ >> in
      <:expr< Sql.Value.field
                $reference_of_comp env row$
                (Sql.Value.unsafe $camlp4_path _loc path$)
                (Sql.Value.unsafe (fun t -> $List.fold_left call <:expr< t >> path$)) >>
  | Tuple tup ->
      let fields =
        let field_decl (_loc, (name, ref)) =
          <:binding< $lid:name$ = $reference_of_comp env ref$ >> in
        Ast.biAnd_of_list (List.map field_decl tup) in
      let field_list =
        let field_item (_loc, (name, _)) =
          <:expr< ($str:name$, Sql.Value.untyped $lid:name$) >> in
        camlp4_list _loc (List.map field_item tup) in
      let result_parser =
        let obj =
          let meth (_loc, (id, _)) = <:class_str_item< method $lid:id$ = $lid:id$ >> in
          <:expr< object $Ast.crSem_of_list (List.map meth tup)$ end >> in
        let decl (_loc, (id, _)) =
          <:binding< $lid:id$ = Sql.Value.parse $lid:id$ input >> in
        <:expr< fun input -> let $Ast.biAnd_of_list (List.map decl tup)$ in $obj$ >> in
      <:expr< let $fields$ in
              Sql.Value.tuple (Sql.Value.unsafe $field_list$) (Sql.Value.unsafe $result_parser$) >>
  | Accum expr -> reference_of_comp env expr
and table_of_comp (_loc, table) = table

(** Quotations setup *)
let () =
  Syntax.Quotation.add "select" Syntax.Quotation.DynAst.expr_tag
    (fun loc _ quote ->
       query_of_comp (CompGram.parse_string comp loc quote));
  Syntax.Quotation.add "value" Syntax.Quotation.DynAst.expr_tag
    (fun loc _ quote ->
       reference_of_comp Env.empty (CompGram.parse_string reference loc quote));
  Syntax.Quotation.default := "select"
