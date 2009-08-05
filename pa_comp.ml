(* macaque : pa_comp.ml
    MaCaQue : Macros for Caml Queries
    Copyright (C) 2009 Gabriel Scherer, Jérôme Vouillon

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library; see the file LICENSE.  If not, write to
    the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
    Boston, MA 02111-1307, USA.
*)

open Camlp4.PreCast

(** Command-line options *)
let warn_undetermined_update = ref true
let () =
  Camlp4.Options.add "-sql-nowarn-undetermined-update"
    (Arg.Clear warn_undetermined_update)
    "warning for compile-time undetermined update tuple type (see documentation)"
let warn_undetermined_update_message =
  "Warning UPDATE SET : undetermined update tuple, exhaustive update assumed"

(** Comprehension (syntaxic form) structure *)
type comp =
  | Select of select
  | Insert of insert
  | Delete of delete
  | Update of update
and insert = table located * select located
and delete = table binding located * where located
and update = table binding located * value located * where located
and where = value located list
and select = select_result located * comp_item located list
and select_result =
  | Simple_select of value
  | Group_by of tuple located * tuple located
and comp_item =
  | Bind of table binding
  | Cond of value
and table = Ast.expr
and value =
  | Field of field
  | Value of Ast.expr
  | Op of value located * value located list
  | Ident of ident
  | Tuple of tuple
  | Accum of value located
and tuple = value binding located list
and field = value located * ident located list
and 'a binding = (ident * 'a located)
and 'a located = (Loc.t * 'a)
and ident = string

(** Syntaxic form parsing *)
module CompGram = MakeGram(Lexer)
let value, view_eoi, select_eoi, insert_eoi, delete_eoi, update_eoi =
  let mk = CompGram.Entry.mk in
  mk "value", mk "view", mk "select", mk "insert", mk "delete", mk "update"

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

  let operation _loc op operands =
    let op_id = match op with
      | <:expr< $lid:id$ >> -> (_loc, Ident id)
      | _ -> assert false in
    (_loc, Op (op_id, operands)) in

  EXTEND CompGram
   GLOBAL: value view_eoi select_eoi insert_eoi delete_eoi update_eoi;

   select_eoi: [[ (_, s) = view; `EOI -> (_loc, Select s) ]];
   view_eoi: [[ (_, s) = view; `EOI -> (_loc, s) ]];
   insert_eoi: [[ tab = table; ":="; sel = view; `EOI ->
                    (_loc, Insert (tab, sel)) ]];
   delete_eoi: [[ bind = row_binding;
                  "|"; items = LIST0 value SEP ";"; `EOI ->
                    (_loc, Delete (bind, (_loc, items))) ]];
   update_eoi: [[ bind = row_binding; ":="; res = value;
                  "|"; items = LIST0 value SEP ";"; `EOI ->
                    (_loc, Update (bind, res, (_loc, items))) ]];
   view: [[ result = result; "|"; items = LIST0 comp_item SEP ";" ->
              (_loc, (result, items)) ]];
   result: [[ (_, v) = value -> (_loc, Simple_select v)
            | "group"; group = tuple; "by"; by = tuple ->
                (_loc, Group_by (group, by)) ]];
   comp_item: [[ (_, binding) = row_binding -> (_loc, Bind binding)
               | (_, cond) = value -> (_loc, Cond cond) ]];
   row_binding: [[ handle = LIDENT; "<-"; table = table ->  (_loc, (handle, table)) ]];
   table: [[ `ANTIQUOT((""|"table"), t) -> (_loc, quote _loc t) ]];
   value:
     [ "top" RIGHTA [ ]
     | "||" RIGHTA [ e1 = SELF; op = infixop6; e2 = SELF -> operation _loc op [e1; e2] ]
     | "&&" RIGHTA [ e1 = SELF; op = infixop5; e2 = SELF -> operation _loc op [e1; e2] ]
     | "<"  LEFTA [ e1 = SELF; op = infixop0; e2 = SELF -> operation _loc op [e1; e2] ]
     | "^"  RIGHTA [ e1 = SELF; op = infixop1; e2 = SELF -> operation _loc op [e1; e2] ]
     | "+"  LEFTA [ e1 = SELF; op = infixop2; e2 = SELF -> operation _loc op [e1; e2] ]
     | "*"  LEFTA [ e1 = SELF; op = infixop3; e2 = SELF -> operation _loc op [e1; e2] ]
     | "**" RIGHTA [ e1 = SELF; op = infixop4; e2 = SELF -> operation _loc op [e1; e2] ]
     | "apply" LEFTA
         [ id = SELF; e = SELF -> (_loc, Op (id, [e])) ]
     | "~-" NONA  [ op = prefixop; e = SELF -> operation _loc op [e] ]
     | "." LEFTA
         [ row = SELF; "."; path = LIST0 [id = LIDENT -> (_loc, id)] SEP "." ->
             (_loc, Field (row, path)) ]
     | "simple"
         [ v = atom -> (_loc, Value v)
         | (_, tup) = tuple -> (_loc, Tuple tup)
         | LIDENT "null" -> (_loc, Op ((_loc, Ident "null"), []))
         | id = LIDENT -> (_loc, Ident id)
         | "("; (_, e) = SELF; ")" -> (_loc, e)
         | "["; e = SELF; "]" -> (_loc, Accum e) ]];

   infixop6: [[ x = ["||"] -> <:expr< $lid:x$ >> ]];
   infixop5: [[ x = ["&&"] -> <:expr< $lid:x$ >> ]];

   tuple: [[ "{"; (_, named_fields) = binding_list; "}" -> (_loc, named_fields) ]];
   binding_list: [[ bindings = LIST0 binding SEP ";" -> (_loc, bindings) ]];
   binding: [[ id = LIDENT; "="; v = value -> (_loc, (id, v)) ]];

   atom: [[ `ANTIQUOT("", v) -> quote _loc v
           | `INT(i, _) -> <:expr< Sql.Data.int $`int:i$ >>
           | `STRING(_, s) -> <:expr< Sql.Data.string $`str:s$ >>
           | `FLOAT(f, _) -> <:expr< Sql.Data.float $`flo:f$ >>
           | "true" -> <:expr< Sql.Data.bool True >>
           | "false" -> <:expr< Sql.Data.bool False >>
           | `ANTIQUOT( (* PGOcaml data types *)
               ( "int" | "int16" | "int32" | "int64"
               | "unit" | "bool" | "point" | "float"
               | "bytea"| "string" | "int32_array"
               | "date" | "time" | "timestamp" | "timestampz" | "interval" )
               as type_name, v) ->
                 <:expr< Sql.Data.$lid:type_name$ $quote _loc v$ >> ]];
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
      else unique (name ^ "_") env in
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


let rec view_of_comp (_loc, (result, items)) =
  let comp_item (from, where, env, code_cont) (_loc, item) = match item with
    | Cond cond ->
        let where_item =
          <:expr< Sql.untyped $reference_of_comp env (_loc, cond)$ >> in
        let where_name = "where_" ^ string_of_int (Random.int max_int) in
        let code_cont k = code_cont
          <:expr< let $lid:where_name$ = $where_item$ in $k$ >> in
        (from, <:expr< $lid:where_name$ >> :: where, env, code_cont)
    | Bind (name, table) ->
        let name_str, env = Env.new_row name env in
        let from_item = <:expr< ($str:name_str$, Sql.untyped_view $table_of_comp table$) >> in
        let code_cont k =
          let runtime_name = <:expr< Sql.unsafe $str:name_str$ >> in
          code_cont
            <:expr< let $lid:name$ =
                      Sql.row $runtime_name$ $table_of_comp table$ in
                    $k$ >> in
        (from_item :: from, where, env, code_cont) in
  let (from, where, env, code_cont) =
    List.fold_left comp_item
      ([], [], Env.empty, (fun k -> k)) items in
  code_cont <:expr< Sql.view
                      $result_of_comp env result$
                      $camlp4_list _loc (List.rev from)$
                      $camlp4_list _loc (List.rev where)$ >>
and result_of_comp env (_loc, r) = match r with
  | Simple_select row -> <:expr< Sql.simple_select $reference_of_comp env (_loc, row)$ >>
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
          Accum (_loc, Value <:expr< Sql.accum $lid:name$ >>) in
        let (!!) map (_loc, v) = (_loc, map v) in
        let rec map_tuple tup = List.map !!map_binding tup
        and map_binding (k, v) = (k, !!map_ref v)
        and map_ref = function
          | Field (row, path) -> Field (!!map_ref row, path)
          | Op (op, operands) -> Op (op, List.map !!map_ref operands)
          | Tuple tup -> Tuple (map_tuple tup)
          | Accum expr -> accum expr
          | (Value _ | Ident _) as v -> v in
        let group = map_tuple group in (* side effect on `res` *)
        group, List.rev !res in
      let env_bindings =
        let rebind id = (_loc, (id, (_loc, Value <:expr< Sql.grouped_row >>))) in
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
        <:expr< Sql.group
                  $reference_of_comp env by_tuple$
                  $reference_of_comp env result_tuple$ >>
and reference_of_comp env (_loc, r) = match r with
  | Value v -> v
  | Ident row -> <:expr< $lid:row$ >>
  | Accum expr -> <:expr< Sql.group_of_accum $reference_of_comp env expr$ >>
  | Op (op, operands) ->
      let operation expr e = <:expr< $expr$ $reference_of_comp env e$ >> in
      let operator = match op with
        | (_loc, Ident id) -> <:expr< Sql.Op.$lid:id$ >>
        | expr -> reference_of_comp env expr in
      List.fold_left operation operator operands
  | Field (row, path) ->
      let call obj (_loc, meth_id) = <:expr< $obj$ # $lid:meth_id$ >> in
      <:expr< Sql.field
                $reference_of_comp env row$
                (Sql.unsafe $camlp4_path _loc path$)
                (Sql.unsafe (fun t -> $List.fold_left call <:expr< t >> path$)) >>
  | Tuple tup ->
      let fields =
        let field_decl (_loc, (name, ref)) =
          let expr = reference_of_comp env ref in
          <:binding< $lid:name$ = Sql.force_gettable (Sql.unsafe $expr$) >> in
        Ast.biAnd_of_list (List.map field_decl tup) in
      let field_list =
        let field_item (_loc, (name, _)) =
          <:expr< ($str:name$, Sql.untyped $lid:name$) >> in
        camlp4_list _loc (List.map field_item tup) in
      let result_parser =
        let obj =
          let meth (_loc, (id, _)) = <:class_str_item< method $lid:id$ = $lid:id$ >> in
          <:expr< object $Ast.crSem_of_list (List.map meth tup)$ end >> in
        let decl (_loc, (id, _)) =
          <:binding< $lid:id$ = Sql.parse $lid:id$ input >> in
        <:expr< fun input -> let $Ast.biAnd_of_list (List.map decl tup)$ in $obj$ >> in
      <:expr< let $fields$ in
              Sql.tuple
                (Sql.unsafe $field_list$)
                (Sql.unsafe $result_parser$) >>
and table_of_comp (_loc, table) = table

let rec query_of_comp (_loc, query) = match query with
  | Select select ->
      <:expr< Sql.select $view_of_comp (_loc, select)$ >>
  | Insert (table, select) ->
      <:expr< Sql.insert $snd table$ $view_of_comp select$ >>
  | Delete (binding, where) ->
        let table, row_name, binding = query_binding binding in
        let where = query_where where in
        <:expr< let $binding$ in
                Sql.delete $table$ $row_name$ $where$ >>
  | Update (binding, set_ast, where) ->
      let table, row_name, binding = query_binding binding in
      let where = query_where where in
      let set = query_reference set_ast in
      let subtyping_witness = match set_ast with
        | (_loc, Tuple tup) ->
            let set_type =
              let bind (_loc, (name, _)) = <:ctyp< $lid:name$ : '$lid:name$ >> in
              Ast.tySem_of_list (List.map bind tup) in
            <:expr< fun (t : Sql.t < t : < $set_type$; .. > as 'a; .. >) ->
                        ($set$ : Sql.t < t : < $set_type$ >; .. > ) >>
        | (_loc, _) ->
            if !warn_undetermined_update then
              Syntax.print_warning _loc warn_undetermined_update_message;
            <:expr< fun t -> t >> in
      <:expr< let $binding$ in
              Sql.update $table$ $row_name$
                               $set$
                               (Sql.unsafe $subtyping_witness$)
                               $where$ >>
and query_where (_loc, conds) =
  camlp4_list _loc (List.map query_reference conds)
and query_reference (_loc, ref) =
  reference_of_comp Env.empty (_loc, ref)
and query_binding (_loc, (name, (_, table))) =
  (* TODO factorize comp_items binding *)
  let name_str = <:expr< Sql.unsafe $str:name$ >> in
  table, name_str,
  <:binding< $lid:name$ = Sql.row $name_str$ $table$ >>

(** Quotations setup *)
let () =
  Syntax.Quotation.add "value" Syntax.Quotation.DynAst.expr_tag
    (fun loc _ quote ->
       reference_of_comp Env.empty (CompGram.parse_string value loc quote));
  Syntax.Quotation.add "view" Syntax.Quotation.DynAst.expr_tag
    (fun loc _ quote ->
       view_of_comp (CompGram.parse_string view_eoi loc quote));
  List.iter
    (fun (name, gram_rule) ->
       Syntax.Quotation.add name Syntax.Quotation.DynAst.expr_tag
         (fun loc _ quote ->
            query_of_comp (CompGram.parse_string gram_rule loc quote)))
    ["select", select_eoi;
     "insert", insert_eoi;
     "delete", delete_eoi;
     "update", update_eoi];
  Syntax.Quotation.default := "view"
