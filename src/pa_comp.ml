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
    "warning for compile-time undetermined \
     update tuple type (see documentation)"
let warn_undetermined_update_message =
  "Warning UPDATE SET : undetermined update tuple, \
   exhaustive update assumed"

let implicit_exhaustive_manipulation = ref false
let () =
  Camlp4.Options.add "-sql-implicit-exhaustive-manipulation"
    (Arg.Set implicit_exhaustive_manipulation)
    "makes the comprehension | optional in UPDATE or DELETE queries \
     with an empty WHERE part"

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
and select =
  { result : select_result located;
    items : comp_item located list;
    order_by : (value located * order) located list option;
    limit : value located option;
    offset : value located option }
and order = Asc | Desc
and select_result =
  | Simple_select of value
  | Group_by of tuple located * tuple located
and comp_item =
  | Bind of table binding
  | Cond of value
and table = Ast.expr
and value =
  | Field of field
  | Atom of Ast.expr
  | Op of value located * value located list
  | If of value located * value located * value located
  | Match of value located * value located  * ident * value located
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

let view_eoi, select_eoi, insert_eoi, delete_eoi, update_eoi,
    value, guard_list =
  let mk = CompGram.Entry.mk in
  mk "view", mk "select", mk "insert", mk "delete", mk "update",
  mk "value", mk "guard list"

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

  let opt_list = function
    | None -> []
    | Some li -> li in

  let unary _loc op = (_loc, Op ((_loc, Ident op), [])) in

  EXTEND CompGram
   GLOBAL: view_eoi select_eoi insert_eoi delete_eoi update_eoi
           value guard_list;

   select_eoi: [[ (_, s) = view; `EOI -> (_loc, Select s) ]];
   view_eoi: [[ (_, s) = view; `EOI -> (_loc, s) ]];
   insert_eoi: [[ tab = table; ":="; sel = view; `EOI ->
                    (_loc, Insert (tab, sel)) ]];
   delete_eoi: [[ bind = row_binding; guards = guard_list; `EOI ->
                    (_loc, Delete (bind, (_loc, guards))) ]];
   update_eoi: [[ bind = row_binding; ":="; res = value;
                  guards = guard_list; `EOI ->
                    (_loc, Update (bind, res, (_loc, guards))) ]];
   guard_list: [[ "|"; items = comp_value_list -> items ]];
   view: [[ result = result;
            order_by = OPT order_by;
            limit = OPT limit;
            offset = OPT offset;
            items = OPT ["|"; li = comp_item_list -> li] ->
              (_loc, { result = result;
                       order_by = order_by;
                       limit = limit;
                       offset = offset;
                       items = opt_list items }) ]];
   result: [[ (_, v) = value -> (_loc, Simple_select v)
            | "group"; group = tuple; by = OPT ["by"; by = tuple -> by] ->
              let by = match by with
                | Some by -> by
                | None -> (_loc, []) in
                (_loc, Group_by (group, by)) ]];
   order_by: [[ "order"; "by"; li = LIST1 sort_expr SEP "," -> li ]];
   sort_expr: [[ v = value; order = OPT ["asc" | "desc"] ->
                   let order = match order with
                     | None | Some "asc" -> Asc
                     | _ -> Desc in
                   (_loc, (v, order)) ]];
   limit: [[ "limit"; v = value -> v ]];
   offset: [[ "offset"; v = value -> v ]];
   comp_item: [[ (_, binding) = row_binding -> (_loc, Bind binding)
               | (_, cond) = value -> (_loc, Cond cond) ]];
   row_binding: [[ handle = LIDENT; "in"; table = table ->  (_loc, (handle, table)) ]];
   table: [[ `ANTIQUOT("", t) -> (_loc, quote _loc t)
           | `ANTIQUOT(id, t) ->
               (_loc, <:expr< Sql.View.$lid:id$ $quote _loc t$ >>) ]];
   value:
     [ "top" RIGHTA
         [ "match"; e = SELF; "with";
           OPT "|"; LIDENT "null"; "->"; null_case = SELF;
           "|"; id = LIDENT; "->"; other_case = SELF ->
             (_loc, Match (e, null_case, id, other_case))
         | "if"; p = SELF; "then"; a = SELF; "else"; b = SELF ->
             (_loc, If(p, a, b)) ]
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
         [ row = SELF; "."; path = field_path ->
             (_loc, Field (row, path)) ]
     | "simple"
         [ v = atom -> (_loc, Atom v)
         | (_, tup) = tuple -> (_loc, Tuple tup)
         | LIDENT "null" -> unary _loc "null"
         | LIDENT "current_timestamp" -> unary _loc "current_timestamp"
         | id = LIDENT -> (_loc, Ident id)
         | "("; (_, e) = SELF; ")" -> (_loc, e)
         | "["; e = SELF; "]" -> (_loc, Accum e) ]];

   comp_item_list:
     [[ i = comp_item; ";"; is = SELF -> i :: is
      | i = comp_item -> [i]
      | -> [] ]];

   comp_value_list:
     [[ v = value; ";"; vs = SELF -> v :: vs
      | v = value -> [v]
      | -> [] ]];

   field_path :
     [[ path = LIST1 [id = LIDENT -> (_loc, id)] SEP "." -> path ]];

   infixop6: [[ x = ["||"] -> <:expr< $lid:x$ >> ]];
   infixop5: [[ x = ["&&"] -> <:expr< $lid:x$ >> ]];

   tuple: [[ "{"; named_fields = binding_list; "}" -> (_loc, named_fields) ]];
   binding: [[ id = LIDENT; "="; v = value -> (_loc, (id, v))
             | v = value LEVEL "simple"; "."; path = field_path ->
                 let (_, name) = List.hd (List.rev path) in
                 (_loc, (name, (_loc, Field(v, path)))) ]];
   binding_list:
     [[ b = binding; ";"; bs = SELF -> b :: bs
      | b = binding -> [b]
      | -> [] ]];

   atom: [[ `ANTIQUOT("", v) -> quote _loc v
          | `INT32(i, _) -> <:expr< Sql.Value.int32 $`int32:i$ >>
          | `INT(i, _) -> <:expr< Sql.Value.int32 (Int32.of_int $`int:i$) >>
          | `STRING(_, s) -> <:expr< Sql.Value.string $`str:s$ >>
          | `FLOAT(f, _) -> <:expr< Sql.Value.float $`flo:f$ >>
          | "true" -> <:expr< Sql.Value.bool True >>
          | "false" -> <:expr< Sql.Value.bool False >>
          | `ANTIQUOT(id, v) ->
              <:expr< Sql.Value.$lid:id$ $quote _loc v$ >> ]];
 END;
;;

let activate_implicit =
  (* this hack is intended to make sure the syntax is extended after
     the command-line parameters have been parsed; the actual call to
     activate_implicit is done in the antiquotation callback *)
  let activated = ref false in
  fun () ->
    if not !activated then begin
      activated := true;
      if !implicit_exhaustive_manipulation then
        (EXTEND CompGram
           GLOBAL: guard_list;
         guard_list: [[ -> [] ]];
         END)
    end

(** Code emission from the syntaxic form *)
let camlp4_list _loc =
  let rec to_list = function
    | [] -> <:expr< [] >>
    | hd::tl -> <:expr< [ $hd$ :: $to_list tl$ ] >> in
  to_list

let camlp4_path _loc path =
  let str (_loc, s) = <:expr< $str:s$ >> in
  camlp4_list _loc (List.map str path)

let camlp4_option _loc = function
  | None -> <:expr< None >>
  | Some expr ->
      let _loc = Ast.loc_of_expr expr in
      <:expr< Some $expr$ >>

module Env : sig
  type env
  val empty : env
  val new_row : ident -> env -> ident * env
  val bound_vars : env -> string list
end = struct
  module SMap = Map.Make(String)
  type env = string SMap.t

  let empty = SMap.empty

  let new_row name env =
    let mangle name =
      let len = String.length name in
      let res = String.create len in
      for i = 0 to len - 1 do
        res.[i] <- (match name.[i] with '\'' -> '$' | c -> c)
      done;
      res in
    let name' = mangle name in
    name', SMap.add name name' env

  let bound_vars env =
    SMap.fold (fun k _ li -> k::li) env []
end
let option_of_comp f = function
  | None -> None
  | Some x -> Some (f x)

let rec view_of_comp (_loc, (select : select) ) =
  let comp_item (from, where, env) (_loc, item) = match item with
    | Cond cond ->
        let where_item =
          <:expr< ($value_of_comp env (_loc, cond)$
                   :> Sql.t < t : Sql.bool_t >) >> in
        (from, where_item :: where, env)
    | Bind (name, table) ->
        let name_str, env = Env.new_row name env in
        let from_table =
          <:expr< ($str:name_str$, Sql.untyped_view $table_of_comp table$) >> in
        let from_row = 
          let name_arg = <:expr< Sql.unsafe $str:name_str$ >> in
          <:binding< $lid:name$ = Sql.row $name_arg$ $table_of_comp table$ >> in
        ((from_row, from_table) :: from, where, env) in
  let (from, where, env) =
    List.fold_left comp_item ([], [], Env.empty) select.items in
  let limit = option_of_comp (value_of_comp Env.empty) select.limit in
  let offset = option_of_comp (value_of_comp Env.empty) select.offset in
  let order_by = option_of_comp (order_by_of_comp env _loc) select.order_by in
  let from_rows, from_tables = List.split from in
  <:expr<
    let (result, from, _where, order_by) =
      (* restricted scope zone *)
      let $Ast.biAnd_of_list (List.rev from_rows)$ in
      ( $result_of_comp env select.result$,
        $camlp4_list _loc (List.rev from_tables)$,
        $camlp4_list _loc (List.rev where)$,
        $camlp4_option _loc order_by$ )
    and limit = $camlp4_option _loc limit$
    and offset = $camlp4_option _loc offset$ in
    (* limit and offset are computed outside the binding scope,
       to avoid illegal variable capture
       (SELECT foo FROM .. LIMIT foo.count) *)
    Sql.view result ?order_by ?limit ?offset from _where >>
and table_of_comp (_loc, table) = table
and order_by_of_comp env _loc order_by =
  let order_of_comp (_loc, (value, order)) =
    let value = value_of_comp env value in
    let order = match order with
      | Asc -> <:expr< Sql.Asc >>
      | Desc -> <:expr< Sql.Desc >> in
    <:expr< (Sql.untyped_t $value$, $order$) >> in
  camlp4_list _loc (List.map order_of_comp order_by)
and result_of_comp env (_loc, r) = match r with
  | Simple_select row -> <:expr< Sql.simple_select $value_of_comp env (_loc, row)$ >>
  | Group_by (group, by) ->
      let bindings_of_comp bindings_comp =
        let bind (_loc, (id, v)) =
          <:binding< $lid:id$ = $value_of_comp env v$ >> in
        Ast.biAnd_of_list (List.map bind bindings_comp) in
      let by_bindings = snd by in
      let rebound_by =
        let rebind (out_loc, (id, (in_loc, _))) =
          (out_loc, (id, (in_loc, Atom <:expr@in_loc< $lid:id$ >>))) in
        List.map rebind by_bindings in
      let rebound_group, accum_bindings =
        let (_loc, group) = group in
        let res = ref [] in
        let count = ref 0 in
        let accum (_loc, expr) =
          let name = Printf.sprintf "accum_%d" !count in
          incr count;
          res := (_loc, (name, (_loc, expr))) :: !res;
          Accum (_loc, Atom <:expr< Sql.accum $lid:name$ >>) in
        let (!!) map (_loc, v) = (_loc, map v) in
        let rec map_tuple tup = List.map !!map_binding tup
        and map_binding (k, v) = (k, !!map_ref v)
        and map_ref = function
          | Field (row, path) -> Field (!!map_ref row, path)
          | Op (op, operands) -> Op (op, List.map !!map_ref operands)
          | Tuple tup -> Tuple (map_tuple tup)
          | Accum expr -> accum expr
          | If (p, a, b) -> If (!!map_ref p, !!map_ref a, !!map_ref b)
          | Match (patt, null_case, id, other_case) ->
              Match (!!map_ref patt, !!map_ref null_case,
                     id, !!map_ref other_case)
          | (Atom _ | Ident _) as v -> v in
        let group = map_tuple group in (* side effect on `res` *)
        group, List.rev !res in
      let env_bindings =
        let rebind id = (_loc, (id, (_loc, Atom <:expr< Sql.grouped_row >>))) in
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
                  $value_of_comp env by_tuple$
                  $value_of_comp env result_tuple$ >>
and value_of_comp env (_loc, r) =
  let (!!) = value_of_comp env in
  match r with
  | Atom v -> v
  | Ident row -> <:expr< $lid:row$ >>
  | Accum expr -> <:expr< Sql.group_of_accum $!!expr$ >>
  | Op (op, operands) ->
      let operation expr e = <:expr< $expr$ $!!e$ >> in
      let operator = match op with
        | (_loc, Ident id) -> <:expr< Sql.Op.$lid:id$ >>
        | expr -> !!expr in
      List.fold_left operation operator operands
  | Field (row, path) ->
      let call obj (_loc, meth_id) = <:expr< $obj$ # $lid:meth_id$ >> in
      <:expr< Sql.field $!!row$
                (Sql.unsafe $camlp4_path _loc path$)
                (Sql.unsafe (fun t -> $List.fold_left call <:expr< t >> path$)) >>
  | If (p, a, b) -> <:expr< Sql.if_then_else $!!p$ $!!a$ $!!b$ >>
  | Match (matched, null_case, id, other_case) ->
      <:expr< Sql.match_null $!!matched$ $!!null_case$
                (fun $lid:id$ -> $!!other_case$) >>
  | Tuple tup ->
      let field_names, field_values =
        let split (_loc, (name, value)) = (_loc, name), value in
        List.split (List.map split tup) in
      let fields_binding action =
        let custom_binding (_loc, (id, value)) =
          <:binding< $lid:id$ = $action _loc id value$ >> in
        Ast.biAnd_of_list (List.map custom_binding tup) in
      let obj =
        let meth (_loc, id) =
          <:class_str_item< method $lid:id$ = $lid:id$ >> in
        <:expr< object $Ast.crSem_of_list (List.map meth field_names)$ end >> in
      let producer =
        let body =
          let field_prod (_loc, name) =
            <:expr< ($str:name$, Sql.untyped_t obj#$lid:name$) >> in
          camlp4_list _loc (List.map field_prod field_names) in
        <:expr< fun obj -> $body$ >> in
      let result_parser =
        let parser_action _loc id _ =
            <:expr< Sql.parse (Sql.recover_type (Sql.get_type fields_obj#$lid:id$)
                                 (Sql.unsafe (List.assoc $str:id$ descr))) >> in
        let parsed_action _loc id _ = <:expr< $lid:id$ input >> in
        <:expr< fun descr -> let $fields_binding parser_action$ in
                  fun input -> let $fields_binding parsed_action$ in $obj$ >> in
      <:expr<
        let fields_obj =
          let $fields_binding (fun _ _ v -> !!v)$ in
          $obj$
        and producer = $producer$ in
        Sql.tuple
          (Sql.unsafe (producer fields_obj))
          (Sql.unsafe producer)
          (Sql.unsafe $result_parser$) >>

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
      let set = query_value set_ast in
      let subtyping_witness =
        let row = <:expr< Sql.row (Sql.unsafe "update row") (Sql.View.table table) >> in
        match set_ast with
          | (_loc, Tuple tup) ->
              let set_type =
                let bind (_loc, (name, _)) = <:ctyp< $lid:name$ : '$lid:name$ >> in
                Ast.tySem_of_list (List.map bind tup) in
              <:expr< (set :> Sql.t (Sql.type_info_only < $set_type$ >)) =
                    ($row$ :> Sql.t (Sql.type_info_only < $set_type$ >)) >>
          | (_loc, _) ->
              if !warn_undetermined_update then
                Syntax.print_warning _loc warn_undetermined_update_message;
              <:expr< set = $row$ >> in
      <:expr< let $binding$ in
              let set = $set$ and _where = $where$
              and table = $table$ and row_name = $row_name$ in
              Sql.update table row_name set
                (Sql.unsafe $subtyping_witness$) _where >>
and query_where (_loc, conds) =
  camlp4_list _loc (List.map query_value conds)
and query_value (_loc, ref) =
  value_of_comp Env.empty (_loc, ref)
and query_binding (_loc, (name, (_, table))) =
  (* TODO factorize comp_items binding *)
  let name_str = <:expr< Sql.unsafe $str:name$ >> in
  table, name_str,
  <:binding< $lid:name$ = Sql.row $name_str$ (Sql.View.table $table$) >>

(** Quotations setup *)
let () =
  Syntax.Quotation.add "value" Syntax.Quotation.DynAst.expr_tag
    (fun loc _ quote ->
       value_of_comp Env.empty (CompGram.parse_string value loc quote));
  Syntax.Quotation.add "view" Syntax.Quotation.DynAst.expr_tag
    (fun loc _ quote ->
       view_of_comp (CompGram.parse_string view_eoi loc quote));
  List.iter
    (fun (name, gram_rule) ->
       Syntax.Quotation.add name Syntax.Quotation.DynAst.expr_tag
         (fun loc _ quote ->
            activate_implicit ();
            query_of_comp (CompGram.parse_string gram_rule loc quote)))
    [ "select", select_eoi;
      "insert", insert_eoi;
      "delete", delete_eoi;
      "update", update_eoi ];
  Syntax.Quotation.default := "view"
