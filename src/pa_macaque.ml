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
let coherence_check = ref false
let warn_undetermined_update = ref true

let () =
  Camlp4.Options.add "-check_tables" (Arg.Set coherence_check)
    "insert coherence checks with the table descriptions";
  Camlp4.Options.add "-sql-nowarn-undetermined-update"
    (Arg.Clear warn_undetermined_update)
    "warning for compile-time undetermined \
     update tuple type (see documentation)";
  ()

let warn_undetermined_update_message =
  "Warning UPDATE SET : undetermined update tuple, \
   exhaustive update assumed"

(** Comprehension (syntaxic form) structure *)
type comp =
  | Select of select
  | Insert of insert
  | Delete of delete
  | Update of update
and insert = table located * select located
and delete = table binding located * refinement located
and update = table binding located * value located * refinement located
and refinement = comp_item located list
and where = value located list
and select =
  | Comprehension of comprehension
  | Select_atom of Ast.expr
  | Select_op of select_op located * select located * select located list
    (* first argument is distinguished to ensure argument list is not empty *)
and select_op = ident
and comprehension =
  { result : comp_result located;
    items : refinement located;
    order_by : (value located * order) located list option;
    limit : value located option;
    offset : value located option }
and order = Asc | Desc
and comp_result =
  | Simple_select of value
  | Group_by of tuple located * tuple located
and comp_item =
  | Bind of table binding
  | Cond of value
and table = Ast.expr
and value =
  | Atom of Ast.expr
  | Op of value located * value located list
  | If of value located * value located * value located
  | Match of value located * value located  * ident * value located
  | Ident of ident
  | Tuple of tuple
  | Accum of value located
  | Access of value located * accessor located
and accessor =
  | Field of ident located list located
  | Default of ident located
and tuple = value binding located list
and 'a binding = (ident * 'a located)
and 'a located = (Loc.t * 'a)
and ident = string

(** Description (syntaxic form) structure *)
type descr =
  { table_name : (ident option * ident) located;
    field_descrs : field_descr located list }
and field_descr =
  { field_name : ident;
    sql_type : ident;
    nullable : bool;
    default : value located option }

(** Syntaxic form parsing *)
module CompGram = MakeGram(Lexer)

let view_eoi, select_eoi, insert_eoi, delete_eoi, update_eoi, value =
  let mk = CompGram.Entry.mk in
  mk "view", mk "select", mk "insert", mk "delete", mk "update", mk "value"

let table_descr, seq_descr =
  let mk = CompGram.Entry.mk in
  mk "table_description", mk "seq_description"

let sql_ident, sql_binder =
  let mk = CompGram.Entry.mk in
  mk "sql_ident", mk "sql_binder"

(* The code of this whole phrase has been adapted from the
   camlp4/Camlp4Parsers/Camlp4OCamlRevisedParser.ml file of the OCaml
   distribution, copyright Daniel de Rauglaurde and Nicolas Pouillard.

   The goal is to reuse the exact rules of the OCaml grammar regarding
   infix operators associativies and priorities.

   I would have much preferred to dynamically reuse the parsers from
   the Ocaml grammar, but this is not possible since Ocaml 3.12 where
   the token_info abstract type prevent some reuse between distinct
   grammars. Hence the little bit of code duplication.
*)
let prefixop,
  infixop0, infixop1, infixop2, infixop3, infixop4, infixop5, infixop6 =
  let setup_op_parser name p =
    CompGram.Entry.of_parser name
      (parser
        [< '((KEYWORD x | SYMBOL x), _) when p x >] ->
          Ident x)
  in

  let symbolchar =
    let list =
      ['$'; '!'; '%'; '&'; '*'; '+'; '-'; '.'; '/'; ':'; '<'; '='; '>'; '?';
       '@'; '^'; '|'; '~'; '\\']
    in
    let rec loop s i =
      i = String.length s
      || (List.mem s.[i] list && loop s (i + 1)) in
    loop in


  let prefixop =
    let list = ['!'; '?'; '~'] in
    let excl = ["!="; "??"] in
    setup_op_parser "prefixop"
      (fun x -> not (List.mem x excl) && String.length x >= 2 &&
        List.mem x.[0] list && symbolchar x 1)
  in

  let infixop0 =
    let list_ok = ["<"; ">"; "<="; ">="; "="; "<>"; "=="; "!="; "$"] in
    let list_first_char_ok = ['='; '<'; '>'; '|'; '&'; '$'; '!'] in
    let excl = ["<-"; "||"; "&&"] in
    setup_op_parser "infixop0"
      (fun x -> (List.mem x list_ok) ||
        (not (List.mem x excl) && String.length x >= 2 &&
           List.mem x.[0] list_first_char_ok && symbolchar x 1))
  in

  let infixop1 =
    let list = ['@'; '^'] in
    setup_op_parser "infixop1"
      (fun x -> String.length x >= 1 && List.mem x.[0] list &&
        symbolchar x 1)
  in

  let infixop2 =
    let list = ['+'; '-'] in
    setup_op_parser "infixop2"
      (fun x -> x <> "->" && String.length x >= 1 && List.mem x.[0] list &&
        symbolchar x 1)
  in

  let infixop3 =
    let list = ['*'; '/'; '%'; '\\'] in
    setup_op_parser "infixop3"
      (fun x -> String.length x >= 1 && List.mem x.[0] list &&
        (x.[0] <> '*' || String.length x < 2 || x.[1] <> '*') &&
        symbolchar x 1)
  in

  let infixop4 =
    setup_op_parser "infixop4"
      (fun x -> String.length x >= 2 && x.[0] == '*' && x.[1] == '*' &&
        symbolchar x 2)
  in

  let infixop5 = setup_op_parser "infixop5"
    (fun s -> s = "&&") in

  let infixop6 = setup_op_parser "infixop6"
    (fun s -> s = "||") in

  prefixop, infixop0, infixop1, infixop2, infixop3, infixop4, infixop5, infixop6

let dummy_identifier name = ("dummy_" ^ name)

let () =
  Camlp4_config.antiquotations := true;
  let quote _loc str =
    Syntax.Gram.parse_string Syntax.expr_eoi _loc str in

  let operation _loc op operands = (_loc, Op ((_loc, op), operands)) in

  let unary _loc op = (_loc, Op ((_loc, Ident op), [])) in

  let opt_list _loc = function
    | None -> (_loc, [])
    | Some thing -> thing in

  (* Identifiers *)
  EXTEND CompGram
   GLOBAL: sql_ident sql_binder;

  (* we made the distinction below to allow for camlp4-time
     verification that the binders used do not conflict with SQL
     reserved keywords. We have now moved the keyword-handling work to
     sql_printers.ml that handles the escaping task, but we keep these
     separate rule in case camlp4-time identifier checking becomes
     needed again. *)
  sql_ident:
    [[ id = LIDENT -> id ]];
  sql_binder:
    [[ id = LIDENT -> id ]];
  END;

  (* replacing a LIDENT by a sql_ident or sql_binder in a Camlp4
     grammar rule is not innocuous; apparently it raises ambiguity
     issue because paths that were left-factored with LIDENT are not
     anymore. When a prefix common to two cases is not factored
     anymore, the destructive behavior of Streams makes that only the
     first case is ever tried, as it consumes the input even if it
     ultimately fails.

     Those "test_" functions are there to test, without consuming any
     input (Stream.npeek), if the case is going to be taken, to avoid such
     conflicts. See at use sites. *)
  let test_in_after_ident, test_equal_after_ident =
    let test_after_ident kw ~name:kwname =
      CompGram.Entry.of_parser ("test_" ^ kwname ^ "_after_ident")
        (fun strm -> match Stream.npeek 1 strm with
          | [(LIDENT _, _)] ->
            begin match Stream.npeek 2 strm with
              | [_; (KEYWORD k, _)] when k = kw -> ()
              | _ -> raise Stream.Failure
            end
          | _ -> raise Stream.Failure)
    in
    test_after_ident "in" ~name:"in",
    test_after_ident "=" ~name:"equal"
  in

  (* Comprehensions *)
  EXTEND CompGram
   GLOBAL: view_eoi select_eoi insert_eoi delete_eoi update_eoi value;

   select_eoi: [[ (_, s) = view; `EOI -> (_loc, Select s) ]];
   view_eoi: [[ (_, s) = view; `EOI -> (_loc, s) ]];
   insert_eoi: [[ tab = table; ":="; sel = view; `EOI ->
                    (_loc, Insert (tab, sel)) ]];
   delete_eoi: [[ bind = row_binding; comp_items = refinement; `EOI ->
                    (_loc, Delete (bind, comp_items))
               | tab = table; `EOI ->
                   let id = dummy_identifier "deleted_table" in
                   (_loc, Delete ((_loc, (id, tab)), (_loc, []))) ]];
   update_eoi: [[ bind = row_binding; ":="; res = value;
                  comp_items = refinement; `EOI ->
                    (_loc, Update (bind, res, comp_items)) ]];

   refinement: [[ "|"; items = comp_item_list -> (_loc, items) ]];

   view:
     [ "op"
          [op = view_op; args = LIST1 view_arg ->
            match args with
              | [] -> assert false (* LIST 1 *)
              | arg0::args ->
                (_loc, Select_op (op, arg0, args)) ]
     | "view"
          [ result = result;
            order_by = OPT order_by;
            limit = OPT limit;
            offset = OPT offset;
            items = OPT refinement ->
              (_loc, Comprehension {
                       result = result;
                       order_by = order_by;
                       limit = limit;
                       offset = offset;
                       items = opt_list _loc items;
                     })
          ]
     ];

   view_op: [[
     op = ["union" | "union_all"
          |"intersect" | "intersect_all"
          | "except" | "except_all"]
     -> (_loc, op)
   ]];
   view_arg: [[
     "("; (_,v) = view; ")" -> (_loc, v)
   | `ANTIQUOT("", t) -> (_loc, Select_atom (quote _loc t))
   ]];
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
   row_binding:
     [[ test_in_after_ident; handle = sql_binder; "in"; table = table ->
          (_loc, (handle, table)) ]];
   table: [[ `ANTIQUOT("", t) -> (_loc, quote _loc t)
           | `ANTIQUOT(id, t) ->
               (_loc, <:expr< Sql.View.$lid:id$ $quote _loc t$ >>) ]];

   value:
     [ "top" RIGHTA
         [ "match"; e = SELF; "with";
           OPT "|"; LIDENT "null"; "->"; null_case = SELF;
           "|"; id = sql_binder; "->"; other_case = SELF ->
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
     | "." LEFTA [ row = SELF; ac = accessor -> (_loc, Access(row, ac)) ]
     | "simple"
         [ v = atom -> (_loc, Atom v)
         | (_, tup) = tuple -> (_loc, Tuple tup)
         | LIDENT "null" -> unary _loc "null"
         | LIDENT "current_timestamp" -> unary _loc "current_timestamp"
         | id = sql_ident -> (_loc, Ident id)
         | "("; (_, e) = SELF; ")" -> (_loc, e)
         | "["; e = SELF; "]" -> (_loc, Accum e) ]];

   comp_item_list:
     [[ i = comp_item; ";"; is = SELF -> i :: is
      | i = comp_item -> [i]
      | -> [] ]];

   accessor:
     [[ "."; path = field_path -> (_loc, Field path)
      | "?"; path = [id = sql_ident -> (_loc, id)] -> (_loc, Default path) ]];

   field_path :
     [[ path = LIST1 [id = sql_ident -> (_loc, id)] SEP "." -> (_loc, path) ]];

   tuple: [[ "{"; named_fields = binding_list; "}" -> (_loc, named_fields) ]];
   binding: [[ test_equal_after_ident; id = sql_binder; "="; v = value ->
                 (_loc, (id, v))
             | v = value LEVEL "simple"; ac = accessor ->
                 let (_, default_name) = match ac with
                   | (_, Field (_, path)) -> List.hd (List.rev path)
                   | (_, Default field) -> field in
                 (_loc, (default_name, (_loc, Access(v, ac)))) ]];
   binding_list:
     [[ b = binding; ";"; bs = SELF -> b :: bs
      | b = binding -> [b]
      | -> [] ]];

   atom: [[ `ANTIQUOT("", v) -> quote _loc v
          | `INT(i, _) -> <:expr< Sql.Value.int32 (Int32.of_int $`int:i$) >>
          | `INT32(i, _) -> <:expr< Sql.Value.int32 $`int32:i$ >>
          | `INT64(i, _) -> <:expr< Sql.Value.int64 $`int64:i$ >>
          | `STRING(s, _) -> <:expr< Sql.Value.string $`str:s$ >>
          | `FLOAT(f, _) -> <:expr< Sql.Value.float $`flo:f$ >>
          | "true" -> <:expr< Sql.Value.bool True >>
          | "false" -> <:expr< Sql.Value.bool False >>
          | `ANTIQUOT(id, v) ->
              <:expr< Sql.Value.$lid:id$ $quote _loc v$ >> ]];
 END;

 (* Table descriptions *)
  EXTEND CompGram
    GLOBAL: table_descr seq_descr;
    table_descr: [[ name = table_name;
                    "("; fields = LIST0 field_descr SEP ","; ")" ->
                      (_loc, {table_name = name;
                              field_descrs = fields}) ]];
    (* types are not binding names but reusing existing
       (probably keyword) identifiers, so we use LIDENT instead of
       sql_ident *)
    field_descr: [[ name = sql_binder; typ = LIDENT;
                    is_null = nullable_descr; def = OPT default_descr ->
                      (_loc,
                       { field_name = name;
                         sql_type = typ;
                         nullable = is_null;
                         default = def }) ]];
    nullable_descr: [[ "NOT"; "NULL" -> false
                     | "NULL" -> true
                     | -> true ]];
    default_descr: [[ "DEFAULT"; "("; v = value; ")" -> v ]];
    table_name: [[ schema = sql_binder; ".";  name = sql_binder ->
                   (_loc, (Some schema, name))
                 | name = sql_binder -> (_loc, (None, name)) ]];

    seq_descr: [[ op = [id = sql_ident -> id | -> "sequence"];
                  name = STRING ->
                    (_loc, (op, name)) ]];
  END
;;

(** Code emission from the syntaxic form *)

(* general definitions *)
let map_located f li =
  List.map (fun (_loc, elem) -> (_loc, f _loc elem)) li

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


(* common definitions *)
let producer _loc fields =
  let field_producer (_loc, name) =
    <:expr< ($str:name$, Sql.untyped_t obj#$lid:name$) >> in
  let descr = camlp4_list _loc (List.map field_producer fields) in
  <:expr< fun obj -> $descr$ >>

let result_parser _loc fields =
  let var name = "field_" ^ name in
  let parser_binding (_loc, (name, typ_expr)) =
    <:binding< $lid:var name$ =
                 Sql.parse (Sql.recover_type $typ_expr$
                              (Sql.unsafe (List.assoc $str:name$ descr))) >> in
  let value_binding (_loc, (name, _)) =
    <:binding< $lid:var name$ = $lid:var name$ input >> in
  let meth (_loc, (name, _)) =
    <:class_str_item< method $lid:name$ = $lid:var name$ >> in
  <:expr<
    fun descr ->
      let $Ast.biAnd_of_list (List.map parser_binding fields)$ in
      fun input ->
        let $Ast.biAnd_of_list (List.map value_binding fields)$ in
        object $Ast.crSem_of_list (List.map meth fields)$ end >>

(* Comprehensions *)
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

let rec view_of_comp (_loc, (select : select)) = match select with
  | Comprehension comp -> view_of_simple_comp (_loc, comp)
  | Select_atom expr -> expr
  | Select_op (op, arg0, args) ->
    let viewop =
      let (_loc, lid) = op in
      <:expr< Sql.ViewOp.$lid:lid$ >> in
    let apply f x = <:expr< $viewop$ $f$ $view_of_comp x$ >> in
    (* view operators are all left-associative *)
    List.fold_left apply (view_of_comp arg0) args
and view_of_simple_comp (_loc, (comp : comprehension)) =
  let (from, where, env) = from_where_env_of_compitems comp.items in
  let limit = option_of_comp (value_of_comp Env.empty) comp.limit in
  let offset = option_of_comp (value_of_comp Env.empty) comp.offset in
  let order_by = option_of_comp (order_by_of_comp env _loc) comp.order_by in
  let from_rows, from_tables = List.split from in
  <:expr<
    let (result, from, _where, order_by) =
      (* restricted scope zone *)
      let $Ast.biAnd_of_list from_rows$ in
      ( $result_of_comp env comp.result$,
        $camlp4_list _loc from_tables$,
        $camlp4_list _loc where$,
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
and from_where_env_of_compitems (_loc, items) =
  (* We fold_left then List.rev instead of fold_right because it may
     be important to preserve the binding left-to-right order *)
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
  let fold, where, env =
    List.fold_left comp_item ([], [], Env.empty) items in
  List.rev fold, List.rev where, env
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
          | Access (row, accessor) -> Access (!!map_ref row, accessor)
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
      let order_tuple = List.sort (fun (_, (x, _)) (_, (y, _)) -> String.compare x y) in
      let result_tuple = (_loc, Tuple (order_tuple (rebound_by @ rebound_group))) in
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
    | Access (row, accessor) ->
        let row = !!row in
        let constructor, path, witness =
          (* jumping through hoops to get good error locations:
             from the concrete syntax "foo.bar", the generated code looks like:
               Sql.field (Sql.unsafe ["bar"]) (Sql.unsafe (fun ~row -> row#bar))
             I want (Sql.unsafe ["bar"]) to get the location corresponding
             to "bar", and (Sql.unsafe (fun ~row -> row#bar)) to ".bar".
          *)
          match accessor with
            | (_loc, Field path) ->
              let call obj (_loc, meth_id) = <:expr< $obj$ # $lid:meth_id$ >> in
              let pathval, pathexpr =
                let (_loc, path) = path in
                <:expr< Sql.unsafe $camlp4_path _loc path$ >>,
                (fun row -> List.fold_left call row path) in
              <:expr< Sql.field >>, pathval,
              <:expr< Sql.unsafe (fun ~row -> $pathexpr <:expr< row >>$) >>
            | (_loc, Default field) ->
              let pathval, pathexpr =
                let (_loc, field) = field in
                <:expr< Sql.unsafe $str:field$ >>,
                (fun row -> <:expr< $row$#$lid:field$ >>) in
              <:expr< Sql.default >>, pathval,
              <:expr< Sql.unsafe
                (fun ~default_fields -> $pathexpr <:expr< default_fields >>$) >>
        in
        <:expr< $constructor$ $row$ $path$ $witness$ >>
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
        let result_parser =
          let field_info _loc name = (name, <:expr< Sql.get_type fields_obj#$lid:name$ >>) in
          result_parser _loc (map_located field_info field_names) in
        <:expr<
          let fields_obj =
            let $fields_binding (fun _ _ v -> !!v)$ in
            $obj$
          and producer = $producer _loc field_names$ in
          Sql.tuple
            (Sql.unsafe (producer fields_obj))
            (Sql.unsafe (fun ~tuple -> producer tuple))
            (Sql.unsafe $result_parser$) >>
;;

let rec query_of_comp (_loc, query) = match query with
  | Select select ->
      <:expr< Sql.select $view_of_comp (_loc, select)$ >>
  | Insert (table, select) ->
      <:expr< Sql.insert $snd table$ $view_of_comp select$ >>
  | Delete (binding, items) ->
      let table, row_name, binding = query_binding binding in
      let from, where, env = from_where_env_of_compitems items in
      let from_rows, from_tables = List.split from in
      <:expr< let $binding$ and $Ast.biAnd_of_list from_rows$ in
              let _where = $camlp4_list _loc where$
              and from = $camlp4_list _loc from_tables$
              and table = $table$ and row_name = $row_name$ in
              Sql.delete table row_name from _where >>
  | Update (binding, set_ast, items) ->
      let table, row_name, binding = query_binding binding in
      let subtyping_witness =
        let row = <:expr< Sql.row (Sql.unsafe "update row") table >> in
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
      let from, where, env = from_where_env_of_compitems items in
      let from_rows, from_tables = List.split from in
      <:expr< let $binding$ and $Ast.biAnd_of_list from_rows$ in
              let set = $value_of_comp env set_ast$
              and _where = $camlp4_list _loc where$
              and from = $camlp4_list _loc from_tables$
              and table = $table$ and row_name = $row_name$ in
              let subtyping_witness = $subtyping_witness$ in
              Sql.update table row_name set
                (Sql.unsafe subtyping_witness) from _where >>
and query_binding (_loc, (name, (_, table))) =
  (* TODO factorize comp_items binding *)
  let name_str = <:expr< Sql.unsafe $str:name$ >> in
  table, name_str, <:binding< $lid:name$ = Sql.row $name_str$ $table$ >>
;;


(* Table descriptions *)
let table_of_descr (_loc, table) =
  let field_name name = "table_field_"^name in
  let type_bindings =
    let bind (_loc, f) =
      let witness =
        (if f.nullable then "nullable" else "non_nullable") ^ "_witness" in
      <:binding< $lid:field_name f.field_name$
                   = Sql.Table_type.$lid:f.sql_type$ Sql.$lid:witness$ >> in
    Ast.biAnd_of_list (List.map bind table.field_descrs) in
  let fields = map_located (fun _ descr -> descr.field_name) table.field_descrs in
  let descr =
    let field_descr (_loc, name) =
      <:expr< ($str:name$, Sql.untyped_type $lid:field_name name$) >> in
    camlp4_list _loc (List.map field_descr fields) in
  let producer =
    <:expr< Sql.unsafe (fun ~row -> $producer _loc fields$ row) >> in
  let result_parser =
    let field_info _loc name = name, <:expr< $lid:field_name name$ >> in
    result_parser _loc (map_located field_info fields) in
  let name_expr = match table.table_name with
    | (_loc, (None, table)) -> <:expr< (None, $str:table$) >>
    | (_loc, (Some schema, table)) -> <:expr< (Some $str:schema$, $str:table$) >> in
  let defaults =
    let default_values =
      let value = function
        | (_, {field_name = name; default = Some (_loc, e)}) -> [(_loc, name, e)]
        | _ -> [] in
      List.concat (List.map value table.field_descrs) in
    let bind (_loc, name, default_val) =
      let expr = value_of_comp Env.empty (_loc, default_val) in
      <:binding< $lid:name$ = $expr$ >> in
    let meth (_loc, name, _) =
      <:class_str_item< method $lid:name$ = $lid:name$ >> in
    let assoc (_loc, name, _) = <:expr< ($str:name$, Sql.untyped_t $lid:name$) >> in
    <:expr<
      let $Ast.biAnd_of_list (List.map bind default_values)$ in
      ( object $Ast.crSem_of_list (List.map meth default_values)$ end,
        $camlp4_list _loc (List.map assoc default_values)$ ) >> in
  let table =
    <:expr<
      let $type_bindings$ in
      Sql.table $descr$ $producer$ $result_parser$ $name_expr$ $defaults$ >> in
  if not !coherence_check then table
  else <:expr< let table = $table$ in do { Check.check_table table; table } >>

let seq_of_descr (_loc, (op, name)) =
  let seq = <:expr< Sql.Sequence.$lid:op$ $str:name$ >> in
  if not !coherence_check then seq
  else <:expr< let seq = $seq$ in do { Check.check_sequence seq; seq } >>


(** Quotations setup *)
let () =
  (* Comprehensions *)
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
            query_of_comp (CompGram.parse_string gram_rule loc quote)))
    [ "select", select_eoi;
      "insert", insert_eoi;
      "delete", delete_eoi;
      "update", update_eoi ];

  (* Table descriptions *)
  Syntax.Quotation.add "table" Syntax.Quotation.DynAst.expr_tag
    (fun loc _ quote ->
       table_of_descr (CompGram.parse_string table_descr loc quote));
  Syntax.Quotation.add "sequence" Syntax.Quotation.DynAst.expr_tag
    (fun loc _ quote ->
       seq_of_descr (CompGram.parse_string seq_descr loc quote));

  (* Default quotation *)
  Syntax.Quotation.default := "view"
