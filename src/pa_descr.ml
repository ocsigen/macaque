(* macaque : pa_descr.ml
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

open Camlp4
open PreCast

(** Global coherence check configuration *)
let coherence_check = ref false

let () = Options.add "-check_tables" (Arg.Set coherence_check)
  "insert coherence checks with the table descriptions"

(** Description (syntaxic form) structure *)
type descr = table_name * field_descr list
and field_descr = field_name * sql_type * nullable
and sql_type = ident
and table_name = ident
and field_name = ident
and nullable = bool
and ident = string
and 'a located = (Loc.t * 'a)

(** Syntaxic form parsing *)
module DescrGram = MakeGram(Lexer)
let table_descr = DescrGram.Entry.mk "table_description"
let seq_descr = DescrGram.Entry.mk "seq_description"

let () =
  Camlp4_config.antiquotations := true;
  let quote _loc str =
    Syntax.Gram.parse_string Syntax.expr_eoi _loc str in
  EXTEND DescrGram
    GLOBAL: table_descr seq_descr;
    table_descr: [[ name = table_name;
                    "("; fields = LIST0 field_descr SEP ","; ")" ->
                      (_loc, (name, fields)) ]];
    field_descr: [[ name = LIDENT; typ = LIDENT;
                    is_null = nullable; def = OPT default ->
                      (_loc, (name, typ, is_null, def)) ]];
    nullable: [[ "NOT"; "NULL" -> false
               | "NULL" -> true
               | -> true ]];
    value: [[ `ANTIQUOT("", t) -> (_loc, quote _loc t) ]];
    default: [[ "DEFAULT"; "("; v = value; ")" -> v ]];
    table_name: [[ schema = LIDENT; ".";  name = LIDENT -> (Some schema, name)
                 | name = LIDENT -> (None, name) ]];

    seq_descr: [[ op = [id = LIDENT -> id | -> "sequence"]; name = STRING ->
                    (_loc, (op, name)) ]];
  END;;

let camlp4_list _loc =
  let rec to_list = function
    | [] -> <:expr< [] >>
    | hd::tl -> <:expr< [ $hd$ :: $to_list tl$ ] >> in
  to_list

(** Code emission from the syntaxic form *)
let table_of_descr (_loc, (name, field_types)) =
  let type_bindings =
    let bind (_loc, (name, sql_type, nullability, default)) =
      let witness =
        (if nullability then "nullable" else "non_nullable") ^ "_witness" in
      <:binding< $lid:name$ = Sql.Table_type.$lid:sql_type$ Sql.$lid:witness$ >> in
    Ast.biAnd_of_list (List.map bind field_types) in
  let fields = List.map (fun (_loc, (name, _, _, _)) -> (_loc, name)) field_types in
  let descr =
    let field_descr (_loc, name) =
      <:expr< ($str:name$, Sql.untyped_type $lid:name$) >> in
    camlp4_list _loc (List.map field_descr fields) in
  let producer =
    let field_producer (_loc, name) =
      <:expr< ($str:name$, Sql.untyped_t obj#$lid:name$) >> in
    let descr = camlp4_list _loc (List.map field_producer fields) in
    <:expr< Sql.unsafe (fun obj -> $descr$) >> in
  let result_parser =
    let parser_binding (_loc, name) =
      <:binding< $lid:name$ =
            Sql.parse (Sql.recover_type $lid:name$
                         (Sql.unsafe (List.assoc $str:name$ descr))) >> in
    let value_binding (_loc, name) =
      <:binding< $lid:name$ = $lid:name$ input >> in
    let meth (_loc, name) = <:class_str_item< method $lid:name$ = $lid:name$ >> in
    <:expr<
      fun descr ->
        let $Ast.biAnd_of_list (List.map parser_binding fields)$ in
        fun input ->
          let $Ast.biAnd_of_list (List.map value_binding fields)$ in
          object $Ast.crSem_of_list (List.map meth fields)$ end >> in
  let name_expr = match name with
    | (None, table) -> <:expr< (None, $str:table$) >>
    | (Some schema, table) -> <:expr< (Some $str:schema$, $str:table$) >> in
  let defaults =
    let default_values =
      let value = function
        | (_, (name, _, _, Some (_loc, e))) -> [(_loc, name, e)]
        | _ -> [] in
      List.concat (List.map value field_types) in
    let bind (_loc, name, e) = <:binding< $lid:name$ = $e$ >> in
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
  Syntax.Quotation.add "table" Syntax.Quotation.DynAst.expr_tag
    (fun loc _ quote ->
       table_of_descr (DescrGram.parse_string table_descr loc quote));
  Syntax.Quotation.add "sequence" Syntax.Quotation.DynAst.expr_tag
    (fun loc _ quote ->
       seq_of_descr (DescrGram.parse_string seq_descr loc quote));

