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

let () =
  EXTEND DescrGram
    GLOBAL: table_descr;
    table_descr: [[ name = table_name;
                    "("; fields = LIST0 field_descr SEP ","; ")" ->
                      (_loc, (name, fields)) ]];
    field_descr: [[ name = LIDENT; typ = LIDENT; is_null = nullable ->
                      (_loc, (name, typ, is_null)) ]];
    nullable: [[ "NOT"; "NULL" -> false
               | "NULL" -> true
               | -> true ]];
    table_name: [[ schema = LIDENT; ".";  name = LIDENT -> (Some schema, name)
                 | name = LIDENT -> (None, name) ]];
  END;;

let camlp4_list _loc =
  let rec to_list = function
    | [] -> <:expr< [] >>
    | hd::tl -> <:expr< [ $hd$ :: $to_list tl$ ] >> in
  to_list

(** Code emission from the syntaxic form *)
let table_of_descr (_loc, (name, field_types)) =
  let type_bindings =
    let bind (_loc, (name, sql_type, nullability)) =
      let witness =
        (if nullability then "nullable" else "non_nullable") ^ "_witness" in
      <:binding< $lid:name$ = Sql.Table_type.$lid:sql_type$ Sql.$lid:witness$ >> in
    Ast.biAnd_of_list (List.map bind field_types) in
  let fields = List.map (fun (_loc, (name, _, _)) -> (_loc, name)) field_types in
  let descr =
    let field_descr (_loc, name) =
      <:expr< ($str:name$, Sql.untyped_type $lid:name$) >> in
    camlp4_list _loc (List.map field_descr fields) in
  let result_parser =
    let parser_binding (_loc, name) =
      <:binding< $lid:name$ = poly_parser.Sql.of_type $lid:name$ >> in
    let value_binding (_loc, name) =
      <:binding< $lid:name$ = $lid:name$ input >> in
    let meth (_loc, name) = <:class_str_item< method $lid:name$ = $lid:name$ >> in
    <:expr<
      fun poly_parser ->
        let $Ast.biAnd_of_list (List.map parser_binding fields)$ in
        (fun input ->
           let $Ast.biAnd_of_list (List.map value_binding fields)$ in
         object $Ast.crSem_of_list (List.map meth fields)$ end) >> in
  let name_expr = match name with
    | (None, table) -> <:expr< (None, $str:table$) >>
    | (Some schema, table) -> <:expr< (Some $str:schema$, $str:table$) >> in
  let table = <:expr< let $type_bindings$ in
                      Sql.table $descr$ $result_parser$ $name_expr$ >> in
  if not !coherence_check then table
  else <:expr< let table = $table$ in do { Check.check_table table; table } >>

(** Quotations setup *)
let () =
  Syntax.Quotation.add "table" Syntax.Quotation.DynAst.expr_tag
    (fun loc _ quote -> table_of_descr (DescrGram.parse_string table_descr loc quote));
