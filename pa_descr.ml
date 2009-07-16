open Camlp4
open PreCast

(** Global coherence check configuration *)
let coherence_check = ref false

let () = Options.add "-check_tables" (Arg.Set coherence_check)
  "insert coherence checks with the table descriptions"

(** Description (syntaxic form) structure *)
type descr = table_name * field_descr list
and field_descr = field_name * sql_type * nullable
and sql_type =
  | TInt
  | TString
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
    table_descr: [[ "TABLE"; name = table_name;
                    "("; fields = LIST0 field_descr SEP ","; ")" ->
                      (_loc, (name, fields)) ]];
    field_descr: [[ name = LIDENT; typ = sql_type; is_null = nullable ->
                      (name, typ, is_null) ]];
    sql_type: [[ "integer" -> TInt | "text" -> TString ]];
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
let table_of_descr (_loc, (name, fields)) =
  let descr =
    let field_descr (name, sql_type, nullable) =
      let output_sql_type =
        let _type = match sql_type with
        | TInt -> <:expr< Sql.TInt >>
        | TString -> <:expr< Sql.TString >> in
        if not nullable then <:expr< Sql.Not_null $_type$ >>
        else <:expr< Sql.Nullable (Some $_type$) >> in
      <:expr< ($str:name$, $output_sql_type$) >> in
    camlp4_list _loc (List.map field_descr fields) in
  let obj = 
    let field_meth (name, sql_type, nullable) =
      let output_caml_type =
        let _type = match sql_type with
        | TInt -> <:ctyp< int >>
        | TString -> <:ctyp< string >> in
        if not nullable then _type
        else <:ctyp< option $_type$ >> in
      <:class_str_item< method $lid:name$ : $output_caml_type$ = $lid:name$ >> in
    <:expr< object $Ast.crSem_of_list (List.map field_meth fields)$ end >> in
  let result_parser =
    let decl (name, _, _) decls =
      <:expr< let $lid:name$ = Sql.call descr $str:name$ input in $decls$ >> in
    <:expr< fun input -> $List.fold_right decl fields obj$ >> in
  let name_expr = match name with
    | (None, table) -> <:expr< (None, $str:table$) >>
    | (Some schema, table) -> <:expr< (Some $str:schema$, $str:table$) >> in
  let table =
    <:expr<
      let descr = $descr$ in
      { Sql.concrete = Sql.Table $name_expr$;
        Sql.descr = descr;
        Sql.result_parser = $result_parser$ } >> in
  if not !coherence_check then table
  else <:expr< let table = $table$ in do { Check.check table; table } >>

(** Quotations setup *)
let () =
  Syntax.Quotation.add "sql_table_descr" Syntax.Quotation.DynAst.expr_tag
    (fun loc _ quote -> table_of_descr (DescrGram.parse_string table_descr loc quote));
