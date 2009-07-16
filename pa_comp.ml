open Camlp4.PreCast

(** Comprehension (syntaxic form) structure *)
type comp = row located * comp_item list
and row = 
  | Row of ident
  | Tuple of tuple
and tuple = reference binding list
and comp_item =
  | Bind of table binding
  | Cond of cond located
and cond = Eq of reference located * reference located
and table = Ast.expr
and reference =
  | Ref of reference'
  | Nullable_ref of reference' located option
and reference' =
  | Row_ref of row
  | Field of field
  | Value of value quotable
and value =
  | Int of int quotable
  | String of string quotable
and field = ident * ident
and 'a binding = (ident * 'a located) located
and 'a located = (Loc.t * 'a)
and 'a quotable =
  | Quoted of Ast.expr
  | Raw of 'a
and ident = string

(** Syntaxic form parsing *)
module CompGram = MakeGram(Lexer)
let comp = CompGram.Entry.mk "comprehension"
let value = CompGram.Entry.mk "sql_value" 

let () =
  Camlp4_config.antiquotations := true;
  let quote _loc str =
    Syntax.Gram.parse_string Syntax.expr_eoi _loc str in
 EXTEND CompGram
   GLOBAL: comp value;
   comp: [[ result = row; "|"; items = LIST0 comp_item SEP ";"; `EOI ->
              (_loc, (result, items)) ]]; 
   row: [[ tup = tuple -> Tuple tup
         | id = LIDENT -> Row id ]];
   tuple: [[ "("; named_fields = LIST0 binding SEP ","; ")" -> named_fields ]];
   binding: [[ id = LIDENT; "="; v = reference -> (_loc, (id, v)) ]];
   comp_item: [[ handle = LIDENT; "<-"; table = table ->  Bind ((_loc, (handle, table)))
               | c = cond -> Cond c ]];
   table: [[ `ANTIQUOT((""|"table"), t) -> (_loc, quote _loc t) ]];
   cond: [[ a = reference; "="; b = reference -> (_loc, Eq (a, b)) ]];
   reference : [[ "Some"; r = reference' -> (_loc, Nullable_ref (Some r))
                | "None" -> (_loc, Nullable_ref None)
                | r = reference' -> let _loc, r = r in (_loc, Ref r) ]];
   reference' : [[ f = field -> (_loc, Field f)
                 | v = value -> (_loc, Value v)
                 | r = row -> (_loc, Row_ref r) ]];
   value: [[ `ANTIQUOT((""|"value"), v) -> Quoted (quote _loc v)
            | raw = raw_value -> Raw raw ]];
   raw_value:
     [[ `INT(i, _) -> Int (Raw i)
      | `ANTIQUOT("int", v) -> Int (Quoted (quote _loc v))
      | `STRING(_, s) -> String (Raw s)
      | `ANTIQUOT("string", v) -> String (Quoted (quote _loc v)) ]];
   field: [[ table = LIDENT; "."; name = LIDENT -> (table, name) ]];
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

let camlp4_list _loc =
  let rec to_list = function
    | [] -> <:expr< [] >>
    | hd::tl -> <:expr< [ $hd$ :: $to_list tl$ ] >> in
  to_list

module Env : sig
  type env
  val empty : env
  val new_row : ident -> env -> ident * env
  val row : ident -> env -> string
end = struct
  module SMap = Map.Make(String)
  type env = string SMap.t

  let unique name sub_env =
    let rec unique name env =
      if not (SMap.mem name env) then name
      else unique (name ^ "'") env in
    let name' = unique name sub_env in
    name', SMap.add name name' sub_env

  let empty = SMap.empty
  let row = SMap.find
    
  let new_row = unique
end


let rec query_of_comp (_loc, (row, items)) =
  let row = (_loc, row) in
  let comp_item (from, where, env, code_cont) = function
    | Cond (_loc, Eq(a, b)) ->
        let valuer, typer = reference_of_comp env, typer_of_comp env in
        let where_item = <:expr< Sql.Eq ($valuer a$, $valuer b$) >> in
        let code_cont =
          let unification = unify _loc (typer a) (typer b) in
          fun k -> code_cont <:expr< do { $unification$; $k$ } >> in
        (from, where_item :: where, env, code_cont)
    | Bind ((_loc, (name, table))) ->
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
        <:expr< ($str:id$, $reference_of_comp env value$) >> in
      <:expr< Sql.Tuple $camlp4_list _loc (List.map reference_of_item tup)$ >>
and typer_of_row env (_loc, row) t = match row with
  | Row row -> <:expr< ignore ($lid:Env.row row env$ : Sql.view $t$) >>
  | Tuple tup ->
      let item_typers, tuple_t =
        let item (_loc, (id, value)) =
          let item_t = new_tvar _loc in
          typer_of_comp env value item_t,
          <:ctyp< $lid:id$ : $item_t$ >> in
        List.split (List.map item tup) in
      <:expr< do { $Ast.exSem_of_list item_typers$;
                   ignore ((None : option $t$)
                           : option < $Ast.tySem_of_list tuple_t$ >) } >>
and parser_of_row env (_loc, row) = match row with
  | Row row -> <:expr< $lid:Env.row row env$.Sql.result_parser >>
  | Tuple tup ->
      let obj =
        let meth (_loc, (id, _)) = <:class_str_item< method $lid:id$ = $lid:id$ >> in
        <:expr< object $Ast.crSem_of_list (List.map meth tup)$ end >> in
      let decl (_loc, (id, _)) decls =
        <:expr< let $lid:id$ = Sql.call descr $str:id$ input in $decls$ >> in
      <:expr< fun input -> $List.fold_right decl tup obj$ >>
and reference_of_comp env (_loc, r) =
    let reference _loc = function
      | Row_ref row -> <:expr< Sql.Row_ref $reference_of_row env (_loc, row)$ >>
      | Field (table, name) -> <:expr< Sql.Field ($str:Env.row table env$, $str:name$) >>
      | Value v -> <:expr< Sql.Value (Sql.Value.concrete $value_of_comp _loc v$) >> in
    match r with
      | Nullable_ref None -> <:expr< Sql.Null >>
      | Ref r -> reference _loc r
      | Nullable_ref (Some (_loc, r)) -> reference _loc r
and value_of_comp _loc = function
  | Quoted expr -> <:expr< ( $expr$ : Sql.Value.t _ ) >>
  | Raw v -> match v with
      | Int (Raw i) ->  <:expr< Sql.Value.int $`int:i$ >>
      | Int (Quoted i) -> <:expr< Sql.Value.int $i$ >>
      | String (Raw s) -> <:expr< Sql.Value.string $`str:s$ >>
      | String (Quoted s) -> <:expr< Sql.Value.string $s$ >>
and typer_of_comp env (_loc, r) t =
    let typer t _loc = function
      | Row_ref row -> typer_of_row env (_loc, row) t
      | Field (table, name) -> <:expr< ignore ($lid:Env.row table env$ : Sql.view < $lid:name$ : $t$ ; .. > ) >>
      | Value (Quoted expr) -> <:expr< ignore ($expr$ : Sql.Value.t $t$) >>
      | Value (Raw v) -> match v with
          | Int _ -> <:expr< ignore (0 : $t$) >>
          | String _ -> <:expr< ignore ("" : $t$) >> in
    match r with
      | Ref v -> typer t _loc v
      | Nullable_ref None -> <:expr< ignore ((None : $t$) : option _) >>
      | Nullable_ref (Some (loc, v)) ->
          let some_t = new_tvar loc in
          <:expr< do { $typer some_t loc v$;
                       ignore ((None : $t$) : option $some_t$) } >>
and descr_of_comp env (_loc, r) =
    let descr_of_val _loc = function
      | Row_ref row ->
          <:expr< let descr = $descr_of_row env (_loc, row)$ in
                  Sql.Not_null (Sql.TRecord
                    (descr, Sql.unsafe_parser $parser_of_row env (_loc, row)$)) >>
      | Field (table, name) -> <:expr< Sql.get_field_type $lid:Env.row table env$.Sql.descr $str:name$ >>
      | Value (Quoted expr) -> <:expr< Sql.value_type (Sql.Value.concrete $expr$) >>
      | Value (Raw v) ->
          match v with
            | Int _ -> <:expr< Sql.Not_null Sql.TInt >>
            | String _ -> <:expr< Sql.Not_null Sql.TString >> in
    match r with
      | Ref v -> descr_of_val _loc v
      | Nullable_ref None -> <:expr< Sql.Nullable None >>
      | Nullable_ref (Some (loc, v)) -> <:expr< Sql.nullable $descr_of_val loc v$ >>
and table_of_comp (_loc, table) = table

(** Quotations setup *)
let () =
  Syntax.Quotation.add "sql" Syntax.Quotation.DynAst.expr_tag
    (fun loc _ quote -> query_of_comp (CompGram.parse_string comp loc quote));
  Syntax.Quotation.add "sql_val" Syntax.Quotation.DynAst.expr_tag
    (fun loc _ quote ->
       value_of_comp loc (CompGram.parse_string value loc quote));
  Syntax.Quotation.default := "sql"
