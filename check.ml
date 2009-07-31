module ISql = Inner_sql

let eprintf = Printf.eprintf

let check_description table_name descr pgsql_descr =
  let check (correct, descr) field =
    let field_name = Sql.get field#name in
    let field_type, descr =
      try Some (List.assoc field_name descr),
        List.remove_assoc field_name descr
      with Not_found -> None, descr in
    match field_type with
      | None ->
          eprintf "SQL Check Warning : In table %s, field %s undescribed\n"
            table_name field_name;
          (correct, descr)
      | Some field_type ->
          let correct = ref correct in
          let sql_type =
            ISql.sql_type_of_string (Sql.get field#data_type) in
          let nullable =
            match Sql.get field#nullable with
              | "YES" -> true
              | "NO" -> false
              | other ->
                  correct := false;
                  eprintf "SQL Check Error : In table %s, field %s \
                           has unknown 'is_nullable' value : '%s'\n"
                    table_name field_name other;
                  true in
          (match field_type with
             | ISql.Nullable _ when not nullable ->
                 correct := false;
                 eprintf "SQL Check Error : In table %s, field %s \
                          is described as NULL but is NOT NULL\n"
                   table_name field_name
             | ISql.Non_nullable _ when nullable ->
                 correct := false;
                 eprintf "SQL Check Error : In table %s, field %s \
                          is described as NOT NULL but is NULL\n"
                   table_name field_name
             | _ -> ());
          (match field_type with
             | ISql.Nullable (Some t) | ISql.Non_nullable t
               when t <> sql_type->
                 correct := false;
                 eprintf "SQL Check Error : In table %s, field %s \
                          has incompatible types :\n\
                          \t%s in description, %s in table\n"
                   table_name field_name
                   (ISql.string_of_sql_type t) (Sql.get field#data_type)
             | _ -> ());
          !correct, descr in
  let correct, left_descr = List.fold_left check (true, descr) pgsql_descr in
  let correct = correct && left_descr = [] in
  List.iter
    (fun (field_name, _) ->
       eprintf
         "SQL Check Error : In table %s, field %s \
          is decribed but does not exists\n"
         table_name field_name)
    left_descr;
  if not correct then
    failwith
      (Printf.sprintf
         "SQL Check : Coherence check of table %s \
          against the PGSQL database failed."
         table_name)
  else
    eprintf "SQL Check : Table %s description \
             is coherent with the PGSQL database.\n"
      table_name;
  flush stderr

let check comp =
  match comp.ISql.concrete with
    | ISql.Selection q -> failwith "unsupported query check"
    | ISql.Table (schema, table) ->
        let table_name = ISql.string_of_table_name (schema, table) in
        let schema = match schema with
          | None -> "public"
          | Some schema -> schema in
        let pgsql_columns =
          <:table< information_schema.columns (
            table_schema text NOT NULL,
            table_name text NOT NULL,
            column_name text NOT NULL,
            data_type text NOT NULL,
            is_nullable text NOT NULL
          ) >> in
        let check_comp = <:select<
          { name = info.column_name;
            data_type = info.data_type;
            nullable = info.is_nullable } |
          info <- $pgsql_columns$;
          info.table_schema = $string:schema$;
          info.table_name = $string:table$ >> in
        let dbh = PGOCaml.connect () in
        let check_result =
          try `Result (check_description table_name comp.Inner_sql.descr
                         (Query.query dbh check_comp))
          with exn -> `Exn exn in
        PGOCaml.close dbh;
        match check_result with
          | `Result res -> res
          | `Exn exn -> raise exn
