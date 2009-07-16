let eprintf = Printf.eprintf

let check_description table_name descr pgsql_descr =
  let check (correct, descr) field =
    let field_type, descr =
      try Some (List.assoc field#name descr),
        List.remove_assoc field#name descr
      with Not_found -> None, descr in
    match field_type with
      | None -> 
          eprintf "SQL Check Warning : In table %s, field %s undescribed\n"
            table_name field#name;
          (correct, descr)
      | Some field_type ->
          let correct = ref correct in
          let sql_type = Sql.sql_type_of_string field#data_type in
          let nullable =
            match field#nullable with
              | "YES" -> true
              | "NO" -> false
              | other ->
                  correct := false;
                  eprintf "SQL Check Error : In table %s, field %s has unknown 'is_nullable' value : '%s'\n"
                    table_name field#name other;
                  true in
          (match field_type with
             | Sql.Nullable _ when not nullable ->
                 correct := false;
                 eprintf "SQL Check Error : In table %s, field %s is described as NULL but is NOT NULL\n"
                   table_name field#name
             | Sql.Not_null _ when nullable -> 
                 correct := false;
                 eprintf "SQL Check Error : In table %s, field %s is described as NOT NULL but is NULL\n"
                   table_name field#name
             | _ -> ());
          (match field_type with
             | Sql.Nullable (Some t) | Sql.Not_null t when t <> sql_type->
                 correct := false;
                 eprintf "SQL Check Error : In table %s, field %s has incompatible types :\n\t%s in description, %s in table\n"
                   table_name field#name (Sql.string_of_sql_type t) field#data_type
             | _ -> ());
          !correct, descr in
  let correct, left_descr = List.fold_left check (true, descr) pgsql_descr in
  let correct = correct && left_descr = [] in
  List.iter
    (fun (field_name, _) ->
       eprintf
         "SQL CHeck Error : In table %s, field %s is decribed but does not exists\n"
         table_name field_name)
    left_descr;
  if not correct then
    failwith
      (Printf.sprintf
         "SQL Check : Coherence check of table %s against the PGSQL database failed."
         table_name)
  else eprintf "SQL Check : Table %s description is coherent with the PGSQL database.\n" table_name;
  flush stderr

let check comp =
  match comp.Sql.concrete with
    | Sql.Query q -> failwith "unsupported query check"
    | Sql.Table (schema, table) ->
        let table_name = Sql.string_of_table_name (schema, table) in
        let schema = match schema with
          | None -> "public"
          | Some schema -> schema in
        let pgsql_columns =
          <:sql_table_descr< TABLE information_schema.columns (
            table_schema text NOT NULL,
            table_name text NOT NULL,
            column_name text NOT NULL,
            data_type text NOT NULL,
            is_nullable text NOT NULL
          ) >> in
        let check_comp =
          << ( name = info.column_name, 
               data_type = info.data_type,
               nullable = info.is_nullable ) |
              info <- $pgsql_columns$;
              info.table_schema = $string:schema$;
              info.table_name = $string:table$ >> in
        check_description table_name comp.Sql.descr (Query.execute check_comp)

          
        
