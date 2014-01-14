let () =
  let dbh = PGOCaml.connect () in
  let print_test message result =
    Printf.printf "Test [%s] : %s\n"
      (String.escaped message) (String.escaped result) in
  (* let print_list li = "[" ^ String.concat ";" li ^ "]" in *)
  let print_opt conv = function
    | None -> "None"
    | Some v -> "Some("^ conv v ^")" in
  let log = stdout in
  let test_view message conv view =
    print_test message (conv (List.hd (Query.view dbh ~log view))#!x) in
  let test message conv v =
    print_test message (conv (Query.value dbh ~log v)) in
  let test_opt message conv v =
    print_test message (print_opt conv (Query.value_opt dbh ~log v)) in
  test "1 + 2" Int32.to_string <:value< 1 + 2 >>;
  test "1 - 2" Int32.to_string <:value< 1 - 2 >>;
  test "5 / 2" Int32.to_string <:value< 5 / 2 >>;
  test "5 * (4 - 4 / 2)" Int32.to_string <:value< 5 * (4 - 4 / 2) >>;
  test "1.2 + 2.5" string_of_float <:value< 1.2 + 2.5 >>;
  test "1 = 2 - 1" string_of_bool <:value< 1 = 2 - 1 >>;
  test "1 = 2 - 1 && 1 = 2" string_of_bool <:value< 1 = 2 - 1 && 1 = 2 >>;
  test "foo\"bar" (fun s -> s) <:value< "foo\"bar" >>;
  test "foo\nbar" (fun s -> s) <:value< "foo\nbar" >>;
  test "foo\\nbar" (fun s -> s) <:value< "foo\\nbar" >>;
  test "foo\\bar" (fun s -> s) <:value< "foo\\bar" >>;
  test "foo'bar" (fun s -> s) <:value< "foo'bar" >>;
  test "foo''bar" (fun s -> s) <:value< "foo''bar" >>;
  test "foo bar\\" (fun s -> s) <:value< "foo bar\\" >>;
  let accums =
    << group {count = count[t]; sum = sum[t.id]; max = max[t.id]} by {} |
        t in $Base.ingredient$ >> in
  test_view "count[ingredient]" Int64.to_string
    << {x = t.count} | t in $accums$ >>;
  test_view "sum[ingredient.id]" Int32.to_string
    << {x = match t.sum with null -> 0 | n -> n} | t in $accums$ >>;
  test_view "max[ingredient.id]" Int32.to_string
    << {x = match t.max with null -> 0 | n -> n} | t in $accums$ >>;
  test "null IS NULL" string_of_bool <:value< is_null null >>;
  test "null IS NOT NULL" string_of_bool <:value< is_not_null null >>;
  test "null IS NOT DISTINCT FROM null"
    string_of_bool <:value< is_not_distinct_from null null >>;
  test "current_timestamp"
    PGOCaml.string_of_timestamp <:value< current_timestamp >>;
  test_opt "NULL" string_of_bool <:value< null >>;
  let in_list = [(<:value< 1 >>); (<:value< 2 >>); (<:value< 3 >>)] in
  test "(1 IN (1, 2, 3))" string_of_bool <:value< in' 1 $in_list$ >>;
  test "FALSE" string_of_bool <:value< in' 1 $[]$ >>;
  ()
