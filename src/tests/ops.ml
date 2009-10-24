let () =
  let dbh = PGOCaml.connect () in
  let test' message conv view =
    Printf.printf "Test [%s] : %s\n"
      message
      (conv (List.hd (Query.Simple.view dbh ~log:stdout view))#!x) in
  let test message conv x = test' message conv << {x = $x$} >> in
  test "1 + 2" Int32.to_string <:value< 1 + 2 >>;
  test "1 - 2" Int32.to_string <:value< 1 - 2 >>;
  test "5 / 2" Int32.to_string <:value< 5 / 2 >>;
  test "5 * (4 - 4 / 2)" Int32.to_string <:value< 5 * (4 - 4 / 2) >>;
  test "1.2 + 2.5" string_of_float <:value< 1.2 + 2.5 >>;
  test "1 = 2 - 1" string_of_bool <:value< 1 = 2 - 1 >>;
  test "1 = 2 - 1 && 1 = 2" string_of_bool <:value< 1 = 2 - 1 && 1 = 2 >>;
  let accums =
    << group {count = count[t]; sum = sum[t.id]; max = max[t.id]} by {} |
        t in $Base.ingredient$ >> in
  test' "count[ingredient]" Int64.to_string << {x = t.count} | t in $accums$ >>;
  test' "sum[ingredient.id]" Int32.to_string << {x = t.sum} | t in $accums$ >>;
  test' "max[ingredient.id]" Int32.to_string << {x = t.max} | t in $accums$ >>;
  test "null IS NULL" string_of_bool <:value< is_null null >>;
  test "null IS NOT NULL" string_of_bool <:value< is_not_null null >>;
  test "null IS NOT DISTINCT FROM null" string_of_bool <:value< is_not_distinct_from null null >>;
  test "current_timestamp" PGOCaml.string_of_timestamp <:value< current_timestamp >>;
  ()
