open Sql

let test_size dbh name ?log expected_result view =
  let result = List.length (Query.view dbh ?log view) in
  if result = expected_result then
    Printf.printf "Test %S passed.\n%!" name
  else Printf.printf "Test %S FAILED (result %d, expected %d)\n%!"
    name result expected_result

let clear dbh table =
  Query.query dbh <:delete< _ in $table$ | >>

let install dbh table values =
  List.iter (fun s -> 
    Query.query dbh <:insert< $table$ := {text = $string:s$} >>)
    values

let () =
  let dbh = PGOCaml.connect () in
  let test_size x = test_size dbh x in
  let set1 = ["foo"; "bar"; "baz"; "foo"] in
  let set2 = ["bar"; "baz"] in
  let len1, len2 = List.length set1, List.length set2 in
  let len1_nodup = len1 - 1 in
  let test1, test2 = Base.test1, Base.test2 in
  clear dbh test1;
  clear dbh test2;
  install dbh test1 set1;
  install dbh test2 set2;
  let (!) table = << t | t in $table$ >> in
  test_size "union" len1_nodup (* set2 is included in set1 *)
    << union $!test1$ $!test2$ >>;
  test_size "union all" (len1 + len2)
    << union_all $!test1$ $!test2$ >>;
  test_size "self union" len1_nodup
    << union $!test1$ $!test1$ >>;
  test_size "self union all" (len1 * 2)
    << union_all $!test1$ $!test1$ >>;
  test_size "intersect" 2
    << intersect $!test1$ $!test2$ >>;
  test_size "intersect all" 2
    << intersect_all $!test1$ $!test2$ >>;
  test_size "except" 1
    << except $!test1$ $!test2$ >>;
  test_size "except all" 2
    << except_all $!test1$ $!test2$ >>;
  test_size "except bis" 1
    << except $!test1$ $!test2$ $!test2$ >>;
  test_size "except ter" len1_nodup
    << except $!test1$ (except $!test2$ $!test2$) >>;
  test_size "bugfix1" 2
    << union ({x=null}) ({x=1}) >>;
  test_size "bugfix2" 2
    << union ({x=1}) ({x=null}) >>;
  clear dbh test1;
  clear dbh test2;
  ()
