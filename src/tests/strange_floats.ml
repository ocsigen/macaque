let test = << {nan = $float:nan$;
               inf = $float:infinity$;
               neg_inf = $float:neg_infinity$} >>
let () =
  let conn = PGOCaml.connect () in
  let res = Query.Simple.view_one conn test in
  Printf.printf "nan:%f inf:%f neg_inf:%f\n" res#!nan res#!inf res#!neg_inf
