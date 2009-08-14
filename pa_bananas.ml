open Camlp4
 
module Id : Sig.Id = struct
  let name = "Bananas"
  let version = "BETA"
end

module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Sig
  include Syntax

  (* creates #! / #? keywords by lexer filtering *)
  let rec banana_filter older_filter stream = 
    let rec filter = parser 
      | [< '(KEYWORD "#", loc); rest >] ->
          (match rest with parser
             | [< '(KEYWORD (("!" | "?") as tok), _) >] -> [< '(KEYWORD ("#" ^ tok), loc); filter rest >]
             | [< >] -> [< '(KEYWORD "#", loc); filter rest >])
      | [< 'other; rest >] -> [< 'other; filter rest >] in
    older_filter (filter stream)

  let () = Token.Filter.define_filter (Gram.get_filter ()) banana_filter

  EXTEND Gram
    expr: LEVEL "." 
    [[ e = SELF; "#!"; lab = label -> <:expr< Sql.get ($e$ # $lab$) >>
     | e = SELF; "#?"; lab = label -> <:expr< Sql.getn ($e$ # $lab$) >> ]];
  END
end

let module M = Register.OCamlSyntaxExtension(Id)(Make) in ()
