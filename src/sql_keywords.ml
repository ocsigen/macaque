(** Most lowercase identifier are perfectly fine as far as Macaque is
    concerned, but some may fail at "sql query generation time"
    because they turn out to be SQL reserved keywords. This runtime
    failure has been reported by Vincent Valat.

    We here have a list of SQL keywords and are careful to warn the
    user when one is used, and to "escape" it into a non-reserved
    identifier.

    Macaque currently only supports PostgreSQL, so it would make sense
    to care about PostgreSQL reserved keywords only, but this will
    hopefully change someday in the future so I decided to also
    consider reserved keywords as defined in the 2003 SQL standard.
*)
type reserved_keyword_status = {
  reserved_in_sql2003 : bool;
  reserved_in_postgresql : bool;
}

let reserved_keywords =
  let keyword pgsql sql03 name =
    if name <> String.uppercase name then
      failwith (Printf.sprintf "Reserved keyword %S should be uppercase" name);
    (name, { reserved_in_sql2003 = sql03; reserved_in_postgresql = pgsql }) in
  let r pgsql sql03 name = [keyword pgsql sql03 name] in
  let l pgsql sql03 names = List.map (keyword pgsql sql03) names in
  (* http://www.postgresql.org/docs/8.3/static/sql-keywords-appendix.html *)
  (* pgSQl SQL03 *) []
  @ r false true  "ABS"
  @ r true  true  "ALL"
  @ r false true  "ALLOCATE"
  @ r false true  "ALTER"
  @ l true  false ["ANALYZE"; "ANALYSE"]
  @ l true  true  ["AND"; "ANY"]
  @ r false true  "ARE"
  @ l true  true  ["ARRAY"; "AS"]
  @ r true  false "ASC"
  @ r false true  "ASENSITIVE"
  @ r true  false "ASYMMETRIC"
  @ l false true  ["AT"; "ATOMIC"]
  @ r true  true  "AUTHORIZATION"
  @ l false true  ["AVG"; "BEGIN"]
  @ r true  true  "BETWEEN"
  @ r false true  "BIGINT"
  @ r true  true  "BINARY"
  @ l false true  ["BLOB"; "BOOLEAN"]
  @ r true  true  "BOTH"
  @ l false true  ["BY"; "CALL"; "CALLED"; "CARDINALITY"; "CASCADED"]
  @ l true  true  ["CASE"; "CAST"]
  @ l false true  ["CEIL"; "CEILING"; "CHAR"; "CHARACTER";
                   "CHARACTER_LENGTH"; "CHAR_LENGTH"]
  @ r true  true  "CHECK"
  @ l false true  ["CLOB"; "CLOSE"; "COALESCE"]
  @ r true  true  "COLLATE"
  @ r false true  "COLLECT"
  @ r true  true  "COLUMN"
  @ l false true  ["COMMIT"; "CONDITION"; "CONNECT"]
  @ r true  true  "CONSTRAINT"
  @ l false true  ["CONVERT"; "CORR"; "CORRESPONDING"; "COUNT";
                   "COVAR_POP"; "COVAR_SAMP"]
  @ l true  true  ["CREATE"; "CROSS"]
  @ l false true  ["CUBE"; "CUME_DIST"; "CURRENT"]
  @ r true  true  "CURRENT_DATE"
  @ l false true  ["CURRENT_DEFAULT_TRANSFORM_GROUP"; "CURRENT_PATH"]
  @ l true  true  ["CURRENT_ROLE"; "CURRENT_TIME"; "CURRENT_TIMESTAMP"]
  @ r false true  "CURRENT_TRANSFORM_GROUP_FOR_TYPE"
  @ r true  true  "CURRENT_USER"
  @ l false true  ["CURSOR"; "CYCLE"; "DATE"; "DAY";
                   "DEALLOCATE"; "DEC"; "DECIMAL"; "DECLARE"]
  @ r true  true  "DEFAULT"
  @ r true  false "DEFERRABLE"
  @ l false true  ["DELETE"; "DENSE_RANK"; "DEREF"]
  @ r true  false "DESC"
  @ l false true  ["DESCRIBE"; "DETERMINISTIC"; "DISCONNECT"]
  @ r true  true  "DISTINCT"
  @ r true  false "DO"
  @ l false true  ["DOUBLE"; "DROP"; "DYNAMIC"; "EACH"; "ELEMENT"]
  @ l true  true  ["ELSE"; "END"]
  @ l false true  ["END-EXEC"; "ESCAPE"; "EVERY"; "EXCEPT";
                   "EXEC"; "EXECUTE"; "EXISTS"; "EXP"; "EXTERNAL"; "EXTRACT"]
  @ r true  true  "FALSE"
  @ l false true  ["FETCH"; "FILTER"; "FLOAT"; "FLOOR"]
  @ l true  true  ["FOR"; "FOREIGN"]
  @ r false true  "FREE"
  @ r true  false "FREEZE"
  @ l true  true  ["FROM"; "FULL"]
  @ l false true  ["FUNCTION"; "FUSION"; "GET"; "GLOBAL"]
  @ l true  true  ["GRANT"; "GROUP"]
  @ r false true  "GROUPING"
  @ r true  true  "HAVING"
  @ l false true  ["HOLD"; "HOUR"; "IDENTITY"]
  @ r false true  "ILIKE"
  @ r true  true  "IN"
  @ r false true  "INDICATOR"
  @ r true  false "INITIALLY"
  @ r true  true  "INNER"
  @ l false true  ["INOUT"; "INSENSITIVE"; "INSERT";
                   "INT"; "INTEGER"; "INTERVAL"]
  @ l true  true  ["INTO"; "IS"]
  @ r true  false "ISNULL"
  @ r true  true  "JOIN"
  @ l false true  ["LANGUAGE"; "LARGE"; "LATERAL"]
  @ l true  true  ["LEADING"; "LEFT"; "LIKE"]
  @ r true  false "LIMIT"
  @ l false true  ["LN"; "LOCAL"]
  @ l true  true  ["LOCALTIME"; "LOCALTIMESTAMP"]
  @ l false true  ["LOWER"; "MATCH"; "MAX"; "MEMBER"; "MERGE"; "METHOD";
                   "MIN"; "MINUTE"; "MOD"; "MODIFIES"; "MODULE"; "MONTH";
                   "MULTISET"; "NATIONAL"]
  @ r true  true  "NATURAL"
  @ l false true  ["NCHAR"; "NCLOB"]
  @ r true  true  "NEW"
  @ l false true  ["NO"; "NONE"; "NORMALIZE"]
  @ r true  true  "NOT"
  @ r false true  "NOTNULL"
  @ r true  true  "NULL"
  @ l false true  ["NULLIF"; "NUMERIC"; "OCTET_LENGTH"; "OF"]
  @ l true  false ["OFF"; "OFFSET"]
  @ l true  true  ["OLD"; "ON"; "ONLY"]
  @ r false true  "OPEN"
  @ l true  true  ["OR"; "ORDER"]
  @ r false true  "OUT"
  @ l true  true  ["OUTER"; "OVERLAP"]
  @ l false true  ["OVERLAY"; "PARAMETER"; "PARTITION";
                   "PERCENTILE_CONT"; "PERCENTILE_DISK"; "PERCENT_RANK"]
  @ r true  false "PLACING"
  @ l false true  ["POSITION"; "POWER"; "PRECISION"; "PREPARE"]
  @ r true  true  "PRIMARY"
  @ l false true  ["PROCEDURE"; "RANGE"; "RANK"; "READS"; "REAL";
                   "RECURSIVE"; "REF"]
  @ r true  true  "REFERENCES"
  @ l false true  ["REFERENCING"; "REGR_AVGX"; "REGR_AVGY"; "REGR_COUNT";
                   "REGR_INTERCEPT"; "REGR_R2"; "REGR_SLOPE";
                   "REGR_SXX"; "REGR_SXY"; "REGR_SYY";
                   "RELEASE"; "RESULT"; "RETURN"]
  @ r true  false "RETURNING"
  @ l false true  ["RETURNS"; "REVOKE"]
  @ r true  true  "RIGHT"
  @ l false true  ["ROLLBACK"; "ROLLUP"; "ROW"; "ROWS"; "ROW_NUMBER";
                   "SCOPE"; "SCROLL"; "SEARCH"; "SECOND"]
  @ r true  true  "SELECT"
  @ r false true  "SENSITIVE"
  @ r true  true  "SESSION_USER"
  @ r false true  "SET"
  @ r true  true  "SIMILAR"
  @ r false true  "SMALLINT"
  @ r true  true  "SOME"
  @ l false true  ["SPECIFIC"; "SPECIFICTYPE";
                   "SQL"; "SQLEXCEPTION"; "SQLSTATE"; "SQLWARNING";
                   "SQRT"; "START"; "STATIC"; "STDDEV_POP"; "STDDEV_SAMP";
                   "SUBMULTISET"; "SUBSTRING"; "SUM"]
  @ r true  true  "SYMMETRIC"
  @ l false true  ["SYSTEM"; "SYSTEM_USER"]
  @ r true  true  "TABLE"
  @ r false true  "TABLESAMPLE"
  @ r true  true  "THEN"
  @ l false true  ["TIME"; "TIMESTAMP"; "TIMEZONE_HOUR"; "TIMEZONE_MINUTE"]
  @ l true  true  ["TO"; "TRAILING"]
  @ l false true  ["TRANSLATE"; "TRANSLATION"; "TREAT"; "TRIGGER"; "TRIM"]
  @ r true  true  "TRUE"
  @ r false true  "UESCAPE"
  @ l true  true  ["UNION"; "UNIQUE"]
  @ l false true  ["UNKNOWN"; "UNNEST"; "UPDATE"; "UPPER"]
  @ l true  true  ["USER"; "USING"]
  @ l false true  ["VALUE"; "VALUES"; "VARCHAR"; "VARYING";
                   "VAR_POP"; "VAR_SAMP"]
  @ r true  false "VERBOSE"
  @ r true  true  "WHEN"
  @ r false true  "WHENEVER"
  @ r true  true  "WHERE"
  @ l false true  ["WIDTH_BUCKET"; "WINDOW"]
  @ r true  true  "WITH"
  @ l false true  ["WITHIN"; "WITHOUT";
                   "XML"; "XMLAGG"; "XMLATTRIBUTES"; "XMLBINARY";
                   "XMLCOMMENT"; "XMLCONCAT"; "XMLELEMENT";
                   "XMLFOREST"; "XMLNAMESPACES"; "XMLPARSE";
                   "XMLPI"; "XMLROOT"; "XMLSERIALIZE";
                   "YEAR"]


(** SQL compatibility warning:
    
    We are going to "quote" identifiers that correspond to reserved
    keywords, so that the query still stays syntactically correct. An
    issue with automatic quoting is that quoted identifiers, beside
    being allowed to contain reserved words, are taken in
    a case-sensitive manner while the rest of SQL is case-insensitive,
    in the sense that they are implicitly normalized by the SQL
    server.

    Now there are funny problem that may arise with this: if you
    define a table as tAbLe, it will internally be define as TABLE
    (if normalized to uppercase) on the server side, and requesting
    the table "tAbLe" will then fail with a "table not found" error.

    Our choice is therefore to case-normalize reserved identifiers
    before quoting them.

    Finally, PostGreSQL does not follow the SQL norm of normalizing
    identifiers to uppercase, it instead normalizes to lowercase. As
    long as Macaque is pgsql-only, we choose lowercase here, but that
    will have to be runtime-configurable in a hopeful future where
    Macaque gets ported to other backends.
*)
let normalize_keyword_case = String.lowercase


(** It is rather awkward to protect SQL identifiers here, at the
    parser level. It would make more sense to preserve the user-input
    identifier as far as possible, that is upto the SQL query
    generation. However, this would require sharing the keyword-base
    code between the Camlp4 extension (which needs to have it to
    generate the warnings) and the output code, which is rather
    awkward to do with the .cmo loading scheme used for Camlp4
    extensions. Doing everything in the extension is just more
    convenient. *)
let keyword_safe identifier =
  let kw = String.uppercase identifier in
  if not (List.mem_assoc kw reserved_keywords) then identifier
  else
    let status = List.assoc kw reserved_keywords in
    if not status.reserved_in_postgresql then identifier
    else
      (* note that we use String.escaped here to escape an
         identifier, not a string litteral. SQL string litteral
         escaping conventions are different and handled by the
         "escape_string" function in sql_printers.ml *)
      Printf.sprintf "%S" (normalize_keyword_case identifier)
