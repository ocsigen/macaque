## MaCaQue : Macros for Caml Queries

MaCaQue (or macaque) is a DSL for SQL Queries in Caml.

It has the following objectives :
- be highly compositional
- use expressive types to avoid runtime errors

macaque is still a work in progress, please send feedback to
`bluestorm dot dylc (gmail)`. I'm interested in bug reports and
reasonable feature requests.

Usual makefile provided (in src/) :
  make, make install, bleh, make uninstall
Depends on PG'Ocaml

Summary of this document :
- [Important macaque types and structures](#macaque_types)
- [Sending queries to the SQL server](#sending_queries)
- [Processing query results](#processing_results)
  - [Single value queries](#single_value_query)
- [General syntax : values, comprehensions, queries](#syntax_description)
- [View expressions](#view)
  - [View results](#view_results)
  - [View modifiers](#view_modifiers)
  - [Comprehension items](#comprehension_items)
- [GROUP BY expressions and aggregate functions](#group_by)
  - [Empty GROUP part](#empty_group)
  - [Restrictions on the GROUP record, accumulators \[..\] syntax](#accumulators)
  - [Static GROUP BY typing errors](#static_group_by_typing)
  - [Non-grouped aggregates](#non_grouped_aggregates)
- [Description syntax](#descriptions)
  - [Auto-incrementing columns](#incr_column)
  - [Sequence descriptions](#sequence_descriptions)
  - [Checking descriptions](#check_descriptions)
- [Sql functions, operators and data types](#SQL_operators)
  - [Sql value operators](#value_operators)
  - [Value types antiquotations](#value_antiquotations)
  - [Column types](#column_types)
  - [Sequence operators](#sequence_ops)
  - [View antiquotations](#view_antiquotations)
- [phantom types](#phantom_types)
- [Specific type fourberies](#type_subtleties)
  - [Sql interface safety](#interface_safety)
  - [Update subtyping problem](#update_subtyping)
- [Remarks](#general_remarks)
  - [Side effects](#side_effects)
  - [Semantic of row bindings](#row_binding_semantics)
  - [Camlp4 use](#camlp4_use)
  - [Macaque and PG'OCaml](#pgocaml_compat)
  - [NULL and semantic issues](#null_inference)



### Important macaque types and structures                      [.macaque_types]

macaque builds SQL queries/views/values at three different levels :

##### query

Macaque queries represent `SELECT`, `INSERT`, `DELETE` and `UPDATE`
queries. They can not be composed.

Examples:

- Select all rows from table `sql_table` whose `id` is inferior to `50` :

    <:select< row | row in $sql_table$; row.id < 50 >>

- Delete all rows from `sql_table` whose `id` is superior to `50` :

    <:delete< row in $sql_table$ | row.id > 50 >>


Type : macaques queries have type `'a Sql.query`, where `'a` is the
return type of the query : a row list for `SELECT` queries, unit
otherwise.

##### view

Views represent SQL tables, views and (sub-)-`SELECT` results; view
are composable and can be used nearly anywhere a SQL table can.

Examples :

- User name and category of the user with id superior to `42`

    <:view< {user = u.name; category = cat.name} |
            u in $users$;
            u.id > 32 + 10;
            cat in $categories$;
            cat.id = u.cat >>


- Names of all simple users of the 'prev_view' view (wich might be the previous example view)

    <:view< {name = row.user} |
            row in $prev_view$;
            row.category = "simple user" >>


- Generic function that will project any view on its "name" column

    let names view = <:view< {name = t.name} | t in $view$ >>


- Generic function that will build an associative table, wich the
  'name' column in the 'name' field and the whole column in the
  't' field.

    let names view = <:view< {name = t.name; t = t} | t in $view$ >>


Type : `('a, 'b) Sql.view`, where `'a` is the type of a row
(considered as a value). `'b` is either `non_writable` or `'c
writable` : only writable views correspond to real tables in the SQL
database, and thus insert/update/delete won't accept non-writable
views. The `'c` type parameter in `'c writable` is useless for now, it
will eventually be used to transport information about default values
for table fields.

:Note:

  views and select queries are distinct objects (view = reusable set
  of rows, select = final query), but views are very common so
  a specific library function is provided to directly send a view to
  the SQL server, without having to manually build a select query
  first.

##### value

Values represent SQL values and expressions : `int`, `float`,
`string`, and results of SQL data functions and operators.

Examples :

- `2` (as a Sql Value, not a Caml int)

    <:value< 1 + 1 >>


- `true` (as a Sql Value, not a Caml bool)

    <:value< 1 = 1 >>


- the current timestamp (with local timezone information, use
  `localtimestamp` to omit it):

    <:value< current_timestamp () >>

- field 'id' of table 'foo', casted as a nullable value (wich can take the `NULL` value)

    <:value< nullable foo.id >>


- returns twice the given int, as a Sql value

    let double (n : int) = <:value< 2 * $int:n$ >>


- returns the immediate row with column `foo` and `bar` of values `1` and `baz`

    <:value< { foo = 1; bar = "baz" } >>


Type : `'a Sql.t`, the most pervasive type in macaque. `'a` is
actually a rich type containing the value type (int, string, a row,
etc.)  but also several phantom types witnessing different properties
of the value (detailed description in the "phantom types" section of
this documentation). View rows are also values, and rows can be nested
at will :

  let strange_pack foo = <:value< { a = 1; b = { c = 2; d = $foo$ } } >>




### Sending queries to the SQL server                         [.sending_queries]

Most of macaque efforts are directed towards producing safe and
composables SQL queries, and rebuilding typed values from the
all-string result of SQL queries. The actual interaction with the SQL
server is relatively minor and well separated. It is done through the
`Query` module.

The `Query` module is designed to work with PGOCaml (and is actually the
only part of macaque with a hard dependence on PGOCaml). It thus
reproduces the functorized design of PGOCaml, wich is intended to
support flexible threading/concurrence models. See PGOCaml
documentation for more information. The casual user can use the
non-functorized values, wich are a directly-usable implementation of
the `QUERY` interface, with no threading integrated.

The core `Query` interface is quite simple :

  val query : _ Db.t -> ?log:out_channel -> 'a Sql.query -> 'a Db.monad
  val view : _ Db.t -> ?log:out_channel -> 'a Sql.view -> 'a list Db.monad
  val view_one : _ Db.t -> ?log:out_channel -> 'a Sql.view -> 'a Db.monad
  val view_opt : _ Db.t -> ?log:out_channel -> 'a Sql.view -> 'a option Db.monad

(in the simple case, `_ Db.t` is `_ PGOCaml.t`, and `'a Db.monad` is just `'a`)

The main function is `query`, wich accepts a database handler (the one
produced by `PGOCaml.connect`), the query, and returns the typed
result : in case of `SELECT`, a list of value rows, otherwise
(`UPDATE`, `INSERT`...) `unit`. An optional parameter `?log` will
output the query string in the given output channel, if any.

`view`, `view_one` and `view_opt` are given for convenience.
`view_one` expect exactly one result, and `view_opt` at most one
result; if they get more results, they will raise a `Failure` exception.

:Note:

  The query module is very loosely integrated with the rest of
  Macaque. In particular, all the typed result processing is done by
  the `Sql` modules, through the `Sql.handle_query_results`
  function. It makes the `Query` module inessential : an user willing
  to experiment other interfaces to the SQL server could easily get
  the SQL string itself (`Sql.sql_of_query`, `Sql.sql_of_view`), call
  the SQL server in her preferred way, and give back the results to
  `Sql.handle_query_results`.

  Beware however that `Sql.handle_query_results` is unsafe, as it
  strongly relies on results corresponding to the given query; giving
  incorrect results (eg. coming from another query) results in an
  undefined behaviour, even possibly a Segfault. You should really not
  try anything clever except giving the query string to a PostGreSQL
  server (in any way you like), and sending back the results to
  Macaque.



### Processing query results                               [.processing_results]

macaque produces `Sql.t` values from the query results. The real caml
values can be accessed using the `Sql.get` and `Sql.getn` operators :

  val get : < get : _; nul : non_nullable; t : 't #type_info > t -> 't
  val getn : < get : _; nul : nullable; t : 't #type_info > t -> 't option

`Sql.get` operates on non-nullable values, while `Sql.getn` operates
no nullable values and returns an `option`.

A common use case is to access fields from a row, leading to code such
as `Sql.get row#field`. The problem with this syntax is that it is not
chainable : to access field of a subrow (row inside the result row),
wich is a Sql.t value itself, one has to write code such as `Sql.get
(Sql.get row#sub_row)#field`.

To lighten the notation, a specific syntaxic sugar is integrated with
the `pa_bananas` syntax extension : `a#!b` is equivalent to `Sql.get
a#b`, and `a#?b` is equivalent to `Sql.getn a#b`. It is thus possible
to write `row#!sub_row#!field` directly.

Note that `a#?b` still returns an option, so you can't have something
like `a#?b#?c` (as `a#?b` is not an object) : you still need to do
pattern matching on the option value yourself. I could create an
unsafe constructor wich would raise an exception in the `None` case,
but that would defeat the point of macaque (as few runtime errors as
possible).  I also feels that such facilities would encourage
nullability-laziness from the user : you have to work on your database
design upfront to eradicate nullable types were possible; in the case
were a `NULL` case is a semantic necessity, you will want the explicit
option matching anyway.



#### Single value queries                                  [.single_value_query]

It is sometimes useful to query single values from the database,
instead of the usual list-of-rows SELECT query -- for example to
request the current time, or the value of a specific counter.

For this purpose, Macaque was extended with:

- A `Sql.value` function building a query from a `Sql.t` value; This
  query, once run through `Query.query`, agains return a `Sql.t`
  value, on which `Sql.get` or `Sql.getn` can be used depending on its
  nullability.

- Convenience functions in Query to get the extracted value directly
  without using Sql.get(n): `Query.value` for non-nullable value,
  and `Query.value_opt` for nullable ones.

As an example, the following three definitions of `get_time` are equivalent:

  let time = <:value< current_timestamp () >>

  let get_time dbh =
    Sql.get (Query.view_one dbh <:view< {x = $time$} >>)#x

  let get_time dbh =
    Sql.get (Query.query dbh (Sql.value time))

  let get_time dbh =
    Query.value dbh time




### General syntax : values, comprehensions, queries       [.syntax_description]

macaque use the Camlp4 quotations system to integrate Domain-specific
syntaxes into the Objective Caml language. General principles are as follows :

##### quotations

`<:foo< ... >>` is a quotation using the 'foo' syntax expander.
macaque provides 6 syntax expanders :
- 'select', 'insert', 'delete', 'update' : queries syntax expanders
- 'view' : view syntax expander
- 'value' : value syntax expander

'view' is the default syntax expander :  `<< ... >>` is equivalent to `<:view< ... >>`

##### antiquotations

Inside a quotations, `$foo$` is an antiquotation with
denotes a caml-level value :

  let value v = <:value< $v$ >> (* the identity function on values *)

antiquotations can also be named in specific context :

  let int n = <:value< $int:n$ + 0 >>
  (* function that transforms a caml int into a Sql.t value *)

named antiquotations are not hardcoded in the syntax definition :
they rely on values of the `Sql` module, wich are in the appropriate
submodule : `$foo:bar$` will expand to `Sql.Module.foo bar`, where the
`Foo` module depends on the antiquotation position (`View`, `Value`,
`Table_type`); this way, on can read the `Sql` interface to discover
named parameters and their meaning; similarly, operators are defined
in `Sql.Op`.

##### Loosely defined syntax

- camlp4 quotations symbols aren't represented
- `[foo]` means optional `foo` (one or none)
- `foo sep ...` means list of `foo`s separated by `sep` (";" or ",")

:BNF:

    select ::= view
    insert ::= table ':=' value [refinement]
    delete ::= row name 'in' table refinement
    update ::= row name 'in' table ':=' value refinement
    view ::= comprehension
           | view-op '(' view ')' '(' view ')' ...
    comprehension ::= comp-result comp-modifiers [refinement]
    refinement ::= '|' comp-items ';' ...
    comp-result ::= value
                  | 'group' tuple ['by' tuple]
    comp-modifiers ::= ['order' 'by' order ',' ...] ['limit' value] ['offset' value]
    order ::= value ['asc' | 'desc']
    comp-item ::= row-binding
                | guard
    row-binding ::= row-name 'in' view
    guard ::= boolean value
    value ::= litteral constants
            | 0-ary operator : 'null', 'current_timestamp'
            | 1-ary operator : 'nullable' value
            | infix binary operator : value '+' value
            | tuple : tuple
            | row '.' field : field-access
            | row '?' field : default value for a field
            | 'cast' value 'as' column-type
    tuple ::= '{' tuple-field ';' ... '}'
    tuple-field ::= field-name '=' value
                  | field-access
    field-access ::= value '.' field-name '.' ...
    view-op ::=
      | 'union'     | 'union_all'
      | 'intersect' | 'intersect_all'
      | 'except"    | 'except_all'
    column-type ::= identifier   (see the "Column Types" section)

:Note:

  A few remarks.

  - ";"-separated lists allows an optional ending ";", to match OCaml syntax : {a;b;}

  - 'table' denotes a place where an `'a table` type is required

  - antiquotations can be used in 'view', 'table' and 'value' places.

    You can use antiquoted expressions in all the places where
    a 'view', 'table' (view) or 'value' (including rows, but not row
    names or field names) is accepted

  - Tuples, comprehension and guard lists can be empty :

    - `<:view< $row$ | >>` is the view with only one line, wich is 'row'
    - `<:insert< $table$ := $row$ | >>` inserts a single row
    - `<:delete< row in $table$ | >>` deletes a whole table
    - `<:update< row in $table$ := $value$ | >>` updates a whole table

    In the 'view' and 'insert' cases, the ending "|" is optional. It
    is however not the case for 'update' and 'delete' : see the
    remark at the end of this section.

  - In order to simplify the common
    `{ a = foo.a; b = bar.foobar.b }` use case, tuple field names
    are optional when the value is a field access : in that case,
    the name of the accessed field is used (in case of nested
    access, eg.`'foo.bar.baz`, the rightmost name is used, here
    `baz`); the example can thus be written `{ foo.a; bar.foobar.b }`

  - if the `BY` record of a `group .. by` expression is empty, the `BY` part
    (`by {}`) is optinal. See the [GROUP BY expressions and aggregate
    functions](#group_by) section.

##### Remark : implicit exhaustive manipulation       [.exhaustive_manipulations]

It was found that user sometimes forgot the guards in an `UPDATE` or
`DELETE` query. The results of that mistake are usually rather
displeasing (deleting or modifying the whole table). In order to make
the mistake more visible and less likely to happen, the "|" before the
guards of an `UPDATE` or `DELETE` query are not optional by default :
if the user forgets the guards altogether, she will also forget the
"|" and will get an error.



### View expressions                                                     [.view]

:BNF:

    view ::= comprehension
           | view-op '(' view ')' '(' view ')' ...
    comprehension ::= comp-result comp-modifiers [refinement]
    view-op ::=
      | 'union'     | 'union_all'
      | 'intersect' | 'intersect_all'
      | 'except"    | 'except_all'

#### View results                                                [.view_results]

View results are either a simple value or a `GROUP BY` expression (see
the [GROUP BY expressions and aggregate functions](#group_by) part of
this document).

The usual return value is a SQL tuple: `{a = foo; b = bar}` will return
a view with columns named `a` and `b` and values corresponding to the
`foo` and `bar` expressions.
You can also give the name of a row : `<< t | t in .. >>`

Columns values can be tuples themselves. This is a quite
Macaque-specific feature, wich makes the language much more
homogenous. Tuple types (column or immediate records) can be nested
arbitrarily. For example, a cartesian product view :

   let cartesian_product va vb = << {a = a; b = b} | a in $va$; b in $vb$ >>

Care is taken that tuple values support all operations the plain SQL
values support : Macaque works hard to translate them to expressions
the usual SQL server can understand (tuple flattening). This part of
macaque is rather delicate and you might encounter bugs with
no-yet-well-tested combinations of nested tuples and other macaque
features.

#### View modifiers                                            [.view_modifiers]

View modifiers correspond to following SQL features : `LIMIT`,
`OFFSET` and `ORDER BY`. Their behaviour should not come as
a surprise : `ORDER BY` will sort the returned rows according to the
specified sorting criteria, `LIMIT` will restrict the number of
returned rows, and `OFFSET` will skip a given number of rows (for
example, `LIMIT 2 OFFSET 3` will return the 4th and 5th row of
a view).

`ORDER BY` syntax mimics the usual SQL syntax :
  << ... order by foo asc, bar desc, ... >>

The ordering modifier (`asc` | `desc`) is optional, `asc` will be
choosed by default. If the ordering expression `(foo, bar)` is
a record (immediate tuple or row), no guarantee is given as to the
column ordering decided (fields of a macaque record are not ordered,
so a lexicographic order cannot be chosen) : the view result will be
sorted on each column of the tuple, with the given ordering modifier.

`ORDER BY` expressions are in the comprehension items scope : they can
depend on the rows bound in the latter part of the
comprehension. `LIMIT` and `OFFSET` are not : obviously `<< foo LIMIT
foo.id | foo in $..$ >>` is not a well-formed query.

#### Comprehension items                                  [.comprehension_items]

There is one important thing to know about comprehension items \: row
bindings are not sequential, they're simultaneous (`let .. and .. and
.. in ..`) ! This is a not so happy feature, as the comprehension
syntax strongly suggest a sequential binding : there is an impedence
mismatch between the comprehension syntax and the SQL behaviour. For
further discussion, see the [Remarks > semantic of row
bindings](#row_binding_semantics) section of this manual.

Of course, guards can depend on the declared rows.

#### View operators

The usual UNION, INTERSECT and EXCEPT operators can be used in prefix
position, with parentheses around their parameters. The syntax allows
to pass arbitrary number of parameters, and a left-associativity is
applied:

  except (foo...) (bar...) (baz...)

is equivalent to

  except (except (foo...) (bar...)) (baz...)



### GROUP BY expressions and aggregate functions                     [.group_by]

The SQL query `SELECT fields GROUP BY group_fields` roughly translate
in the `group {fields} by {group_fields}` expression : after `group`
the aggregating part, and after `by` the grouping part.

The returned row are the concatenation of the `GROUP` record (`{fields}`)
and the `BY` records (`{group_fields}`).

Example :
  << group {subtotal = sum[t.a]} by {k = t.b} | t in $view$ >>
Will expand to
  SELECT SUM(t.a) AS subtotal, t.b AS k  GROUP BY t.b FROM (...) AS t

#### Empty GROUP part                                             [.empty_group]

`GROUP BY` expressions with all fields in the `BY` part are equivalent to
a `SELECT DISTINCT` query.

#### Restrictions on the GROUP record, accumulators [..] syntax  [.accumulators]

The `BY` values (here `t.b`) can be any expression depending on the bound
view (here `t`, but possibly more than one), and anything else in
scope. The group values are restricted to match the `GROUP .. BY`
semantic :
- values depending on the bound tables of the view must be inside
  "accumulators", that is square brackets :
  - `<:view< group {c = t.c} by {k = t.b} | ... >>` is illegal (and won't type),
- accumulators must be used by an aggregate function, and nowhere else :
  - `<:view< group {c = [t.c]} by {k = t.b} | ... >>` is illegal (and won't type),
  - `<:view< group {c = count[t.c]} by {k = t.b} | ... >>` is legal
- fields name bound in the `BY` record can be used freely :
  - `<:view< group {c = k; d = k + k; e = count[l]} by {k = t.b; l = t.c} | ... >>` is legal

#### Corresponding typing errors                       [.static_group_by_typing]

- Use of group-varying values outside accumulators

    Error: This expression has type Sql.grouped_row
           but an expression was expected of type
           < nul : Sql.non_nullable; t : 'a #Sql.row_t; .. > Sql.t

  Cause : use of a bound row in the `GROUP` record outside an accumulator

- Use of aggregate functions outside accumulators

    Error: This expression has type < nul : 'a; t : 'b > Sql.group Sql.unsafe
            but an expression was expected of type
            < nul : 'c; t : 'd; .. > Sql.t Sql.unsafe

  Cause : one of the field values is an accumulator expression;
  accumulators should be consumed by aggregate functions and cannot be
  returned directly


#### Non-grouped aggregates                            [.non_grouped_aggregates]

Aggregates functions *cannot* be used outside group by expressions. If
you want to use an aggregate functions over all the rows of a table,
use a group by expression with an empty BY record :

`SELECT max(t.id) FROM ...` -> `group {max = max[t.id]} by {} | ...`

In that case, `by` is optional : `group {max = max[t.id]} | ...`

Rationale : aggregate functions are not regular operators; they have
a non-trivial semantic, wich is modeled by the typing transformations
inside the group .. by expressions. Using them outside those
expressions would not be safe and could lead to runtime errors if the
SQL server doesn't accept an ill-formed query. It is a design goal of
macaque to protect from runtime errors.



### Description syntax                                           [.descriptions]

Description syntax is used to describe existing database tables, making
them as macaque views (internally, it builds a runtime description of
the data-base typing informations, to be used by further macaque
processing). The syntax is a far relative of `SQL CREATE TABLE`
statements, and can be discovered in the example base.ml
file. Currently, no other information that name, type, nullability and
default value are accepted.

  <:table< recipes (
    recipe bigint NOT NULL,
    name text,
    amount bigint NOT NULL
      DEFAULT($ <:value< 0L >> $),
    category text NOT NULL
      DEFAULT($ <:value< "unknown category" >> $)
  ) >>

This is not a table creation/specification tool : it does not free you
from the need of creating your tables in the database (with
a potentially richer description : foreign keys, etc.). If you change
a table description, you will have to duplicate the changes in the
caml description as well.

#### Auto-incrementing columns                                    [.incr_column]

`SERIAL` columns are not supported (yet). It is however possible to
emulate this feature with explicit `SEQUENCE` manipulations ([sequence
operators](#sequence_ops) section) :

  let my_table = <:table< my_sql_table (
    id bigint NOT NULL DEFAULT(nextval $table_id_seq$),
    ...
  ) >>

At insertion site, one can then use the default access syntax to get
the next identifier:

  <:insert< my_table := { id = my_table?id; ... } >>

#### sequence descriptions                              [.sequence_descriptions]

It is not possible to create new sequences from Macaque (this is
coherent with the choice that macaques allows descriptions, not
declarations), but macaque support sequence descriptions. Sequence
creation operators are part of the `Sql.Sequence` module.

  let table_id_seq = <:sequence< serial "the_sql_sequence_name" >>

#### Checking descriptions                                 [.check_descriptions]

To help you with the macaque/database synchronization, the `Check`
module provide coherence check routines (`check_table`,
`check_sequence`). It will raise errors if the caml-side description
is not faithful to the real table structure (as described by the SQL
server table informations). It is possible to enforce an automatic
runtime check of every macaque-described table or sequence with the
'-check_tables' command line option (camlp4 time), wich can be enabled
by the `use_check` ocamlbuild flag (see the [OCamlbuild](#ocamlbuild)
section of this document).



### OCamlbuild                                                      [.ocamlbuild]

I have created a macaque-specific OCamlbuild plugin. It was originally
intended to help during macaque development only, but is probably
a valuable ressource if you want to use Macaque inside your
project. Besides the classical ocamlfind integration, you'll find the
`Sql_syntax` modules with support for the following tags :

- `use_macaque` : enable the macaque syntax extensions
- `use_check` : enable the `-check_tables` flag (see the [Decription
  syntax > Checking table descriptions](#check_descriptions)
  subsection)

You should generally use the `macaque` tag, the three other
preprocessing tags being there for finer-grained control.



### Sql functions, operators and data types                     [.SQL_operators]

#### Sql value operators                                      [.value_operators]

macaque can use all function and operators defined in the `Sql.Op`
module, using the standard ocaml syntax (and operator associativities
and precedences) :
- `<:value< $a$ + $b$ >>` is equivalent to `Sql.Op.(+) a b`
- `<:value< nullable $foo$ >>` is equivalent to `Sql.Op.nullable foo`

##### aggregate function

`Sql.Op` has some aggregate functions. See the [GROUP BY expressions
and aggregate functions](#group_by) section of this document for more
information.

#### Value types antiquotations                          [.value_antiquotations]

macaque supports some SQL data types, some of them having a litteral
syntax (ints and string : `<:value< 2 >>`, `<:value< "foo" >>`). They can
all be constructed by using named antiquotations :
  <:value< $bool:true$ >>
  <:value< $float:cos 1.2$ >>

More generally, the type constructors are the values in Sql.Value :
`<:value< $foo:bar$ >>` is equivalent to `Sql.Value.foo bar`

#### Column types                                                [.column_types]

Data types used in table descriptions are defined in the
`Sql.Table_type` module. We use a different set of type names, in
order to mimic SQL type names and ease specification derivation from
existing SQL tables. For example, "integer" is used instead of "int",
and will expand to a `Sql.Table_type.integer` value.

#### Casts                                                              [.casts]

You can use an explicit cast of the form `cast foo as bar`, where
`bar` is an identifier for a column-type as described in the "Column
types" section above. This is useful in particular to work around some
limitations of PostgreSQL type inference for nulls (see the "NULL and
semantic issues" subsection): if a `null` somewhere is "too
polymorphic" for PostgreSQL, you may want to use for example `cast
null as integer` instead.

#### Sequence description

Sequence creators are defined in the `Sql.Sequence` module.

#### Sequence operators                                          [.sequence_ops]

Sql.Op has some sequence functions. Currently supported are
`currval` and `nextval`. They can be used to have an
auto-incrementing identifier :

  <:insert< $table$ := {id = nextval $table_id_seq$; .. } >>

See the src/tests/sequence.ml for example.

#### View antiquotations                                  [.view_antiquotations]

In view positions, macaque supports antiquotations through the
`Sql.View` module : `$foo:bar$` will expand to `Sql.View.foo bar`.



### phantom types                                               [.phantom_types]

Sql.t types sure are heavy. You *will* be confronted to cluttered
error messages with ugly as hell unification problems. Hopes this
section helps.

Sql values are packed in a phantom type providing several information
about the value. It is an object type with a field `t` containing type
information, and other fields for value information.

Type information fields :

- 'typ' : the corresponding caml type (eg. `<typ : int>`); `Sql.null` has
  a polymorphic `t` field, as `None` for option types

    val null : < t : < typ : 'a; numeric : 'b >; nul : nullable; get : unit > t

- 'numeric' : allows for numerical operators overloading (see `Sql.Op.(+)` type)

Value information fields :

- 'nul' field : nullability information, is either `Sql.nullable` or
  `Sql.non_nullable`. Constants from the `Sql.Data` operators have
  a polymorphic `nul` field, so that they can be used in both
  nullability context

- 'get' : just-built sql values (<:value< 1 >>, etc.) or values
  retrieved from a SQL query are gettable : you can ask for their
  corresponding caml value values using the two accessors Sql.get and
  Sql.getn (get nullable) :

       val get : < t : < typ : 'a; ..>; nul : non_nullable; get : _; .. > Sql.t -> 'a
       val getn : < t : <typ : 'a; ..>; nul : nullable; get : _; .. > Sql.t -> 'a option

  On the contrary, values built from SQL operators or field access can
  not be transformed back into caml values without being first sent to
  the SQL server as part of a query : they don't have a 'get' field.

Now, you probably understand why the error messages tends to be
slightly longer than usual. And all sql values carry such information,
including (nested) rows...



### Specific type fourberies                                  [.type_subtleties]

macaque being strongly (and intricately) typed, you will often find
yourself confronted to hostile error messages, wich means someone has
done something wrong. I hope that you will more often be wrong than
macaque. There are nonetheless some specificites that you should be
aware of.


#### Sql interface safety                                    [.interface_safety]

Macaque syntax extensions transform user code into complicated caml
code. But the produced codes still lies outside macaque module
boundaries, and has access to the same information that the user
has. Concretely, as most of the operations needed by those extensions
are not typable inside the OCaml type system (for example dynamic
construction of an object based on a list of field names and values),
some unsafe operations had to be exposed through Sql interface^α.
They are marked with the "unsafe" parametrized type (wich is only
there for documentation purposes) and you should NOT use them : every
function having an "unsafe" type somewhere in its interface is to be
considered forbidden. All the other functions should be type-safe,
otherwise it's a bug.

:Footnote:

  α: and there is some Obj magic behind the scene; but it's protected
  by typing and you won't get a segfault, I hope.

#### Update subtyping problem                                [.update_subtyping]

An example of update syntax is << t in $tab$ := {amount = t.amout + 1} >>,-
wich increment the "amout" column of all rows in table `tab`. Table
`tab` probably has more fields than just the "amount" colum, so the
type of the tuple on the right should be a subtype of `table` row
type.

The problem is that polymorphic subtyping quantification is not
available in OCaml type system : subtype relations have to be
explicitely constructed from the object types. This is fine when, as
in the given example, the right tuple object type is known as
camlp4-time (an object with only one field 'amout'), but not when the
corresponding value is an antiquotation :

        let update table value predicate =
           <:update< row in $table$ := $value row$ | $predicate row$ >>

In this case, the update syntax is used to define a generic update
operation^β : I know of no syntaxic way to impose that the right
tuple type be a subtype of the table row type.

To keep things simple, there is an ad-hoc rule for the specific update
problem : when tuple type is known as camlp4-time, a subtyping
relation is used, but when there is a quotation, the two values types
are unified : the set tuple has to have exactly the same columns as
the table, no less.

This loss of generality could surprise the innocent user. In order to
prevent hair scratching, a warning is emitted at camlp4-time when an
antiquoted caml value is used for the update tuple. This warning can
be disabled using the -sql-nowarn-undetermined-update command line
option.

Of course, it is always possible to manually expand an antiquotated
value, when the fields name are known at development-time :
  <:update< t in $table$ := $tup$ | ... >>
should be rewritten into
  <:update< t in $table$ := { foo = $tup$.foo; bar = $tup$.bar } | ... >>

:Footnote:

  β: notice how `value` and `predicate` are actually functions
  depending on `row` values; this kind of thing can greatly improve
  code factorization among your SQL queries



### Remarks                                                   [.general_remarks]

#### Side effects                                                [.side_effects]

It is probably an obvious thing to say, but users should not put
expressions wich have side-effect when evaluated inside macaque
expressions. No guarantee is given that any part of a macaque
expression will be evaluated at all, or only once.

In case of doubt, you should explicitely evaluate expressions before
handling them to the macaque expression :
  let my_val = my_expr in << ... $my_val$ ... >>

#### Semantic of row bindings                           [.row_binding_semantics]

From the [View expressions > Comprehension
items](#comprehension_items) part of this document :
%
> There is one important thing to know about comprehension items : row
> bindings are not sequential, they're simultaneous (let .. and .. and
> .. in ..) ! This is a not so happy feature, as the comprehension
> syntax strongly suggest a sequential binding : there is an impedence
> mismatch between the comprehension syntax and the SQL behaviour.

The rationale behind this choice is purely pragmatic : SQL queries use
simultaneous bindings, and reproducing that choice much facilitate the
translation. It would be possible to have sequential bindings, but
that's somewhat more complicated, and I supposed it was unnatural to
the SQL query writer anyway.

I'm not quite satisfacted with this state of affairs, so it is
possible that I replace the current behaviour with sequential
declarations in the future. Please do not write code that would break
if that was the case : `<< .. | t1 in $..$; t2 in $ .. t1 $ >>` when
`t1` is already in the scope. Such trickery tremendously hurts
readability anyway, so you should not use it even if it wasn't for
compatibility reasons.


#### Camlp4 use                                                   [.campl4_use]

I am under the impression that some "serious" ocaml users try to avoid
to include camlp4 in their compilation chain if possible, and to
minimize their reliance on camlp4 extensions. Camlp4 is
a not-so-simple (and really-not-so-well-documented) tool, and camlp4
extensions are sometimes fragile and more subject to bugs than other
pieces of ocaml software; syntaxic bugs can have far-reaching effects
(they are, however, nearly always spotted at compilation-time) and are
difficult to spot for the non-camlp4-aware user.

Specific care has been taken in Macaque to make the camlp4 processing
as solid as possible :

- camlp4 processing is split in two independent extensions
  (`pa_macaque`, `pa_bananas`) so that the user can make fine-grained
  decisions as to what extension to activate for a specific
  compilation

- `pa_macaque` relies on the "quotation" mechanism of the existing
  ocaml camlp4 grammar : they DO NOT modify the ocaml grammar but
  instead reuse the `<:..< .. >>` mechanism wich is widely used inside
  camlp4 itself, and thus can be reasonably trusted

- `pa_bananas` is the only extension modifying the OCaml grammar
  itself; it is a simple extension wich affects a localized part of
  the parsing process (the "." level of expressions : `a.b`, `a#b`..),
  with no precedence/associativity subtleties. It is a relatively
  trustable extension, and any spotted defects would be easy to fix.

  Moreover, `pa_bananas` is only a convenience extension and it is easy
  not to use it (use `Sql.get`/`Sql.getn` instead of `#!`/`#?`). If a problem
  were to arise, it would be easy for a user to mechanically replace
  all `pa_bananas` syntax uses by camlp4-free equivalent expressions,
  and drop `pa_bananas` from her compilation chain entirely.


#### Macaque and PG'OCaml                                      [.pgocaml_compat]

Macaque relies on PG'OCaml low-level interface. It is fully compatible
with PG'OCaml : you can use a database handler for Macaque queries and
PG'OCaml queries at the same time (PG'OCaml stocks some private
information in those handler; Macaque doesn't access nor modify them).

PG'OCaml is more mature and its general approach leads to a safer
software : Macaque will probably catch less errors at compile-time
than PG'OCaml (see the PostGreSQL typing issues of the next section
for an area where I believe Macaque lacks behind PG'OCaml in terms of
safety), and it is very young software wich probably have quite a few
bugs remaining.

This inconvenient comes with the flexibility Macaque brings : it is
composable, and does not need the SQL server available at compile time
(or, more exactly, camlp4-time).

I also believe that PG'OCaml prepared queries model is a bit more
efficient than Macaque raw-SQL-queries output. I have done absolutely
no work on Macaque optimisation, and my priority are rather safety and
composability. If you're interested in investigating Macaque
performances, let me know.


#### NULL and semantic issues                                  [.null_inference]

I have tried to reproduce SQL behaviour as closely as possible. In
particular, all operators are really SQL operators, so they will have
the SQL behaviour, for example `null = null` is null, not false.

I have encountered issues with PostGreSQL typing system
though. PostGreSQL gives NULL the type 'unknown', wich is problematic
because sql operators are strongly typed, but the type system is not
very expressive. For example `NULL + 0` will work okay, but :

  base=> SELECT e.n + 0 FROM (SELECT NULL AS n) AS e;
  ERROR:  failed to find conversion function from unknown to integer

I have tried to work around those issues, but think some of them are
probably lurking somewhere. It is unfortunate as it undermines Macaque
static safety : these runtime errors are subtle and quite difficult to
prevent. Please report them if you find some. I hope not.
