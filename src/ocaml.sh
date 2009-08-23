OBJS="sql_base.cmo sql_internals.cmo sql_parsers.cmo sql_printers.cmo \
      sql_flatten.cmo sql_types.cmo sql_builders.cmo sql_public.cmo \
      inner_sql.cmo sql.cmo query.cmo check.cmo \
      pa_comp.cmo pa_descr.cmo pa_bananas.cmo"

ocamlbuild $OBJS

cat > init.ml.tmp <<-EOML
	#use "topfind";;
	#require "pgocaml";;
	#camlp4o;;
EOML

for f in $OBJS
do
    echo \#load \"_build/$f\"\;\; >> init.ml.tmp
done

echo "open Sql;;" >> init.ml.tmp

rlwrap ocaml -I _build -init init.ml.tmp
rm init.ml.tmp
