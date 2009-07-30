MLI=`echo $1 | sed s/\.ml/.inferred.mli/`
ocamlbuild $MLI && cat _build/$MLI
