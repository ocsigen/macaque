BYTE=`echo $1 | sed s/\.ml/.byte/`
ocamlbuild $BYTE && ./_build/$BYTE
