all: macaque.cma pa_macaque.cmo pa_bananas.cmo

%.cmo:
	ocamlbuild -tag annot $@

macaque.cma:
	ocamlbuild -tag annot macaque.cma

install:
	ocamlfind install macaque META \
	_build/sql_base.cmi _build/sql_types.cmi _build/sql_parsers.cmi \
	_build/sql_printers.cmi _build/sql_builders.cmi _build/sql_public.cmi \
	_build/inner_sql.cmi \
	_build/sql.cmi _build/check.cmi _build/query.cmi \
	_build/macaque.cma \
	_build/pa_macaque.cmo _build/pa_bananas.cmo

uninstall:
	ocamlfind remove macaque

clean:
	ocamlbuild -clean
