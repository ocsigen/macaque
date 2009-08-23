all: macaque.cma pa_comp.cmo pa_descr.cmo pa_bananas.cmo

%.cmo:
	ocamlbuild $<

macaque.cma:
	ocamlbuild macaque.cma 

install:
	ocamlfind install macaque META \
	_build/sql_base.cmi _build/sql_types.cmi _build/sql_parsers.cmi \
	_build/sql_printers.cmi _build/sql_builders.cmi _build/sql_public.cmi \
	_build/inner_sql.cmi \
	_build/sql.cmi _build/check.cmi _build/query.cmi \
	_build/macaque.cma \
	_build/pa_comp.cmo _build/pa_descr.cmo _build/pa_bananas.cmo

uninstall:
	ocamlfind remove macaque
