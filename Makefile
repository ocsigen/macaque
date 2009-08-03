all: macaque.cma pa_comp.cmo pa_descr.cmo

%.cmo:
	ocamlbuild $<

macaque.cma:
	ocamlbuild macaque.cma 

install:
	ocamlfind install macaque META _build/macaque.cma \
	_build/sql.cmi _build/query.cmi \
	_build/check.cmi _build/inner_sql.cmi \
	_build/pa_comp.cmo _build/pa_descr.cmo

uninstall:
	ocamlfind remove macaque