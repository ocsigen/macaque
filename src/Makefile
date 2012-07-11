INTERFACES= sql_base.cmi sql_types.cmi sql_parsers.cmi \
		sql_printers.cmi sql_builders.cmi sql_public.cmi \
		inner_sql.cmi \
		sql.cmi check.cmi query.cmi \

TARGETS=pa_macaque.cmo pa_bananas.cmo macaque.cma macaque.cmxa macaque.cmxs

$(TARGETS):
	ocamlbuild -tag annot $@

all: $(TARGETS)

install: all
	ocamlfind install macaque META $(addprefix "_build/",$(INTERFACES) $(TARGETS))

uninstall:
	ocamlfind remove macaque

reinstall: uninstall install

clean:
	ocamlbuild -clean
