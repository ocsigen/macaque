opam pin add --no-action macaque .
opam install --deps-only macaque
opam install --verbose macaque

do_build_doc () {
  # Nothing yet...
}

do_remove () {
  opam remove --verbose macaque
}
