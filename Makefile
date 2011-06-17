OCAMLMAKEFILE=OCamlMakefile

RESULT=run_tests
SOURCES=lwt_fiber.mli lwt_fiber.ml tests.ml
PREDS=camlp4o
PACKS=lwt lwt.unix lwt.syntax
LIBS=delimcc
ANNOTATE=true

.PHONY: all
all: nc
	@ :

include $(OCAMLMAKEFILE)
