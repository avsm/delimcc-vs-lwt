OCAMLMAKEFILE=OCamlMakefile

RESULT=res
SOURCES=lwt_fiber.mli lwt_fiber.ml chart.ml
PREDS=camlp4o
PACKS=lwt lwt.unix lwt.syntax
LIBS=delimcc
ANNOTATE=true

.PHONY: all
all: nc
	@ :

include $(OCAMLMAKEFILE)
