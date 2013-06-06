.PHONY: all debug native
OCAMLBUILD = ocamlbuild -use-ocamlfind -I src

all: debug native

debug:
	$(OCAMLBUILD) debug.otarget

native:
	$(OCAMLBUILD) native.otarget
