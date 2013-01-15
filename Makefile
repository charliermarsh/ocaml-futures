all: future

future: future.ml
	ocamlc -thread -o future unix.cma threads.cma future.mli future.ml