all: main_future

main_future: main_future.ml future.ml
	ocamlc -thread -o main_future unix.cma threads.cma future.mli future.ml main_future.ml

clean:
	rm -f main_future *.cm*