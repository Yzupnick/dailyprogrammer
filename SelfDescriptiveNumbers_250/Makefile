all: evolutionary_algorithm.native

evolutionary_algorithm.native: evolutionary_algorithm.ml
		ocamlbuild -r -use-ocamlfind -pkg core -tag thread evolutionary_algorithm.native

clean:
		ocamlbuild -clean
