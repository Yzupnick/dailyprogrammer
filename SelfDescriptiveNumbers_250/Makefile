all: SelfDescriptiveNumbers.native

SelfDescriptiveNumbers.native: SelfDescriptiveNumbers.ml
		ocamlbuild -r -use-ocamlfind -pkg core -tag thread SelfDescriptiveNumbers.native

clean:
		ocamlbuild -clean

debug:
		ocamlbuild -r -use-ocamlfind -pkg core -tag thread -tag debug SelfDescriptiveNumbers.byte
