
all:
	ocamlbuild -use-ocamlfind src/space_camels.native

clean:
	ocamlbuild -clean
