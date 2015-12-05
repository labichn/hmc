.PHONY = all,debug,clean

all: main.byte

main.byte: main.ml 
	ocamlbuild -use-ocamlfind -pkgs ocamlgraph,ppx_deriving main.byte

clean:
	ocamlbuild -clean
	rm -rf \#*\# *\~ *.cm* *.out *.native *.byte *.opt _build parser.ml parser.mli lexer.ml
