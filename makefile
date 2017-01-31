all: lab1

lab1: lab1.ml
	ocamlbuild lab1.byte
	
clean:
	rm -rf _build *.byte
