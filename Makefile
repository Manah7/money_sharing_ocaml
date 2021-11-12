
build:
	@echo "\n==== COMPILING ====\n"
	ocamlbuild ftest.native

symbols: 
	@echo "\n==== SYMBOLS ====\n"
	cd src/ && ocamlc -c graph.mli graph.ml gfile.mli gfile.ml && cd ..

format:
	ocp-indent --inplace src/*

edit:
	code . -n

demo: build
	@echo "\n==== EXECUTING ====\n"
	./ftest.native graphs/graph1 1 2 outfile
	@echo "\n==== RESULT ==== (content of outfile) \n"
	@cat outfile

clean:
	-rm -rf _build/
	-rm ftest.native
	-rm src/*.cmo
	-rm src/*.cmi
