
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
	./ftest.native graphs/graph2.txt 0 12 outfile
	@echo "\n==== RESULT ==== (content of outfile) \n"
	@cat outfile

image: demo
	@echo "\n==== IMAGE ====\n"
	dot -Tsvg outfile > image.svg

test_path: demo
	@echo "\n==== TEST PATH ====\n"
	@cat outfile_ff

float: 
	@echo "\n==== COMPILING ====\n"
	ocamlbuild floattest.native

	@echo "\n==== EXECUTING ====\n"
	./floattest.native graphs/float2.txt 0 12 outfile

	@echo "\n==== RESULT ==== (content of outfile) \n"
	@cat outfile

	@echo "\n==== IMAGE ====\n"
	dot -Tsvg outfile > image.svg

share:
	@echo "\n==== COMPILING ====\n"
	ocamlbuild sharingtest.native

	@echo "\n==== EXECUTING ====\n"
	./sharingtest.native moneysharing/sharing2.txt outfile

	@echo "\nCreated image."
	dot -Tsvg outfile > image.svg

clean:
	-rm -rf _build/
	-rm ftest.native
	-rm outfile
	-rm outfile_ff
	-rm *.svg
	# -rm src/*.cmo
	# -rm src/*.cmi
