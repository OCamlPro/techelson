all: compile

.SILENT:

compile:
	dune build src/main.exe && mkdir -p bin && cp _build/default/src/main.exe bin/techelson

test: compile
	./rsc/test.sh

doc-test: compile
	./user_doc/test.sh

clean:
	dune clean
