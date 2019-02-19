all: compile

.SILENT:

compile:
	dune build src/main.exe && mkdir -p bin && cp _build/default/src/main.exe bin/techelson

bin-test: compile
	./rsc/test.sh

doc-test: compile
	(cd user_doc ; ./test.sh)

test: bin-test doc-test

clean:
	dune clean
