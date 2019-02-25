all: compile

.SILENT:

compile:
	dune build src/main.exe && mkdir -p bin && cp _build/default/src/main.exe bin/techelson

bin-test: compile
	./rsc/test.sh

doc-test: compile
	(cd user_doc ; ./test.sh)

user-doc:
	(cd user_doc ; mdbook build)
	mkdir -p docs/user_doc
	rsync -a --delete user_doc/book/html/* docs/user_doc

test: bin-test doc-test

doc: user-doc

clean:
	dune clean
