all: compile

compile:
	dune build src/main.exe && mkdir -p bin && cp _build/default/src/main.exe bin/techelson

test: compile
	for file in `find tests/thezos -iname "*.tz"` ; do \
		./_build/default/src/main.exe --contract $$file &> /dev/null ; \
		if [ "$$?" -ne "0" ] ; then \
			./_build/default/src/main.exe --contract $$file ; \
			echo "Failure on file '$$file'..." ; \
			exit 2 ; \
		fi \
	done
	for file in `find tests -iname "*.liq.tz"` ; do \
		./_build/default/src/main.exe --contract $$file &> /dev/null ; \
		if [ "$$?" -ne "0" ] ; then \
			./_build/default/src/main.exe --contract $$file ; \
			echo "Failure on file '$$file'..." ; \
			exit 2 ; \
		fi \
	done

test1 : compile
	./bin/techelson -vvvvvv --contract rsc/test1/test1.liq.tz --step off -- rsc/test1/test1.liq.tz.tst

test1_step : compile
	./bin/techelson -vvvvvv --contract rsc/test1/test1.liq.tz --step on -- rsc/test1/test1.liq.tz.tst

clean:
	dune clean
