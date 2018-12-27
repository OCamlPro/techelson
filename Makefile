all: compile

compile:
	dune build src/main.exe

test: compile
	for file in `find tests -iname "*.liq.tz"` ; do \
		./_build/default/src/main.exe --contract $$file &> /dev/null ; \
		if [ "$$?" -ne "0" ] ; then \
			./_build/default/src/main.exe --contract $$file ; \
			echo "Failure on file '$$file'..." ; \
			exit 2 ; \
		fi \
	done