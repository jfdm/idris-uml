##  Makefile

IDRIS := idris
LIB   := uml
BIN   := ${LIB}bin
OPTS  :=

.PHONY: clean build

exe: install
	${IDRIS} ${OPTS} --build ${BIN}.ipkg

install: lib
	${IDRIS} ${OPTS} --install ${LIB}.ipkg

lib:
	${IDRIS} ${OPTS} --build ${LIB}.ipkg

clean:
	${IDRIS} --clean ${LIB}.ipkg
	${IDRIS} --clean ${BIN}.ipkg
	find . -name "*~" -delete

check: clobber
	${IDRIS} --checkpkg ${LIB}.ipkg

clobber : clean
	rm a.out
	find . -name "*.ibc" -delete

test :
	echo "Not yet, tests are old and broken."
	#(cd tests; bash runtests.sh)

doc:
	${IDRIS} --mkdoc ${LIB}.ipkg