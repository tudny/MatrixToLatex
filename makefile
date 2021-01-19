
MAIN = test
TAGS = debug

run : native
	@echo "Executing..."
	@./${MAIN}.native

native :
	@echo "Building native..."
	@ocamlbuild ${MAIN}.native -tags ${TAGS}

debug : native
	@echo "Debug..."
	@OCAMLRUNPARAM=b ./${MAIN}.native

clean :
	@ocamlbuild -clean
