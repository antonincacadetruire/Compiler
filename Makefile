# Variables
OCAMLC = ocamlc
SOURCES = compiler.ml
EXECUTABLE = compiler.exe

# Rules
compiler: $(EXECUTABLE)

$(EXECUTABLE): $(SOURCES)
	$(OCAMLC) -o $@ $(SOURCES)

clean:
	rm -f *.cmo *.cmi $(EXECUTABLE)

all: clean compiler tests

tests:
	./run_tests.sh

.PHONY: all clean tests compiler