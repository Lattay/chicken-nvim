PREFIX=
CSC=
ifeq ($(CSC),)
ifeq ($(PREFIX),)
CSC=csc
else
CSC=$(PREFIX)/bin/csc
endif
endif

CSI=
ifeq ($(CSI),)
ifeq ($(PREFIX),)
CSI=csi
else
CSI=$(PREFIX)/bin/csi
endif
endif

CSC_OPTIONS=

.PHONY: all unit test integration clean

all: nvim.so

install: all
	chicken-install
	make clean

help:
	@echo "Usage: make [PREFIX=<chicken installation prefix>] [CSC=<csc command name>] <target>"
	@echo "Available target:"
	@echo "  Test targets:"
	@echo "    test                   proceed to all tests"
	@echo "    unit                   unit testing of server and client"
	@echo "    integration            integration testing of server and client (depends on Python components)"
	@echo ""
	@echo "  Lib targets:"
	@echo ""
	@echo "  Other targets:"
	@echo "    all                    compile all libs"
	@echo "    src/nvim.scm           generate the module by interogating neovim"
	@echo "    clean                  remove every build product"

# Development test

test: integration unit

unit:
	@echo "Nothing yet"

integration: test/tests.scm nvim.so
	$(CSC) $(CSC_OPTIONS) $< -o run-test
	./run-test

src/nvim.scm:
	$(CSI)

nvim.so: src/nvim.scm
	$(CSC) $(CSC_OPTIONS) -s -j nvim -o $@ $<
	$(CSC) $(CSC_OPTIONS) nvim.import.scm -dynamic

clean:
	rm -f test/*.o *.o run-test unit-* *.c test/*.c *.so *.import.scm src/*.c src/*.so
	rm -f *.*.sh *.link
