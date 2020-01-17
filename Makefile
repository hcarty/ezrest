.PHONY: all repl test clean fmt
all:
	dune build

repl:
	dune utop src

test:
	dune runtest

clean:
	dune clean

fmt:
	dune build @fmt --auto-promote
