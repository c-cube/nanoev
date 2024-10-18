
DUNE_OPTS?=
build:
	dune build @install $(DUNE_OPTS)

clean:
	@dune clean

test:
	@dune runtest $(DUNE_OPTS)

test-autopromote:
	@dune runtest $(DUNE_OPTS) --auto-promote

doc:
	@dune build $(DUNE_OPTS) @doc

format:
	@dune build $(DUNE_OPTS) @fmt --auto-promote

format-check:
	@dune build $(DUNE_OPTS) @fmt --display=quiet

WATCH?= @check @runtest
watch:
	dune build $(DUNE_OPTS) -w $(WATCH)
