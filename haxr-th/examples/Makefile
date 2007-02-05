GHC = ghc
GHCFLAGS = -O2 -package haxr-th -fallow-overlapping-instances -fth

TEST_PROGS = person_server person_client

.SUFFIXES: .hs .hi .o

.PHONY: all clean

default all: $(TEST_PROGS)

%: %.hs
	$(GHC) $(GHCFLAGS) --make -o $@ $<

clean:
	-rm -f *.hi *.o $(TEST_PROGS)
