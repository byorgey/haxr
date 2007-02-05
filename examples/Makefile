GHC = ghc
GHCFLAGS = -O2 -package haxr -fallow-overlapping-instances

TEST_PROGS = latest-xmlrpc make-stubs parse_response \
             raw_call \
	     simple_client simple_server test_client \
             test_server time-xmlrpc-com validate

.SUFFIXES: .hs .hi .o

.PHONY: all clean

default all: $(TEST_PROGS)

%: %.hs
	$(GHC) $(GHCFLAGS) --make -o $@ $<

clean:
	-rm -f *.hi *.o $(TEST_PROGS)
