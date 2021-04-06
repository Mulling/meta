GHCFLAGS := -Wall
GHCTARGET := Main
GHCOUT := meta
GHCOUTDIR := ../dist/

all:
	ghc --make $(GHCTARGET) -o $(GHCOUT) -outputdir $(GHCOUTDIR) $(GHCFLAGS)

.PHONY: release clean runtest test

release: GHCFLAGS += -O3 
release: all

test: GHCTARGET := Test
test: GHCOUT := test
test: GHCOUTDIR := ../test/
test: all
	@./test

profile: GHCFLAGS += -O3 -prof -fprof-auto -rtsopts
profile: all
profile: 
	./meta +RTS -p

clean:
	rm -f test
	rm -f meta
	rm -f ../dist/*
	rm -f ../test/*
