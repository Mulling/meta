GHCFLAGS := -Wall
GHCTARGET := Main
GHCOUT := meta

all:
	ghc --make $(GHCTARGET) -o $(GHCOUT) -outputdir ../dist/ $(GHCFLAGS)

.PHONY: release clean

release: GHCFLAGS += -O3 
release: all

runtest: GHCTARGET = Test
runtest: GHCOUT = test
runtest: all
	@echo " :: Running Tests"
	@./test

profile: GHCFLAGS += -O3 -prof -fprof-auto -rtsopts
profile: all
profile: 
	./meta +RTS -p

clean:
	rm -f test
	rm -f meta
	rm -f ../dist/*
