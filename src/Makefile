GHCFLAGS := -Wall

all:
	ghc --make Main -o meta -outputdir ../dist/ $(GHCFLAGS)

.PHONY: release clean

release: GHCFLAGS += -O3 
release: all

profile: GHCFLAGS += -O3 -prof -fprof-auto -rtsopts
profile: all
profile: 
	./meta +RTS -p

clean:
	rm meta
	rm ../dist/*
