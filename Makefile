DIRS = . src
OBJS = *.hi *.o .*.swp .*.swo
MAIN = REPL
EXE  = scheme

all:
	ghc --make -isrc -o $(EXE) src/$(MAIN).hs

clean:
	for d in $(DIRS); do (cd $$d; rm -rf $(EXE) $(OBJS)); done
