.DEFAULT_GOAL := help
.PHONY : help large-thunk-ht LargeThunk.eventlog

BROWSER=firefox

large-thunk-ht: ## LargeThunk +RTS -l -hT -i0.5 -RTS
	ghc -fforce-recomp -eventlog -rtsopts -O2 LargeThunk
	./LargeThunk 100000 100000 30000000 +RTS -l-agu -hT -i0.5 -RTS
	eventlog2html LargeThunk.eventlog
	$(BROWSER) LargeThunk.eventlog.html

large-thunk-hi: ## LargeThunk +RTS -l -hi -i0.5 -RTS
	ghc -fforce-recomp -eventlog -rtsopts -O2 -finfo-table-map -fdistinct-constructor-tables LargeThunk
	./LargeThunk 100000 100000 30000000 +RTS -l -hi -i0.5 -RTS
	eventlog2html LargeThunk.eventlog
	$(BROWSER) LargeThunk.eventlog.html

callstack1: ## HasCallStack1.hs
	ghc HasCallStack1
	./HasCallStack1

callstack2: ## HasCallStack2.hs
	ghc HasCallStack2
	./HasCallStack2

xctraces: ## XCTraces.hs
	ghc XCTraces -prof -fprof-auto -fforce-recomp
	./XCTraces +RTS -xc -RTS

clean: ## Clean temporal files
	rm -f *.hi *.hp *.o *.prof *.html *.eventlog LargeThunk HasCallStack1 HasCallStack2 XCTraces

help: ## Show this help
	@echo -e "$$(grep -hE '^\S+:.*##' $(MAKEFILE_LIST) | sed -e 's/:.*##\s*/:/' -e 's/^\(.\+\):\(.*\)/\\x1b[36m\1\\x1b[m:\2/' | column -c2 -t -s :)"
