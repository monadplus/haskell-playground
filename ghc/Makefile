.DEFAULT_GOAL := help
.PHONY : help large-thunk-ht LargeThunk.eventlog

BROWSER=firefox

a-no-worker-wrapper: A.hs ## ghc -c A.hs without worker-wrapper transform
	ghc -c -fforce-recomp -ddump-to-file -dno-typeable-binds -dsuppress-coercions -dsuppress-module-prefixes  -dsuppress-type-applications -ddump-simpl -ddump-stg-final -O -fno-worker-wrapper A.hs

a-worker-wrapper: A.hs ## ghc -c A.hs
	ghc -c -fforce-recomp -ddump-to-file -dno-typeable-binds -dsuppress-coercions -dsuppress-module-prefixes  -dsuppress-type-applications -ddump-simpl -ddump-stg-final -O A.hs

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

gdb-example: ## gdb.hs
	ghc -g -rtsopts gdb-example.hs
	gdb --args ./gdb-example +RTS -V0 -RTS # -V0 disable RTS' periodic timer
	# b gdb-example.hs:4

clean: ## Clean temporal files
	rm -f *.hi *.hp *.o *.prof *.html *.eventlog LargeThunk HasCallStack1 HasCallStack2 XCTraces gdb-example

help: ## Show this help
	@echo -e "$$(grep -hE '^\S+:.*##' $(MAKEFILE_LIST) | sed -e 's/:.*##\s*/:/' -e 's/^\(.\+\):\(.*\)/\\x1b[36m\1\\x1b[m:\2/' | column -c2 -t -s :)"
