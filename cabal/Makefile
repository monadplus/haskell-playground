.DEFAULT_GOAL := help
.PHONY : help clean

pointerChasing: ## bench/PointerChasing
	cabal configure --disable-documentation --enable-library-profiling
	cabal bench pointerChasing --benchmark-options='+RTS -hy -l-agu -i0.0001 -RTS'
	eventlog2html pointerChasing.eventlog
	xdg-open pointerChasing.eventlog.html

clean: ## Clean temporal files
	rm -f *.hi *.hp *.o *.html *.eventlog *~

help: ## Show this help
	@echo -e "$$(grep -hE '^\S+:.*##' $(MAKEFILE_LIST) | sed -e 's/:.*##\s*/:/' -e 's/^\(.\+\):\(.*\)/\\x1b[36m\1\\x1b[m:\2/' | column -c2 -t -s :)"
