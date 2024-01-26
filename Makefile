.PHONY : install repl rustp test golden

install :
	cabal install --overwrite-policy=always

repl :
	cabal repl agda2rust # e.g. `:set args -itest -otest/build test/AllTests.agda ... main ... :r ... main`

rustp :
	cabal repl rustp # e.g. `:rustp Input`

test :
	cabal install agda2rust --overwrite-policy=always --installdir=test --install-method=copy
	make -C test

golden :
	make -C test golden
