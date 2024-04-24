.PHONY : build install repl rustp test golden testRust testClean html cleanHtml

default : build

build :
	cabal build agda2rust

install :
	cabal install agda2rust --overwrite-policy=always

repl :
	cabal repl agda2rust # e.g. `:set args -itest -otest/build test/AllTests.agda ... main ... :r ... main`

rustp :
	cabal repl rustp # e.g. `:rustp Input`

test/agda2rust : src/*.hs
	cabal install agda2rust:agda2rust agda2rust:reUnicode \
		--overwrite-policy=always --installdir=test --install-method=copy

test : test/agda2rust
	make -C test

golden :
	make -C test golden

testRust :
	make -C test rust

testRun :
	make -C test test

testClean :
	make -C test clean

html : test/agda2rust
	make -C test html

cleanHtml :
	make -C test cleanHtml
