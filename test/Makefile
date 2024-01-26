
.PHONY : default clean golden alltests ompare

ROOT=$(shell cd ..; pwd)/

default : alltests # compare

clean :
	@rm -rf build/*

alltests :
	@echo == Compiling tests ==
	./agda2rust AllTests.agda -o build
	@echo == Running rustc ==
	@(cd build; rustc --crate-type=lib AllTests.rs)

compare :
	@echo == Comparing output ==
	@diff -r build golden

golden :
	@cp -r build/* golden
