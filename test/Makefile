
.PHONY : default clean golden alltests ompare

default : alltests compare

clean :
	@rm -rf build/*

alltests :
	@echo == Compiling tests ==
	./agda2rust AllTests.agda -o build
	@echo == Running rustc ==
	@(cd build; rustc AllTests.rs && ./AllTests && rm AllTests)

compare :
	@echo == Comparing output ==
	@diff -r build golden $(patsubst %,-x%.rs,Exp List Product Lambdas)

golden :
	@cp -r build/* golden
