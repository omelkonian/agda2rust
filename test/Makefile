MAIN=AllTests
.PHONY : default rust html compare test golden clean cleanHtml

default : clean cleanHtml build/$(MAIN).rs compare test

rust : build/$(MAIN).rs

build/$(MAIN).rs : *.agda
	@echo == Compiling tests ==
	./agda2rust $(MAIN).agda -o build

html : html/Agda.css html/index.html

html/Agda.css : Agda.css
	mkdir -p html/
	cp -f $< $@

html/index.html : html/$(MAIN).html
	mkdir -p html/
	cp -f $< $@

html/$(MAIN).html : *.agda build/$(MAIN).rs html/Agda.css
	@echo == Generating HTML ==
	./agda2rust -d --html --css=Agda.css $(MAIN).agda
	./renderTranslations.sh

compare :
	@echo == Comparing output ==
	@diff -r build golden

test :
	@echo == Running rustc ==
	@(cd build; rustc -A uncommon_codepoints $(MAIN).rs && ./$(MAIN) && rm $(MAIN))

golden :
	@rm -rf golden/ && mkdir golden
	@find build/ -name *.rs -exec cp --parents {} golden/ \;
	@mv -f golden/build/* golden/

clean :
	@rm -rf build/*

cleanHtml :
	 @rm -rf html/*
	 @rm -rf build/**.html build/**.md


