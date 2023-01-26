course = main

.PHONY : all clean scribble

all: clean $(course)

$(course): scribble

scribble:
	raco scribble --htmls \
		++style css/extra.css \
		++style css/fancyverb.css \
		++extra js/accessibility.js \
		++xref-in setup/xref load-collections-xref \
		--redirect-main http://docs.racket-lang.org/ \
		$(course).scrbl


push:
	rsync -avzhe ssh main/ jmct@junkfood.cs.umd.edu:/fs/www/class/spring2023/cmsc433

clean:
	rm -rf $(course)
