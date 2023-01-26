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



clean:
	rm -rf $(course)
