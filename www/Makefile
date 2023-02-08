course = main

.PHONY : all clean scribble code

all: clean $(course)

$(course): scribble code

scribble:
	raco scribble --htmls \
		++style css/extra.css \
		++style css/fancyverb.css \
		++style css/recolor.css \
		++extra js/accessibility.js \
		++xref-in setup/xref load-collections-xref \
		--redirect-main http://docs.racket-lang.org/ \
		$(course).scrbl

hw:
	mkdir -p $(course)/hw/
	cd assignments/ ; \
	for f in `ls -d .*/hw*/`; do \
		git archive -o $${f%/}.zip HEAD $$f; \
		cp $${f%/}.zip ../$(course)/hw/; \
	done

code:
	mkdir -p $(course)/code/
	cd code ; \
		tar -c `git ls-files *.hs` | \
		(cd ../main/code ; tar -x)

push: scribble code hw
	rsync -avzhe ssh main/ jmct@junkfood.cs.umd.edu:/fs/www/class/spring2023/cmsc433

clean:
	rm -rf $(course)
