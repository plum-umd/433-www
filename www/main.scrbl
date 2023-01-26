#lang scribble/manual
@(require scribble/core
	  scriblib/footnote
          scribble/decode
          scribble/html-properties
      	  "defns.rkt"
          "utils.rkt")

@(define (blockquote . strs)
   (make-nested-flow (make-style "blockquote" '(command))
                     (decode-flow strs)))


@(define accessible
   (style #f (list (js-addition "js/accessibility.js")
                   (attributes '((lang . "en"))))))

@title[#:style accessible @courseno]{: Programming Language Technologies and Paradigms}

@image[#:scale 1/2 #:style float-right]{img/wizard.png}

@emph{@string-titlecase[semester], @year}

@emph{Lectures: @lecture-schedule, @classroom}

@emph{Professor: @prof (@prof-pronouns)}

CMSC433 has one major aim: to demonstrate how the technologies in programming languages can shape the way you
solve a problem. Our main language of discourse will be Haskell, with some Rust examples. Concurrency will be
a common theme as we explore various language technologies.


@tabular[#:style 'boxed 
         #:row-properties '(bottom-border ())
	 (list* (list @bold{Staff} 'cont 'cont)
	        (list @bold{Name} @elem{@bold{E-mail}} @elem{@bold{Hours}})
	        (list prof prof-email "By appt.")
		staff)]

@bold{Communications:} Email

@bold{Assumptions:} This course assumes you know the material in CMSC 330 and
CMSC 216. In particular, you need to know how to program in a functional
programming language like OCaml and be confident with some systems-level
concepts like pipes and files and have a basic familiarity with Rust. See the
@seclink["Texts"]{Texts} page for references to brush up on this material.

@bold{Disclaimer:} All information on this web page is tentative and subject to
change. Any substantive change will be accompanied with an announcement to the
class via ELMS.

@include-section{syllabus.scrbl}
@include-section{texts.scrbl}
@include-section{schedule.scrbl}
@include-section{notes.scrbl}
@include-section{assignments.scrbl}
@include-section{midterms.scrbl}
@include-section{project.scrbl}
@include-section{software.scrbl}
