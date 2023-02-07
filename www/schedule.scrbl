#lang scribble/manual
@(require scribble/core racket/list)
@(require "defns.rkt")

@title[#:style 'unnumbered]{Schedule}

@(define (wk d) (nonbreaking (bold d)))

@; for unreleased assignments, switch to seclink when ready to release
@(define (tbaseclink lnk txt) txt)

@tabular[#:style 'boxed
         #:sep @hspace[1] 
         #:row-properties '(bottom-border)
         (list (list @bold{Week} @bold{Due} @bold{Tuesday} @bold{Thursday})
               (list @wk{1/23}
	       	     ""
               ""
                     @itemlist[@item{Intro}
                               @item{@secref["Basics"] }])

               (list @wk{1/30}
	       	     ""
                     @itemlist[@item{@secref["DList"]}]
                     @itemlist[@item{@secref["DList"]}
                               @item{Quiz 1}]
                     )

               (list @wk{2/6}
	       	     ""
                     @itemlist[@item{@secref["HigherOrder"]}
                               @item{@link["https://forms.gle/17jmnscb7VZEPmP69"]{Study Group form}}]
                     @elem{})

               (list @wk{2/13}
	             ""
                     @elem{}
                     @elem{})
                                    
               (list @wk{2/20}
	             ""
                     @elem{}
                     @elem{})
               
               (list @wk{2/27}
	             ""
                     @elem{}
                     @elem{})
               
               (list @wk{3/6}
	             ""
                     @elem{}
                     @elem{})

               (list @wk{3/13}
	             ""
                     @elem{}
                     @elem{})
		                                   
               (list @wk{3/20}
               ""
                     @elem{}
                     @elem{})
               
               (list @wk{3/27}
      	       ""
                     @elem{}
                     @elem{})
                                    
               (list @wk{4/3}
	             ""
                     @elem{}
                     @elem{})

               (list @wk{4/10}
               ""
                     @elem{}
                     @elem{})

               (list @wk{4/17}
               ""
                     @elem{}
                     @elem{})

               (list @wk{4/24}
               ""
                     @elem{}
                     @elem{})
               
               (list @wk{5/1}
               ""
                     @elem{}
                     @elem{})

               (list @wk{5/6}
               ""
                     @elem{}
                     @elem{})

               )]


@bold{Final project assessment: @|final-date|.}
