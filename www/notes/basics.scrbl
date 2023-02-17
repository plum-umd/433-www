#lang scribble/manual

@(require (for-label (except-in racket compile)))
@(require scribble/examples
	  redex/reduction-semantics	  
          redex/pict
	  (only-in pict scale)
	  (only-in racket system)
	  "../fancyverb.rkt"
	  "../utils.rkt"
	  "utils.rkt"
	  "ev.rkt")

@(define codeblock-include (make-codeblock-include #'here))

@(define (shellbox . s)
   (parameterize ([current-directory (build-path notes "abscond")])
     (filebox (emph "shell")
              (fancyverbatim "fish" (apply shell s)))))

@(require (for-syntax "../utils.rkt" racket/base "utils.rkt"))
@(define-syntax (shell-expand stx)
   (syntax-case stx ()
     [(_ s ...)
      (parameterize ([current-directory (build-path notes "abscond")])
        (begin (apply shell (syntax->datum #'(s ...)))
	       #'(void)))]))

@title[#:tag "Basics"]{Getting Started with Haskell}

@section{Overview}

@filebox-include[fancy-hask "../code/Basics.hs"]
