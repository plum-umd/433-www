#lang scribble/manual
@(require scribble/core racket/list)
@(require (for-label racket))
@(require redex/reduction-semantics
          redex/pict (only-in pict scale))

@(require scribble/examples racket/sandbox)

@(require "defns.rkt")

@(define core-racket
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator 'racket/base)))

@(core-racket '(require racket/match))

@(define-syntax-rule (ex e ...) (examples #:eval core-racket #:label #f e ...))



@(define-syntax-rule (render-grammar L)
   (scale (render-language L) 1))

@(define-syntax-rule (render-grammar/nts L nts)
   (scale (render-language L #:nts nts) 1))



@title[#:style 'unnumbered]{Software}

This course will make use of the following software:

@itemlist[

 @item{Operating system: Linux and MacOS will work right out of the box. Windows users will require WSL2.}

 @item{GHC and Cabal: the Haskell implementation and a package manager for Haskell code. }

]

Instruction for using each system are below:

@itemlist[
@item{@secref{Unix-like}}
@item{@secref{Windows}}
]


@section[#:tag "Unix-like"]{Using Unix-like OSs (Mac and Linux)}


You should be able to use @link["https://www.haskell.org/ghcup/install"]{GHC-Up}, which is a tool for installing the Haskell compiler and it's associated tools.

For this course we will be using GHC version 9.0.2, but any version 9.* version should work.

@link["https://www.haskell.org/ghcup/guide"]{This link} explains how to install
a specific version of GHC.

@section[#:tag "Windows"]{Using Windows}

It is possible to use GHC without WSL, but unless you're confident with Windows
development and powershell, I would use WSL2. If you are confident, you can
read the @link["https://www.haskell.org/ghcup/guide"]{GHC-Up} guide for how to
use it in Windows Powershell.

For Windows users, using WSL for testing is highly recommended. Beyond 
the first few assignments, the projects will require generating and 
executing assembly code using the nasm package. Students in the past 
have had trouble trying to configure this in the Windows environment, 
so an easier workaround is simply to enable WSL and run your tests through 
some Linux Distribution. Here is a breakdown of the steps:

@itemlist[
 #:style 'ordered
 @item{Following the instructions at
  @link["https://docs.microsoft.com/en-us/windows/wsl/install-win10"]{
   this link}, install a Linux Distro of your choice (e.g.,
  Ubuntu). The instructions include a suggestion to upgrade to
  WSL2; this is not necessary but will improve efficiency in
  general.}

 @item{Open your installed Linux distribution of choice and
  make any initial configurations necessary (user, pass,
  etc.). Run @tt{sudo apt update} and follow with @tt{sudo apt
   upgrade}. These two may take some time. }

 @item{Follow the instructions for the @secref{Unix-like} section in order to install GHC}
]

Regardless of the IDE used, you can now run your tests from your Linux 
subsystem by entering the project directory and using GHC + Cabal.
