Assignment #1 for CMSC433

Introduction
============

This is the first homework assignment for CMSC 433. It provides practice with
the basic built-in data structures of Haskell, including lists, tuples and
maybes, as well as recursion and pattern matching. It also covers the basics of
Haskell code style and test-driven development. If you have not read the Basics
module you should do that first.

Main.hs is a "literate" Haskell program, meaning that explanation is
interspersed with actual Haskell code. To complete your assignment, edit
Main.hs and submit it through Gradescope.

This file starts by first declaring that we are creating a module
called Main and are using functions defined in the modules Prelude,
Test.HUnit, Data.List and Data.Char.

The Prelude line imports all except for the functions listed (which
you will write). The module Prelude is special in that it is always
imported by default, so the the point of this line is not to import
more functions, but rather to exclude a few functions. (Haskell does
not allow functions to be redefined in the same module.)

The Test.HUnit line imports all functions defined in that module. The
line Data.List imports all functions from that module, but makes them
available with qualified names, such as List.intersperse, etc.


Recommended Workflow
====================

Everyone has their own workflow when programming. This course does not
enforce a particular coding workflow. With that said, the following are
some tried-and-true approaches to working with Haskell code. If you'd
like to add to this list, please email jmct@umd.edu.

REPL-first development
----------------------

You can ensure that all the necessary libraries are installed, and launch a repl
with those libraries by running `cabal repl`, as follows

```{shell}
$ cabal repl .
```

This will install the libraries as specified in `hw01.cabal` and launch the repl.
At the interactive prompt, you can check the types of various functions with `:t`,
or edit `Main.hs` with `:e`.

If at any point you want to execute the `main` function, you can use `:main`

IDE-first development
---------------------

If you have an IDE of choice you can load the files you'd like to edit
(`Main.hs`, in this case), in that IDE, and have a repl open in a terminal
window.

When you save changes to your file, you'll need to 'reload' the file in you
repl with `:r`

Compiling the executable
------------------------

Regardless of how you develop your code, you may want to actually compile
your solution into an executable. You can do this with the `cabal` command:

```{shell}
$ cabal build
```

The above will build the executable and link all of the necessary libraries.

To run the executable you can run `cabal run`.
