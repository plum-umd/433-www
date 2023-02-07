Assignment #1 for CMSC433

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

IDE-first development
---------------------

If you have an IDE of choice you can load the files you'd like to edit
(`Main.hs`, in this case), in that IDE, and have a repl open in a terminal
window.

When you save changes to your file, you'll need to 'reload' the file in you
repl with `:r`
