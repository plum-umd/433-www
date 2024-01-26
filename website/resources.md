---
title: Resources
---

You should aim to get working Dafny and Haskell installations as soon
as possible. That will be the focus of Assignment 0.

# Dafny Resources

[Dafny](https://dafny.org/) is a "verification-aware" object-oriented
programming language that we will be using in this class.  This page
contains installation information for both the Dafny tools and
interactive-development-environment (IDE) support, links to resources
you may find useful, and explanations for how to run Dafny once it is
installed.

 
## Installation
([VS Code extension (IDE)](https://marketplace.visualstudio.com/items?itemName=dafny-lang.ide-vscode) | [Official Installation Instructions](https://dafny.org/dafny/Installation))

* We recommend using VS Code as your IDE. 
* (.NET SDK) Dafny and VS Code require .NET SDK 6.0 to work properly. You will need to install the SDK first.
    - (Windows) Follow [this link](https://dotnet.microsoft.com/en-us/download/dotnet/6.0), download the Installer under SDK 6.0.*** (the last part of the version number doesn't matter) for your architecture, and run the installer. 
    - (Linux) In general, you need to install both dotnet-runtime and aspnetcore-runtime from your package manager or through the same link as the Windows one.
    - (Ubuntu) sudo apt install  dotnet-runtime-6.0 aspnetcore-runtime-6.0
      WARNING: Do not install Dafny from apt.
    - (Arch) sudo pacman -S dotnet-runtime-6.0 aspnet-runtime-6.0
    - (MacOS) There are two alternatives to doing this
        + (Without Brew) Same as Windows.
        + (With Brew) brew install dotnet@6 Installing in this way, you need to export the PATH variable properly for it to work. Please refer to the instructions in its brew info page. We don't recommend you install Dafny from `brew` unless you know how to configure the path
* (Dafny) If you use VS Code as your IDE, then the Dafny extension to VS Code can install Dafny for you.  Specifically, install the Dafny extension in VS Code.  Then, the first time you open a Dafny file in VS Code, you will be prompted about installing the latest version of Dafny; agree to this, and Dafny will be installed automatically in your home directory (specific location depends on your operating system). There is also a [demo](https://dafny.org/dafny/Installation#Visual-Studio-Code) that navigates you through this.
* If you want to use another IDE/editor, you will need to install the Dafny binaries explicitly; follow the [Dafny binaries](https://dafny.org/dafny/Installation#windows-binary) link to do this. We don't recommend this approach unless you understand your OS well.  You will then need to configure your IDE to access the files that you installed.  The link above also contains information on how to do this in Emacs.
* To test your installation, load file hello-world.dfy into your IDE and execute it.
* Troubleshooting
    - Q: My Dafny Language Server crashes, and VS Code cannot proceed! Or, I'm prompted a missing "libhostpolicy".
    - A: If you follow the instructions, you don't need to make any configurations in VS Code. If you have made some configurations, please try erasing them and retrying.
    - Q: VS Code shows that I don't have a debuggable version of the extension when pressing "F5".
    - A: Click the editor window first and try again.
    - Q: My Dafny says that the file I supplied is not a Dafny file.
    - A: It's most likely that you have an out-dated Dafny installation. Please remove the Dafny package you installed and install it either through VS Code or using the latest build from the Github release page.

 
## Other Resources

* [Getting Started (Tutorial)](https://dafny.org/latest/OnlineTutorial/guide)
* [Quick Reference](https://dafny.org/latest/QuickReference)
* [Language Reference Manual](https://dafny.org/latest/DafnyRef/DafnyRef)
* [Style Guide](https://dafny.org/latest/StyleGuide/Style-Guide)
* [Other](https://dafny.org/latest/toc)

## Running Dafny

* Dafny file names should end in ".dfy".
* From VS Code.  If you use the VS Code extension then the Dafny verifier runs continuously in the background.  If you wish to execute your Dafny program, you can do one of the following:
    - Press function key F5
    - Use Command Palette and search Dafny: Run
    - Right-click the editor window and find Dafny > Run
    - WARNING:  on Windows, if the path to your Dafny installation contains a space (i.e. " ") then the technique could not work, due to a bug in the extension.
    - From the command line.  You can also execute the Dafny verifier and execution engine from the command line.  In your binary installation locate the file "Dafny.exe."  Executing "Dafny.exe verify <file.dfy>" from either Windows Powershell or the Windows System for Linux (WSL) invokes the verifier on the given file.  Executing "Dafny.exe run <file.dfy>" invokes the verifier, then executes the program.  If you want to run Dafny.exe from a directory other than where it is installed, you will need to adjust your Path variable (details depend on your operating system).

# Haskell Resources

## Style Guide

You should take a look and follow the [style
guide](https://www.cs.umd.edu/class/spring2024/cmsc433/style.html).

## Install Stack

Stack is a build tool for Haskell that helps achieve reproducible
builds, by managing different versions of GHC (the de-facto standard
Haskell compiler) and any external packages that are needed to build
your code.

To install Stack, you can find the full instructions
[here](https://docs.haskellstack.org/en/stable/README/). In short:

- For unix based systems you can just run:

    curl -sSL https://get.haskellstack.org/ | sh

- For Windows users, I **strongly** recommend using WSL2. 

## Install the IDE of your choice.

You're free to use whatever IDE you prefer.

That said, I personally use emacs with [Haskell
mode](https://github.com/haskell/haskell-mode). Another popular choice
is [VSCode](https://code.visualstudio.com/).

You should now be able to play around!
If you run:

    stack new <project-name>

you can play around by editing files in the app and src folders.

## Library Reference

- [Hoogle](https://hoogle.haskell.org/) is your friend! It searches many
  Haskell libraries by name or approximate type signature!

- [Prelude](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html)
  is the set of functions loaded by default in every Haskell file.

## Further Reading

- [Real World Haskell](http://book.realworldhaskell.org/) is an amazing
  resource for getting into low-level details of using Haskell effectively
  in "real world" projects.

- [Algorithm Design with Haskell](https://www.youtube.com/watch?v=JJv74IJUp4E)
  is a new book focusing on writing algorithms with Haskell.






