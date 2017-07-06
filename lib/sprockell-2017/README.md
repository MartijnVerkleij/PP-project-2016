# Sprockell
Sprockell is a **S**imple **Proc**essor in Has**kell**. It was originally written by Jan Kuper at the University of Twente. It has later been extended to allow multiple Sprockells to be run at once, communicating via shared memory.

# Features
* Simple arithmetic
* Branches / jumps
* Stack
* Local memory
* Shared memory
* IO system for numbers and string
* customizable debugger

# Documentation
The instructions are documented in [HardwareTypes.hs].

## Examples
There are a number of demo programs showing various features.
* [DemoFib.hs] shows the IO system for numeric value
* [DemoCharIO.hs] shows the IO system for characters and strings.
  It also demonstrates the use of the debugger.
* [DemoMultipleSprockells.hs]
 shows communication between multiple Sprockell cores using the shared memory

## Installing
You can install the Sprockell system as a Haskell package.
That way you can separate your (generated) code from the code of the Sprockell system.
Also the system gets compiled to native machine code, and runs a lot faster. (To see the difference, try running DemoMandelbrot in and outside of the repository.)

Installing can be done using the following commands:
```shell
# clone the repository (if you haven't done so already)
git clone https://github.com/leonschoorl/sprockell.git

# go into the directory
cd sprockell

# ask cabal to install the package from the current directory
cabal install
```

When you decide to make changes to the Sprockell system don't forget to reinstall it.

[HardwareTypes.hs]: src/Sprockell/HardwareTypes.hs#L115
[DemoFib.hs]: src/DemoFib.hs
[DemoCharIO.hs]: src/DemoCharIO.hs
[DemoMultipleSprockells.hs]: src/DemoMultipleSprockells.hs
