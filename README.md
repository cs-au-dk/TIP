# TIP

TIP is a tiny imperative programming language aimed at teaching the
fundamental concepts of static program analysis. This code accompanies the
lecture notes on [Static Program Analysis](http://cs.au.dk/~amoeller/spa/).

[![Build Status](https://travis-ci.org/cs-au-dk/TIP.svg)](https://travis-ci.org/cs-au-dk/TIP)

## Getting started

Prerequisites:
- Java 8
- [Scala 2.11](http://www.scala-lang.org/download/)
- [SBT](http://www.scala-sbt.org/)

We suggest you to use IntelliJ for working on TIP, but all the following
options are viable.

### IntelliJ IDEA

- Install the Scala and the SBT plugins in IntelliJ.
- Clone this repository (File -> New -> Project from Version Control -> GitHub).
  **Important: due to a bug in the SBT plugin, it is important that you choose a directory name without uppercase letters, e.g. `tip` (otherwise, the following step will fail).**
- IntelliJ should then detect an SBT project. Click 'Import SBT project' and follow the instructions.
  (If this is your first Scala project, you will need to setup the Scala SDK.)
- Right-click on `Tip.scala` in `src/tip`, then select `Run 'Tip'`. To supply
  arguments, use `Edit Configurations` in the `Run` menu.
  
If you clone the repository using git instead of IntelliJ, you will need to import the project from the SBT model 
(File -> New -> Project from Existing Sources). 
Since the `.idea` folder is then regenerated from scrach, in order to keep the
inspection profiles you need to checkout the `.idea/inspectionProfiles`
folder and the `.idea/codeStyleSettings.xml` file from the original repo,
leaving untouched the other generated folders.

#### IntelliJ performance

If your IntelliJ has high-CPU and high-memory peaks while editing, the
following tweaks might be useful:

- Disable type-aware highlighting by clicking on the small `[T]` icon on the
  bottom right corner of the window.
- Go to `Help -> Edit Custom VM Options` and increase the JVM memory at least
  with the following values:
```
-Xms500m -Xmx1500m
```
- If still nothing works, try `File -> Power Save Mode`.

#### IntelliJ Import optimization

IntelliJ offers an option to optimize imports upon commit. We suggest not to
use that feature, as it may remove needed imports thereby breaking
compilation.

### Eclipse

- Check that you have installed the [scala-plugin](http://scala-ide.org/) for
  Eclipse.
- To run TIP from within Eclipse, feed the [arguments](#tipcmd) into the `Run
  Arguments` dialog.

### Working from the command-line

A wrapper command `tip` (`tip.bat` for Windows) is provided to compile and run
TIP with the given arguments.

Example:
```
./tip -run examples/fib.tip
```

To build:
```
sbt compile
```

## Command-line arguments <a name="tipcmd"></a>

Usage:
```
tip <options> <source> [out]
```
where `<source>` can be a file or a directory containing `.tip` files and
`[out]` is an output directory (default: ./out).

To see the possible options, run `tip` without options.

## Code style

To avoid using inconsistent code styles and meaningless diffs caused
by IDE reformatting we use [scalafmt](https://olafurpg.github.io/scalafmt/).
Before committing, please double-check that all the code is in the right
format by executing `sbt scalafmt` or by installing the `scalafmt` IDE plugin
and enabling the `Format on save` option as explained
[here](https://olafurpg.github.io/scalafmt/#IntelliJ).

## Authors

- [Gianluca Mezzetti](http://gmezzetti.name/)
- [Anders M&oslash;ller](http://cs.au.dk/~amoeller/)

with contributions from

- [Coen De Roover](http://soft.vub.ac.be/~cderoove/)
- [Quentin Stievenart](http://awesom.eu/~acieroid/)
- Erik Krogh Kristensen
- Christian Budde Christensen
