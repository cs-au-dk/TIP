# TIP

TIP is a tiny imperative programming language aimed at teaching the
fundamental concepts of static program analysis. This code accompanies the
lecture notes on [Static Program Analysis](http://cs.au.dk/~amoeller/spa/).

## Getting started

Prerequisites:
- Java 8 (or newer)
- [Scala 2.12](http://www.scala-lang.org/download/)
- [SBT](http://www.scala-sbt.org/)

We suggest you to use IntelliJ for working on TIP, but all the following
options are viable.

### IntelliJ IDEA

- Install the Scala and the SBT plugins in IntelliJ.
- Clone this repository (File -> New -> Project from Version Control -> GitHub).
  Choose the directory name `tip`. (**If you use another directory name, then the next step may fail!**)
- IntelliJ should then detect an SBT project. Click 'Import SBT project' and follow the instructions.
  (If this is your first Scala project, you will need to setup the Scala SDK.)
- Right-click on `Tip.scala` in `src/tip`, then select "Run 'Tip'". To supply
  arguments, use Run... -> Edit Configurations in the Run menu.
  
If you clone the repository using git instead of IntelliJ, you will need to import the project from the SBT model 
(File -> New -> Project from Existing Sources). 
Since the `.idea` folder is then regenerated from scrach, in order to keep the
inspection profiles you need to checkout the `.idea/inspectionProfiles`
folder and the `.idea/codeStyles` folder from the original repo,
leaving the other generated folders untouched.

#### IntelliJ Scala bugs

**Important: if you experience spurious type errors reported by IntelliJ for code involving Scala implicits, try disabling type-aware highlighting**
by clicking on the small `[T]` icon on the bottom right corner of the window.

#### IntelliJ performance

If your IntelliJ has high-CPU and high-memory peaks while editing, the
following tweaks might be useful:

- Disable type-aware highlighting by clicking on the small `[T]` icon on the
  bottom right corner of the window.
- Go to Help -> Edit Custom VM Options and increase the JVM memory at least
  with the following values:
```
-Xms500m -Xmx1500m
```
- If still nothing works, try File -> Power Save Mode.

#### IntelliJ Import optimization

IntelliJ offers an option to optimize imports upon commit. We suggest not to
use that feature, as it may remove needed imports thereby breaking
compilation.

### Eclipse

- Check that you have installed the [scala-plugin](http://scala-ide.org/) for
  Eclipse.
- To run TIP from within Eclipse, feed the [arguments](#tipcmd) into the Run
  Arguments dialog.

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
Option `-verbose` is recommended when developing and testing analyses.

## Visualizing control flow graphs and analysis results

The main function `Tip.scala` emits control flow graphs and analysis results as ".dot" files 
that can be processed by [Graphviz](https://www.graphviz.org/) to produce images, for example using the Graphviz dot command-line tool:
```
dot -O -Tpng out/example.tip__sign.dot
```

## Program normalization

Some analyses require the programs use restricted subsets of TIP. 
The following kinds of normalization can be performed automatically:

- `-normalizecalls`: 
  normalizes function calls to be top-level only and such that arguments are identifiers 
  (e.g. `id1 = id2(id3,id4)`)
- `-normalizereturns`: 
  normalizes return expressions to be identifiers 
  (e.g. `return id`)
- `-normalizepointers`: 
  normalizes pointer operations to primitive statements
  (`id = alloc P` where `P` is null or an integer constant, `id1 = &id2`, `id1 = id2`, `id1 = *id2`, `*id1 = id2`, or`id = null`) 
 
If one or more of these options are enabled, the normalized program is printed to e.g. `out/example.tip__normalized.tip`. 
 
## Help to Scala novices

This implementation takes advantage of many cool Scala language features that allow the code to be concise and flexible. 
Many of these language features are quite intuitive and easy to understand for anyone familiar with 
object oriented and functional programming, even without prior knowledge of Scala.
Still, the following language features deserve some extra attention:

- [traits](https://docs.scala-lang.org/tour/traits.html)
- [case classes](https://docs.scala-lang.org/tour/case-classes.html)
- [companion objects](https://docs.scala-lang.org/tour/singleton-objects.html)
- [abstract type members](https://docs.scala-lang.org/tour/abstract-types.html) (see e.g. [GenericLattices.scala](src/tip/lattices/GenericLattices.scala))
- [implicit parameters](https://docs.scala-lang.org/tour/implicit-parameters.html) (see e.g. [TypeAnalysis.scala](src/tip/analysis/TypeAnalysis.scala))
- [implicit conversions](https://docs.scala-lang.org/tour/implicit-conversions.html) (see e.g. [TipType.ast2typevar](src/tip/types/Types.scala))
- [implicit classes](https://docs.scala-lang.org/overviews/core/implicit-classes.html) (see e.g. [AstNodeData.AstNodeWithDeclaration](src/tip/ast/AstNodeData.scala))

Tutorials and extensive documentation for Scala are available at [http://docs.scala-lang.org/](http://docs.scala-lang.org/).

Useful tips: 
  - You can see what type Scala has inferred for an expression by selecting the expression and pressing Alt+Equals 
(depending on keyboard settings in Settings -> Keymap -> Scala -> Type Info).
  - You can see what implicit conversion Scala is applying by enabling View -> Show Implicit Hints (and View -> Expand Implicit Hints, for full information).

## Code style

To avoid using inconsistent code styles and meaningless diffs caused
by IDE reformatting we use [scalafmt](http://scalameta.org/scalafmt/).

The code is automatically formatted upon compilation by SBT.
In IntelliJ you need to ensure that SBT is used for compiling: in File -> Settings... -> Build Tools -> SBT, check the option "Use SBT shell for build and import".

Before committing, please double-check that all the code is in the right format by executing `sbt scalafmt`. 
By installing the scalafmt IntelliJ plugin and enabling the "Format on save" option as explained
[here](https://olafurpg.github.io/scalafmt/#IntelliJ) the code is automatically formatted when the file is saved (unfortunately the formatting is only triggered when the file is *explicitly* saved with Ctrl-S).

## Authors

- [Gianluca Mezzetti](http://gmezzetti.name/)
- [Anders M&oslash;ller](http://cs.au.dk/~amoeller/)

with contributions from

- [Coen De Roover](http://soft.vub.ac.be/~cderoove/)
- [Quentin Stievenart](http://awesom.eu/~acieroid/)
- Erik Krogh Kristensen
- Christian Budde Christensen
