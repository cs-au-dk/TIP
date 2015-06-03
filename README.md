#TIP

TIP is a tiny imperative programming language aimed at teaching the fundamental concepts of static program analysis.

[![Build Status](https://travis-ci.org/cs-au-dk/TIP.svg)](https://travis-ci.org/cs-au-dk/TIP)

## Getting started

Prerequisites:
- Check you have [Scala 2.11](http://www.scala-lang.org/download/) installed 

### Eclipse users

- Check that you have installed the [scala-plugin](http://scala-ide.org/) for eclipse 
- Run ```./gradlew eclipse``` (```gradlew eclipse``` on Windows) to generate the eclipse project files.
- Now you should be able to open and build the project from eclipse.
- To run tip from within eclipse feed the [arguments](#tipcmd) into the "Run Arguments" dialog

### IntelliJ IDEA users

- Import the project from the Gradle model
- Since the ```.idea``` folder is regenerated from scrach, in order to keep the inspection profiles, you need to checkout the ```.idea/inspectionProfiles``` folder from this repo, leaving untouched the other generated folders
- Follow the IDE instructions to download the Scala plugin and to set-up the Scala-SDK, if needed

### Working from the terminal

To build 

```
./gradlew build
```

To build an executable

```
./gradlew installDist
```

#### Build -> Run cycle

A wrapper ```tipw``` is provided to compile and run TIP with the given arguments.

## Command-line arguments <a name="tipcmd"></a>

```
 Usage:
 tip <options> <source> [out]

 <source> can be a file or a directory,

 [out] is an output directory (default: ./out)

 possible options are:

 Analyses:

 -cfg               construct the control flow graph, but do not perform any analysis
 -icfg              construct the interprocedural control flow graph, but do not perform any analysis
 -types             enable type analysis
 -cfa               enable control-flow analysis
 -andersen          enable Andersen pointer analysis
 -steensgaard       enable Steensgaard pointer analysis
 -sign              enable sign analysis
 -livevars          enable live variables analysis
 -available         enable available expressions analysis
 -vbusy             enable very busy expressions analysis
 -reaching          enable reaching definitions analysis
 -constprop         enable constant propagation analysis
 -interval          enable interval analysis

 some of the previous options can be followed immediately by the modifiers

 wl       use the worklist solver
 wli      use the worklist solver with init
 wliw     use the worklist solver with init and widening
 wliwn    use the worklist solver with init, widening, and narrowing
 wlip     use the worklist solver with init and propagation
 iwli     use the worklist solver with init, interprocedural version
 iwlip    use the worklist solver with init and propagation, interprocedural version
 iwlic    use the worklist solver with init, interprocedural version with CFA analysis

 e.g. -sign wl  will run the sign analysis using the basic worklist solver

 Running:

 -run               run the program as the last step
```
## Authors

- Gianluca Mezzetti
- [Anders M&oslash;ller](http://cs.au.dk/~amoeller/)
- Erik Krogh Kristensen
- Christian Budde Christensen

