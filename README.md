#TIP

TIP is a simple imperative programming language aimed at teaching the fundamental concepts of static program analysis.


## Getting started

Prerequisites:
- Check you have [Scala 2.11](http://www.scala-lang.org/download/) installed 


### Eclipse users

- Check that you have installed the [scala-plugin](http://scala-ide.org/) for eclipse 
- Run ```./gradlew eclipse``` (```gradlew eclipse``` on Windows) to generate the eclipse project files.
- Now you should be able to open and build the project from eclipse.
- To run tip from within eclipse feed the [arguments](#tipcmd) into the "Run Arguments" dialog

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

A wrapper ```tipw``` is provided that compile and run tip with the provided arguments

## Command-line arguments <a name="tipcmd"></a>

 Usage:
 tip [options] &lt;source&gt; [out]

 &lt;source&gt; can be a file or a directory,

 [out] is an optional custom output directory,
 by default ./out is used

 possible options are:
 
 - -cfg : to output the control flow graph
 - -types : to output the cfg with the types at the declaration nodes

## Documentation

A pre-generated documentation is available [here](http://cs-au-dk.github.io/TIP/build/docs/scaladoc/)


