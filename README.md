# Special: Compiling Scala to Something special

[![Join the chat at https://gitter.im/scalan/scalan](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/scalan/scalan?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/scalan/special.svg?branch=master)](https://travis-ci.org/scalan/special)

## Intro

DISCLAIMER: This is very experimental! Use it at your own risk.

Special is a framework for compiling Scala to something special. It allows you to write Scala code and compile it into some domain specific execution engine by configuring your specific compilation pipeline.

Special subsumes [Scalanizer](https://github.com/scalan/scalanizer) and includes it as [submodule](https://github.com/scalan/special/scalanizer). 

Special can be used to develop domain-specific compilers for hot-spot optimization in Scala. (The basic idea is explained in this Scala Days [video](https://www.parleys.com/tutorial/program-functionally-execute-imperatively-peeling-abstraction-overhead-from-functional-programs). The corresponding demonstration project is [here](https://github.com/scalan/scalanizer-demo))

Special is the only framework (to our best knowledge) which uses [Isomorphic Specialization](http://dl.acm.org/citation.cfm?id=2633632): a new specialization algorithm and technique which allows to perform cross-domain translations of programs. It enables construction of compilation pipelines with gradual lowering of domain-specific abstractions. Other approaches exist though :-)

Please visit [Scalan Google Group](https://groups.google.com/forum/#!forum/scalan) for Special discussions. See also [Contributions](#contributions) below and get involved.

### Building the project and running tests

[SBT](http://www.scala-sbt.org/) is required to build Special. See SBT documentation for installation and usage instructions.

If you want to create your own project depending on Special, you should use `publishLocal` SBT command to publish Special artifacts to your local Ivy repository and add dependencies as usual.

### Stability

Currently this is experimental research playground and quite far from stable release. If you are interested to try it out for you special case, please get in touch.

## If you are familiar with Scalan and/or LMS 
With the introduction of [Scalanizer](https://github.com/scalan/special/scalanizer), the role of scalan-core is shifted one level down in the middle part of the compilation pipeline to perform main transformations of Scalan IR at compile time. Even though Special still can be used for development of new EDSLs without Scalanizer using [old Scalan way](https://github.com/scalan/scalan), this is NOT its main use case.

With Special if you write a program which is legal Scala, but not legal in your special domain, there should be error messages or warnings at compile time issued by Scalanizer plugin. This is one of the reasons and rationale for creating Special as a new frontend for Scalan pipeline, in this way errors can easily be handled at compile time.

## Contributions

Minor pull requests (typos, bug fixes and so on) are gladly accepted; for anything larger please raise an issue first.

Issues with the `low-hanging fruit` label should be easy to fix if you want something to get started with.

If you want to start working on an issue (existing or one you just raised), please label it `in progress`.

## See also

[Scalan](https://github.com/scalan/scalan) - the original framework for domain-specific compilation. Currently frozen and most usefull ideas are being reincarnated in Special. 

[Scalanizer](https://github.com/scalan/scalanizer) - a first version of experimental Scala plugin which allows to capture Scala ASTs and translate it into Scalan IR. (subsumed by [this submodule](https://github.com/scalan/special/scalanizer).)

[Scalanizer Demo](https://github.com/scalan/scalanizer-demo) - a simple project that demonstrates how to use Scalanizer, declare hot-spot regions and generate efficient kernels for JVM and native execution. (this corresponds to Scala Days 2015 demonstration)
