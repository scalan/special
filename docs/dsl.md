
### Project structure

Special project contains the following modules

Module Name    |   Description 
---------------|--------------
 `common`      | various utilities, helpers etc.
 `meta`        | implementation of AST used by scalanizer plugin (ScalanAst)
 `core`        | implementation of basic Graph-based AST machinery (see Scalan trait)
 `plugin`      | Scala plugin infrastructure which is reused by the different `scalanizer` modules
 `scalanizer`  | implementation of Scala plugin specifically to be used in the current project (see LibraryPlugin)
 `library-conf`| configuration of Scala plugin for current project (see SpecialLibraryConfig) 
 `library-api` | Source API module of DSL specification, where only interfaces (traits) are defined
 `library-impl`| Source Impl module of DSL specification, where API interfaces can be implemented
 `library`     | Target module which will contain all generated code (see Library trait)

### General organization of Dsl related modules

The structure of the project is created around the notion of Special Library.
Any project containing implementation of a Special Library named `mylib` should have the following 
modules:

`mylib-conf` - configuration of `mylib` Special Libarary, it may depend on other `lib-conf` from upstream jars
`mylib-api`  - dsl declarations, abstract traits without implementation
`mylib-impl` - dsl implementations, which specify how traits from `mylib-api` should be implemented 
`mylib-library` - target module where code of graph-based IR is generation. The
generated IR will contain classes to represent traits and methods declared in `mylib-api`
`scalanizer` - implementation and configuration of Scala plugin to generate code in `mylib-library` module

As a reference implementation of Special Library see the corresponding modules
of [SigmaDsl](https://github.com/ScorexFoundation/sigmastate-interpreter).


### How to configure new Special Library
TODO

### How to add new operations to DSL

1. Declare required traits and/or methods in `mylib-api` (for example in `special.collection.Col`)

2. Make sure the code compiles (i.e. you don't have compilation errors), for
that you may also have to implement those new methods in implementation classes of `mylib-impl`.

3. Commit changes (make sure you don't have uncommitted files)

4. Enable code generation
    - find the following line in build.sbt 
    ```//    s"-Xplugin:${file(".").absolutePath }/scalanizer/target/scala-2.12/scalanizer-assembly-mybranch-5a836259-SNAPSHOT.jar"```
    - uncomment it
    - replace `mybranch` with your current branch name 
    - replace `5a836259` with 8-digit hash of your latest commit
    
5. Reload sbt session so that sbt pick up new plugin declaration and enable it.
If at some point sbt throwd StackOverflowException you have to increase defaults
for example by using `sbt -J-Xss2M`

6. In sbt console run `;scalanizer/assembly ;mylib-api/clean ;mylib-api/compile`.
   This will make full recompilation of `mylib-api` module with active `scalanizer` plugin.
   As result you will see changes in the files of `mylib-api/src/main/resources` directory.
   The sbt command should complete without errors before proceeding to the next step.
   
7. In sbt console run `;scalanizer/assembly ;mylib-impl/clean ;mylib-impl/compile`.
  This will make full recompilation of `mylib-impl` module with active `scalanizer` plugin.
  As result you will see changes in the files of `mylib-impl/src/main/resources` directory.
  The sbt command should complete without errors before proceeding to the next step.
  
8. In sbt console run `;scalanizer/assembly ;mylib-library/clean ;mylib-library/compile`.
   This will make full recompilation of `mylib-library` module with active `scalanizer` plugin.
   As result you will see changes in the files of `mylib-impl/src/main` directory.
   The sbt command should complete without errors before proceeding to the next step.
   If for any reason you've got a compiler error you need to revert all the changes
   made in `mylib-library` and only after that you can repeat this step. 
   (That is where you will truly appreciate the step 3)
   
9. Disable code generation (you no longer need scalanizer plugin, so you need to disable it)
   - just comment back the line in `build.sbt` which you uncommented in step 4.
 
10. Reload sbt session, to actually disable the plugin.

11. Rebuild your project. If successful you are all done. Congratulations!
   If you we got errors in generated code this may happen because you had used
   some unsupported feature and scalanizer were not smart enought to notice that 
   and just generated incorrect code. Get in touch with maintainers. Sorry.
   

