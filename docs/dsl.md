
### Project structure

Special has the following modules

Module Name    |   Description 
---------------|--------------
 `common`      | various utilities, helpers etc.
 `meta`        | implementation of AST used by scalanizer plugin (ScalanAst)
 `core`        | implementation of basic Graph-based AST machinery (see Scalan trait)
 `plugin`      | implementation of Scala plugin infrastructure which is reused different `scalanizer` modules
 `scalanizer`  | implementation of Scala plugin for current project (see LibraryPlugin)
 `library-conf`| configuration of Scala plugin for current project (see SpecialLibraryConfig) 
 `library-api` | Source API module of DSL specification, where only interfaces (traits) are defined
 `library-impl`| Source Impl module of DSL specification, where API interfaces can be implemented
 `library`     | Target module which will contain all generated code (see Library trait)

### General organization of Dsl related modules

The structure of the project is created around the notion of Special Library.
Any project containing implementation of a Special Library named `library` should have the following 
modules:

`library-conf` - Special Libarary configuration 
`library-api` - dsl declarations 
`library-impl` - dsl implementation 
`library-library` - target library for code generation 
`scalanizer` - implementation and configuration of Scala plugin to generate code in `library` 
               module



### How to add new operations to DSL

1. Declare required methods in `library-api` (for example in `special.collection.Col`)


