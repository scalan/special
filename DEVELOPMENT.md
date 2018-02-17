## Versioning

[sbt-release](https://github.com/sbt/sbt-release) plugin is used. While we are in 0.x.y stage, minor incompatible changes can bump the bugfix (last) version part.

## Build

Current `scalaVersion` is set to 2.11. 

Important to mention that cross-compilation modifies `scalaVersion` globally to
the version of the target project. You need to `reload` sbt to reset
configuration.
