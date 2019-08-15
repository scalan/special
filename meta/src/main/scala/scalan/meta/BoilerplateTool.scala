package scalan.meta

import com.typesafe.scalalogging.StrictLogging

class BoilerplateTool extends StrictLogging {
  def coreMainConfig(name: String, entityFile: String) =
    UnitConfig(
      baseDir = "",
      name = name, entityFile = entityFile,
      srcPath = "core/src/main/scala",
      resourcePath = "core/src/main/resources",
      baseContextTrait = "" // used like this: trait ${module.name}Defs extends ${config.baseContextTrait.opt(t => s"$t with ")}${module.name} {
    )

  def coreTestConfig(name: String, entityFile: String) =
    UnitConfig(
      baseDir = "",
      name = name, entityFile = entityFile,
      srcPath = "core/src/test/scala",
      resourcePath = "core/src/test/resources",
      baseContextTrait = "scalan.Scalan")

  def corexMainConfig(name: String, entityFile: String) =
    UnitConfig(
      baseDir = "",
      name = name, entityFile = entityFile,
      srcPath = "corex/src/main/scala",
      resourcePath = "corex/src/main/resources",
      baseContextTrait = "" // used like this: trait ${module.name}Defs extends ${config.baseContextTrait.opt(t => s"$t with ")}${module.name} {
    )

  def corexTestConfig(name: String, entityFile: String) =
    UnitConfig(
      baseDir = "",
      name = name, entityFile = entityFile,
      srcPath = "corex/src/test/scala",
      resourcePath = "corex/src/test/resources",
      baseContextTrait = "scalan.ScalanEx")

  lazy val viewsConfig           = coreMainConfig("views",      "scalan/Views.scala")
  
  lazy val convertersConfig      = corexMainConfig("converters", "scalan/Converters.scala")
  lazy val specializationsConfig = corexMainConfig("specializations", "scalan/dynamic/Specializations.scala")

  lazy val structKeysConfig  = corexMainConfig("structKeys",  "scalan/primitives/StructKeys.scala")
  lazy val structItemsConfig = corexMainConfig("structItems", "scalan/primitives/StructItems.scala")

  lazy val segmentsConfig        = corexTestConfig("segments",  "scalan/common/Segments.scala")
  lazy val kindsConfig           = corexTestConfig("kinds",     "scalan/common/Kinds.scala")
  lazy val metatestsConfig       = corexTestConfig("metatests", "scalan/common/MetaTests.scala")

  val allConfigs = List(
    viewsConfig, convertersConfig, specializationsConfig,
    structKeysConfig, structItemsConfig,
    segmentsConfig, kindsConfig, metatestsConfig
  )

  val runGroups =
    allConfigs.map(c => c.name -> List(c)).toMap ++ // each individual config has its own group and can be executed
    Map( // config groups can be declared and executed by name
      "all" -> allConfigs
    )

  def listGroups = runGroups.keySet.mkString(", ")

  def getRequestedConfigs(requestedGroups: Array[String]): Seq[UnitConfig] =
    requestedGroups.flatMap { groupName => runGroups.getOrElse(groupName,
      sys.error(s"Unknown codegen config $groupName. Allowed values: $listGroups"))
    }.distinct

  def main(args: Array[String]) {
    val configs = getRequestedConfigs(args)

    if (configs.isEmpty) {
      logger.warn(s"BoilerplateTool run without specified config groups. Available: $listGroups")
    } else {
      val parsers = new Parsers(allConfigs)
      val em = new EntityManagement(parsers)
      for (c <- configs) {
        println(s"Processing ${c.srcPath}")
        em.generate(c.name)
        println(s"Ok\n")
      }
    }
  }
}

object BoilerplateToolRun extends BoilerplateTool {
}
