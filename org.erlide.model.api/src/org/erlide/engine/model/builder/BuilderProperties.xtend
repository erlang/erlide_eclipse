package org.erlide.engine.model.builder

/*
 * TODO there should be specific properties for each builder tool
 * but we wait until we see if there are many properties that are only
 * relevant for a single tool
 */
class BuilderProperties {

  /** The tool that is used to build the project. */
  @Property BuilderTool builderTool = BuilderTool.INTERNAL

  /** Full build target name. */
  @Property String buildTarget = "rebuild"

  /** Incremental build target name. */
  @Property String compileTarget = "compile"

  /** Clean target name. */
  @Property String cleanTarget = "clean"

  /** Test target name. */
  @Property String testTarget = "test"

  /** Any extra flags required, as they would be specified on the command line. */
  @Property String extraFlags = ""

  def void fromString(String data) {
    val parts = data.split("|")
    try {
      val bt = BuilderTool.valueOf(parts.get(0))
      val b = parts.get(1)
      val c = parts.get(2)
      val l = parts.get(3)
      val t = parts.get(4)

      builderTool = bt
      buildTarget = b
      compileTarget = c
      cleanTarget = l
      testTarget = t
    } catch (Exception e) {
      return
    }
  }

  override toString() {
    '''«builderTool»|«compileTarget»|«cleanTarget»|«testTarget»|«extraFlags»'''
  }

}
