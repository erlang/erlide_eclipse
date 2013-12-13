package org.erlide.engine.model.builder

/*
 * TODO there should be specific properties for each builder tool 
 * but we wait until we see if there are many properties that are only 
 * relevant for a single tool
 */
class BuilderProperties {

    /* TODO is this necessary? */
    @Property boolean nukeOutputOnClean = false

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

}
