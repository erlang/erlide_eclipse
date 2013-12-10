package org.erlide.engine.model.builder

class BuilderProperties {
    
    /* TODO is this necessary? */
    @Property boolean nukeOutputOnClean = false
    
    /** The tool that is used to build the project. */
    @Property BuilderTool builderTool
    
    /** Full build target name. */
    @Property String buildTarget
    /** Incremental build target name. */
    @Property String compileTarget
    /** Clean target name. */
    @Property String cleanTarget
    
}
