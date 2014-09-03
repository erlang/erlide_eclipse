package org.erlide.engine.model.builder

import java.util.regex.Pattern
import org.eclipse.xtend.lib.annotations.Accessors

/*
 * TODO there should be specific properties for each builder tool
 * but we wait until we see if there are many properties that are only
 * relevant for a single tool
 */
@Accessors
class BuilderProperties {

    /** The tool that is used to build the project. */
    BuilderTool builderTool = BuilderTool.INTERNAL

    /** Full build target name. */
    String buildTarget = "rebuild"

    /** Incremental build target name. */
    String compileTarget = "compile"

    /** Clean target name. */
    String cleanTarget = "clean"

    /** Test target name. */
    String testTarget = "test"

    /** Any extra flags required, as they would be specified on the command line. */
    String extraFlags = ""

    def void fromString(String data) {
        val parts = data.split(Pattern.quote("|"))
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
