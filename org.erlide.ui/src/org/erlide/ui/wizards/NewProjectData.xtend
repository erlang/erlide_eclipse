package org.erlide.ui.wizards

import org.eclipse.core.runtime.IPath
import org.erlide.engine.model.root.ErlangProjectProperties
import org.erlide.engine.model.builder.BuilderTool
import org.erlide.engine.model.builder.BuilderConfigType
import com.google.common.base.Objects
import java.util.Map

class NewProjectData extends ErlangProjectProperties {

    @Property String name = null
    @Property IPath location = null
    @Property boolean existingProject = false

    @Property BuilderTool builder = BuilderTool.INTERNAL
    @Property BuilderConfigType builderConfig = BuilderConfigType.INTERNAL
    
    // TODO build targets?
    @Property Map<String, String> builderData = newHashMap

    override String toString() {
        val helper = Objects.toStringHelper(this) => [
            add("name", _name)
            add("location", _location)
            add("existingProject", _existingProject)
            add("builder", _builder)
            add("builderConfig", _builderConfig)
            add("builderData", _builderData)
            add("super", super.toString)
        ]
        helper.toString
    }

}
