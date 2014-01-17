package org.erlide.ui.wizards

import org.eclipse.core.runtime.IPath
import org.erlide.engine.model.root.ErlangProjectProperties
import org.erlide.engine.model.builder.BuilderTool
import org.erlide.engine.model.builder.BuilderConfigType

class NewProjectData extends ErlangProjectProperties {
    
    @Property String name
    @Property IPath location

    @Property BuilderTool builder = BuilderTool.INTERNAL
    @Property BuilderConfigType builderConfig = BuilderConfigType.INTERNAL
    
}