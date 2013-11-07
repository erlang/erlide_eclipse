package org.erlide.ui.wizards

import org.eclipse.core.runtime.IPath
import org.erlide.engine.model.root.ErlangProjectProperties

class NewProjectData extends ErlangProjectProperties {
    
    @Property String builderName
    @Property String builderConfig
    @Property String name
    @Property IPath location
    
}