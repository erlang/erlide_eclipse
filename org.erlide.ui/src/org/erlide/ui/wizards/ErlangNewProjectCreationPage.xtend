package org.erlide.ui.wizards

import org.eclipse.core.runtime.Path
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage
import org.eclipse.core.runtime.IPath
import java.io.File

class ErlangNewProjectCreationPage extends WizardNewProjectCreationPage {

    val NewProjectData info

    new(String name, NewProjectData info) {
        super(name);
        this.info = info
    }

    override setVisible(boolean visible) {
        super.setVisible(visible)
        if (!visible) {
            val projectPath = new Path(locationURI.path)
            info.locationChanged = info.name != projectName || info.location != projectPath
            info.name = projectName
            info.location = projectPath
            info.existingProject = projectExists()
        }
    }

    def private boolean projectExists() {
        val IPath loc = info.getLocation()
        if (loc === null || info.getName().isEmpty()) {
            return false
        }
        val File dir = loc.toFile
        return dir.exists
    }

}
