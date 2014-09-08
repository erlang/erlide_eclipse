package org.erlide.ui.wizards

import java.io.File
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.Path
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage
import org.erlide.engine.model.root.NewProjectData

class ErlangNewProjectCreationPage extends WizardNewProjectCreationPage {

    val NewProjectData info

    new(String name, NewProjectData info) {
        super(name);
        this.info = info
    }

    override setVisible(boolean visible) {
        super.setVisible(visible)
        if (visible) {
            onEntry
        } else {
            onExit
        }
    }

    def protected void onEntry() {
    }

    def protected void onExit() {
        info.name = projectName
        info.location = new Path(locationURI.path)
        info.existingProject = projectExists()
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
