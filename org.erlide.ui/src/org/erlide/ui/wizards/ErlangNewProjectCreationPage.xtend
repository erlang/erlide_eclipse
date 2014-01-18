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
        if (visible) {
            onEntry
        } else {
            onExit
        }
    }

    def private void onEntry() {
    }

    def private void onExit() {
        val projectPath = new Path(locationURI.path)
        if (info.name != projectName || info.location != projectPath) {
            // clear info? or nothing?
        }
        info.name = projectName
        info.location = projectPath
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
