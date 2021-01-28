package org.erlide.ui.wizards;

import java.io.File;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;
import org.erlide.engine.NewProjectData;

@SuppressWarnings("all")
public class ErlangNewProjectCreationPage extends WizardNewProjectCreationPage {
    private final NewProjectData info;

    public ErlangNewProjectCreationPage(final String name, final NewProjectData info) {
        super(name);
        this.info = info;
    }

    @Override
    public void setVisible(final boolean visible) {
        super.setVisible(visible);
        if (visible) {
            onEntry();
        } else {
            onExit();
        }
    }

    protected void onEntry() {
    }

    protected void onExit() {
        info.setName(getProjectName());
        final String _path = getLocationURI().getPath();
        final Path _path_1 = new Path(_path);
        info.setLocation(_path_1);
        info.setExistingProject(projectExists());
    }

    private boolean projectExists() {
        final IPath loc = info.getLocation();
        if (loc == null || info.getName().isEmpty()) {
            return false;
        }
        final File dir = loc.toFile();
        return dir.exists();
    }
}
