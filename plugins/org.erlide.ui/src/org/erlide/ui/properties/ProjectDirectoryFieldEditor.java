package org.erlide.ui.properties;

import java.io.File;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.swt.widgets.Composite;

public class ProjectDirectoryFieldEditor extends DirectoryFieldEditor {
    private final IProject project;
    private String fOutputLocation;
    private final boolean mustExist;

    public ProjectDirectoryFieldEditor(final String name, final String labelText,
            final Composite parent, final IProject project, final boolean mustExist) {
        super(name, labelText, parent);
        this.project = project;
        this.mustExist = mustExist;
    }

    @Override
    protected String changePressed() {
        final IContainer container = DirectorySelectUtil.chooseLocation("Choose folder",
                getLabelText(), project, fOutputLocation, getShell());
        if (container != null) {
            return container.getProjectRelativePath().toString();
        }
        return null;
    }

    @Override
    protected boolean doCheckState() {
        String fileName = getTextControl().getText();
        fileName = fileName.trim();
        if (fileName.length() == 0 && isEmptyStringAllowed()) {
            return true;
        }
        if (project != null) {
            final String prjLoc = project.getLocation().toString();
            fileName = prjLoc + "/" + fileName;
        }
        if (mustExist) {
            final File file = new File(fileName);
            return file.isDirectory();
        }
        return true;
    }

}
