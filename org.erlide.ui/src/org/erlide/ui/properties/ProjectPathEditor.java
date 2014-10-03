package org.erlide.ui.properties;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.preference.PathEditor;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.erlide.util.PreferencesUtils;

public class ProjectPathEditor extends PathEditor {
    private final IProject project;
    private final String dirChooserLabelText;

    public ProjectPathEditor(final String name, final String labelText,
            final String dirChooserLabelText, final Composite parent,
            final IProject project) {
        super(name, labelText, dirChooserLabelText, parent);
        this.dirChooserLabelText = dirChooserLabelText;
        this.project = project;
    }

    @Override
    protected String getNewInputObject() {
        final IContainer container = DirectorySelectUtil.chooseLocation(
                dirChooserLabelText, getLabelText(), project, null, getShell());
        if (container != null) {
            return container.getProjectRelativePath().toString();
        }
        return null;
    }

    @Override
    protected String createList(final String[] items) {
        return PreferencesUtils.packArray(items);
    }

    @Override
    protected String[] parseString(final String stringList) {
        final String[] result = PreferencesUtils.unpackArray(stringList);
        for (int i = 0; i < result.length; i++) {
            result[i] = result[i].trim();
        }
        return result;
    }

    @Override
    protected void doFillIntoGrid(final Composite parent, final int numColumns) {
        super.doFillIntoGrid(parent, numColumns);
        final org.eclipse.swt.widgets.List list = getListControl(parent);
        final GridData gd = (GridData) list.getLayoutData();
        gd.heightHint = 100;
    }
}
