package org.erlide.ui.properties;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.preference.PathEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;

public class ProjectPathEditor extends PathEditor {
	private final String dirChooserLabelText;
	private final IProject project;

	public ProjectPathEditor(final String name, final String labelText,
			final String dirChooserLabelText, final IProject project,
			final Composite parent) {
		init(name, labelText);
		this.dirChooserLabelText = dirChooserLabelText;
		this.project = project;
		createControl(parent);
	}

	@Override
	protected String getNewInputObject() {

		final DirectoryDialog dialog = new DirectoryDialog(getShell());
		if (dirChooserLabelText != null) {
			dialog.setMessage(dirChooserLabelText);
		}
		dialog.setFilterPath(project.getLocation().toString());

		String dir = dialog.open();
		if (dir != null) {
			dir = dir.trim();
			if (dir.length() == 0) {
				return null;
			}
		}
		return dir;
	}

}
