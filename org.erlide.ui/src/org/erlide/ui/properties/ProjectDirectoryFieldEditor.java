package org.erlide.ui.properties;

import java.io.File;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;

public class ProjectDirectoryFieldEditor extends DirectoryFieldEditor {
	private final IProject project;

	public ProjectDirectoryFieldEditor(String name, String labelText,
			Composite parent, IProject project) {
		super(name, labelText, parent);
		this.project = project;
	}

	@Override
	protected String changePressed() {
		String f = getTextControl().getText();
		if (!new File(f).exists()) {
			f = null;
		}
		String d = getDirectory(f);
		if (d == null) {
			return null;
		}

		return d;
	}

	private String getDirectory(String f) {

		final DirectoryDialog dialog = new DirectoryDialog(getShell());
		if (getLabelText() != null) {
			dialog.setMessage(getLabelText());
		}
		dialog.setFilterPath(project == null ? "/" : project.getLocation()
				.toString());

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
