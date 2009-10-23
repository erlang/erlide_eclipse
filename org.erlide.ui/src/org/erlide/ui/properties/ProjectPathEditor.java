package org.erlide.ui.properties;

import org.eclipse.jface.preference.PathEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;

public class ProjectPathEditor extends PathEditor {
	private final String dirChooserLabelText;
	private final String rootPath;

	public ProjectPathEditor(final String name, final String labelText,
			final String dirChooserLabelText, final String rootPath,
			final Composite parent) {
		init(name, labelText);
		this.dirChooserLabelText = dirChooserLabelText;
		this.rootPath = rootPath;
		createControl(parent);
	}

	@Override
	protected String getNewInputObject() {

		final DirectoryDialog dialog = new DirectoryDialog(getShell());
		if (dirChooserLabelText != null) {
			dialog.setMessage(dirChooserLabelText);
		}
		dialog.setFilterPath(rootPath);

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
