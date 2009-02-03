package org.erlide.ui.properties;

import org.eclipse.jface.preference.PathEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;

public class ProjectPathEditor extends PathEditor {
	private String dirChooserLabelText;
	private String rootPath;

	public ProjectPathEditor(String name, String labelText,
			String dirChooserLabelText, String rootPath, Composite parent) {
		init(name, labelText);
		this.dirChooserLabelText = dirChooserLabelText;
		this.rootPath = rootPath;
		createControl(parent);
	}

	@Override
	protected String getNewInputObject() {

		DirectoryDialog dialog = new DirectoryDialog(getShell());
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
