package org.erlide.ui.wizards;

import java.util.List;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

public class ErlangProjectImportIncludeAndSourceDirsWizardPage extends
		WizardPage {

	private DirectoryTreeGroup sourceDirTreeGroup;
	private DirectoryTreeGroup includeDirTreeGroup;

	public ErlangProjectImportIncludeAndSourceDirsWizardPage() {
		super("Select directories for source and include files");
		setTitle(ErlangDataTransferMessages.ErlangProjectImport_selectSourceAndIncludeDirectories);
		setDescription(ErlangDataTransferMessages.ErlangProjectImport_selectSourceAndIncludeDirectories);
	}

	public void createControl(final Composite parent) {
		initializeDialogUnits(parent);

		// Browse
		final Composite composite = new Composite(parent, SWT.NULL);
		composite.setLayout(new GridLayout());
		composite.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_FILL
				| GridData.HORIZONTAL_ALIGN_FILL));
		composite.setSize(composite.computeSize(SWT.DEFAULT, SWT.DEFAULT));
		composite.setFont(parent.getFont());

		sourceDirTreeGroup = createDirectoryTreeGroup(composite,
				"Source directories");
		includeDirTreeGroup = createDirectoryTreeGroup(composite,
				"Include directories");

		setErrorMessage(null); // should not initially have error message

		setControl(composite);

	}

	private DirectoryTreeGroup createDirectoryTreeGroup(
			final Composite composite, final String title) {
		return new DirectoryTreeGroup(title, composite);
	}

	public void setup(final String projectDir, final List<String> allDirs,
			final List<String> includeDirs, final List<String> sourceDirs) {
		sourceDirTreeGroup.setAllDirs(allDirs);
		sourceDirTreeGroup.setCheckedDirs(sourceDirs);
		includeDirTreeGroup.setAllDirs(allDirs);
		includeDirTreeGroup.setCheckedDirs(includeDirs);
	}

	@Override
	public void setVisible(final boolean visible) {
		super.setVisible(visible);
		if (visible) {
			sourceDirTreeGroup.refresh();
			includeDirTreeGroup.refresh();
		}
	}
}
