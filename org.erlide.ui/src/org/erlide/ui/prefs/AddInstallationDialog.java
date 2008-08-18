/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.prefs;

import java.io.File;
import java.text.MessageFormat;
import java.util.ArrayList;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.StatusDialog;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.erlide.runtime.backend.InstallationInfo;
import org.erlide.ui.dialogfields.DialogField;
import org.erlide.ui.dialogfields.IDialogFieldListener;
import org.erlide.ui.dialogfields.IListAdapter;
import org.erlide.ui.dialogfields.IStringButtonAdapter;
import org.erlide.ui.dialogfields.ListDialogField;
import org.erlide.ui.dialogfields.StringButtonDialogField;
import org.erlide.ui.dialogfields.StringDialogField;
import org.erlide.ui.util.StatusInfo;

public class AddInstallationDialog extends StatusDialog implements
		IListAdapter<String> {

	public static class StringLabelProvider implements ILabelProvider {

		public Image getImage(Object element) {
			return null;
		}

		public String getText(Object element) {
			return (String) element;
		}

		public void addListener(ILabelProviderListener listener) {
		}

		public void dispose() {
		}

		public boolean isLabelProperty(Object element, String property) {
			return false;
		}

		public void removeListener(ILabelProviderListener listener) {
		}

	}

	private final IAddDialogRequestor<InstallationInfo> fRequestor;

	private final InstallationInfo fEditedInstallation;

	private StringDialogField fInstallationName;

	private StringButtonDialogField fOtpHome;

	private ListDialogField<String> fCodePath;

	private StringDialogField fDefaultArgs;

	private final IStatus[] fStatuses;

	public AddInstallationDialog(
			IAddDialogRequestor<InstallationInfo> requestor, Shell shell,
			InstallationInfo editedInstallation) {
		super(shell);
		setShellStyle(getShellStyle() | SWT.RESIZE);
		fRequestor = requestor;
		fStatuses = new IStatus[5];
		for (int i = 0; i < fStatuses.length; i++) {
			fStatuses[i] = new StatusInfo();
		}

		fEditedInstallation = editedInstallation;
	}

	/**
	 * @see Windows#configureShell
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		// PlatformUI.getWorkbench().getHelpSystem().setHelp(newShell,
		// IJavaDebugHelpContextIds.EDIT_JRE_DIALOG);
	}

	protected void createDialogFields() {

		fInstallationName = new StringDialogField();
		fInstallationName
				.setLabelText(InstallationPreferenceMessages.addDialog_ertsName);

		fOtpHome = new StringButtonDialogField(new IStringButtonAdapter() {

			public void changeControlPressed(DialogField field) {
				browseForInstallDir();
			}
		});
		fOtpHome.setLabelText("Location"); //$NON-NLS-1$
		fOtpHome.setButtonLabel("&Browse..."); //$NON-NLS-1$

		final String[] buttons = new String[] {
				InstallationPreferenceMessages.addDialog_add,
				InstallationPreferenceMessages.addDialog_remove, "Move up",
				"Move down" };
		fCodePath = new ListDialogField<String>(this, buttons,
				new StringLabelProvider());
		fCodePath.setLabelText("PathA");

		fDefaultArgs = new StringDialogField();
		fDefaultArgs.setLabelText("E&xtra args"); //$NON-NLS-1$
	}

	protected void createFieldListeners() {
		fInstallationName.setDialogFieldListener(new IDialogFieldListener() {

			public void dialogFieldChanged(DialogField field) {
				setInstallationNameStatus(validateInstallationName());
				updateStatusLine();
			}
		});

		fOtpHome.setDialogFieldListener(new IDialogFieldListener() {

			public void dialogFieldChanged(DialogField field) {
				setInstallationLocationStatus(validateInstallationLocation());
				updateStatusLine();
			}
		});

	}

	protected String getInstallationName() {
		return fInstallationName.getText();
	}

	protected File getInstallationLocation() {
		return new File(fOtpHome.getText());
	}

	@Override
	protected Control createDialogArea(Composite ancestor) {
		createDialogFields();
		final Composite parent = (Composite) super.createDialogArea(ancestor);
		((GridLayout) parent.getLayout()).numColumns = 3;

		fInstallationName.doFillIntoGrid(parent, 3);
		fOtpHome.doFillIntoGrid(parent, 3);
		fCodePath.doFillIntoGrid(parent, 3);
		fDefaultArgs.doFillIntoGrid(parent, 3);

		final Text t = fInstallationName.getTextControl(parent);
		final GridData gd = (GridData) t.getLayoutData();
		gd.grabExcessHorizontalSpace = true;
		gd.widthHint = convertWidthInCharsToPixels(50);

		initializeFields();
		createFieldListeners();
		applyDialogFont(parent);
		return parent;
	}

	@Override
	public void create() {
		super.create();
		fInstallationName.setFocus();
	}

	private void initializeFields() {
		if (fEditedInstallation == null) {
			fInstallationName.setText(""); //$NON-NLS-1$
			fOtpHome.setText(""); //$NON-NLS-1$
			fCodePath.setElements(new ArrayList<String>(5));
			fDefaultArgs.setText(""); //$NON-NLS-1$
		} else {
			fInstallationName.setText(fEditedInstallation.getName());
			fOtpHome.setText(fEditedInstallation.getOtpHome());
			fCodePath.setElements(fEditedInstallation.getCodePath());
			fDefaultArgs.setText(fEditedInstallation.getArgs());
		}
		setInstallationNameStatus(validateInstallationName());
		setInstallationLocationStatus(validateInstallationLocation());
		updateStatusLine();
	}

	protected IStatus validateInstallationName() {
		final StatusInfo status = new StatusInfo();
		final String name = fInstallationName.getText();
		if (name == null || name.trim().length() == 0) {
			status.setInfo("Enter the installation's name"); //$NON-NLS-1$
		} else {
			if (fRequestor.isDuplicateName(name)
					&& (fEditedInstallation == null || !name
							.equals(fEditedInstallation.getName()))) {
				status.setError("The name is already used"); //$NON-NLS-1$
			} else {
				final IStatus s = ResourcesPlugin.getWorkspace().validateName(
						name, IResource.FILE);
				if (!s.isOK()) {
					status.setError(MessageFormat.format("name is invalid",
							(Object[]) new String[] { s.getMessage() }));
				}
			}
		}
		return status;
	}

	protected IStatus validateInstallationLocation() {
		final StatusInfo status = new StatusInfo();
		final String loc = fOtpHome.getText();
		if (loc == null || loc.trim().length() == 0) {
			status.setInfo("Enter the installation's location");
		} else {
			final File f = new File(loc);
			if (!f.exists()) {
				status.setError("Location doesn't exist");
			} else if (!f.isDirectory()) {
				status.setError("Location isn't a directory");
			} else if (!InstallationInfo.validateLocation(loc)) {
				status.setError("Location is not a valid OTP home");
			}
		}
		return status;
	}

	protected void updateStatusLine() {
		IStatus max = null;
		for (final IStatus curr : fStatuses) {
			if (curr.matches(IStatus.ERROR)) {
				updateStatus(curr);
				return;
			}
			if (max == null || curr.getSeverity() > max.getSeverity()) {
				max = curr;
			}
		}
		updateStatus(max);
	}

	protected void browseForInstallDir() {
		final DirectoryDialog dialog = new DirectoryDialog(getShell());
		dialog.setFilterPath(fOtpHome.getText());
		dialog
				.setMessage(InstallationPreferenceMessages.addDialog_pickInstallationRoot);
		final String newPath = dialog.open();
		if (newPath != null) {
			fOtpHome.setText(newPath);
		}
	}

	@Override
	protected void okPressed() {
		doOkPressed();
		super.okPressed();
	}

	private void doOkPressed() {
		if (fEditedInstallation == null) {
			final InstallationInfo info = new InstallationInfo();
			setFieldValuesToInstallation(info);
			fRequestor.itemAdded(info);
		} else {
			setFieldValuesToInstallation(fEditedInstallation);
		}
	}

	protected void setFieldValuesToInstallation(InstallationInfo installation) {
		installation.setOtpHome(fOtpHome.getText());
		installation.setName(fInstallationName.getText());

		installation.setCodePath(fCodePath.getElements());

		final String argString = fDefaultArgs.getText().trim();
		installation.setArgs(argString);

	}

	protected File getAbsoluteFileOrEmpty(String path) {
		if (path == null || path.length() == 0) {
			return new File(""); //$NON-NLS-1$
		}
		return new File(path).getAbsoluteFile();
	}

	protected void setInstallationNameStatus(IStatus status) {
		fStatuses[0] = status;
	}

	protected void setInstallationLocationStatus(IStatus status) {
		fStatuses[1] = status;
	}

	protected void setInstallationVersionStatus(IStatus status) {
		fStatuses[2] = status;
	}

	/**
	 * Updates the status of the ok button to reflect the given status.
	 * Subclasses may override this method to update additional buttons.
	 * 
	 * @param status
	 *            the status.
	 */
	@Override
	protected void updateButtonsEnableState(IStatus status) {
		final Button ok = getButton(IDialogConstants.OK_ID);
		if (ok != null && !ok.isDisposed()) {
			ok.setEnabled(status.getSeverity() == IStatus.OK);
		}
	}

	/**
	 * @see org.eclipse.jface.dialogs.Dialog#setButtonLayoutData(org.eclipse.swt.widgets.Button)
	 */
	@Override
	protected void setButtonLayoutData(Button button) {
		super.setButtonLayoutData(button);
	}

	/**
	 * Returns the name of the section that this dialog stores its settings in
	 * 
	 * @return String
	 */
	protected String getDialogSettingsSectionName() {
		return "ADD_VM_DIALOG_SECTION"; //$NON-NLS-1$
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.window.Window#close()
	 */
	@Override
	public boolean close() {
		// DialogSettingsHelper.persistShellGeometry(getShell(),
		// getDialogSettingsSectionName());
		return super.close();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.jface.window.Window#getInitialLocation(org.eclipse.swt.graphics
	 * .Point)
	 */
	@Override
	protected Point getInitialLocation(Point initialSize) {
		// Point initialLocation = DialogSettingsHelper
		// .getInitialLocation(getDialogSettingsSectionName());
		// if (initialLocation != null)
		// {
		// return initialLocation;
		// }
		return super.getInitialLocation(initialSize);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.window.Window#getInitialSize()
	 */
	@Override
	protected Point getInitialSize() {
		final Point size = super.getInitialSize();
		return size;
		// return DialogSettingsHelper.getInitialSize(
		// getDialogSettingsSectionName(), size);
	}

	public void customButtonPressed(ListDialogField<String> field, int index) {
		switch (index) {
		case 0:
			addPath(field);
			break;
		case 1:
			removePath(field);
			break;
		case 2:
			moveUp(field);
			break;
		case 3:
			moveDown(field);
			break;
		default:
			;
		}
	}

	private void moveDown(ListDialogField<String> field) {
	}

	private void moveUp(ListDialogField<String> field) {
	}

	private void removePath(ListDialogField<String> field) {
	}

	private void addPath(ListDialogField<String> field) {

		// TODO validate value ?
		InputDialog dlg = new InputDialog(new Shell(), "Add path",
				"Enter a path to be added to ", "", null);
		dlg.setBlockOnOpen(true);
		dlg.open();
		String value = dlg.getValue();

		if (value.length() > 0) {
			field.addElement(value);
		}
	}

	public void doubleClicked(ListDialogField<String> field) {
	}

	public void selectionChanged(ListDialogField<String> field) {
	}
}
