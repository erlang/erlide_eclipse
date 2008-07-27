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
package org.erlide.basicui.prefs;

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
import org.erlide.basiccore.RuntimeInfo;
import org.erlide.basiccore.StatusInfo;
import org.erlide.basicui.dialogfields.DialogField;
import org.erlide.basicui.dialogfields.IDialogFieldListener;
import org.erlide.basicui.dialogfields.IListAdapter;
import org.erlide.basicui.dialogfields.IStringButtonAdapter;
import org.erlide.basicui.dialogfields.ListDialogField;
import org.erlide.basicui.dialogfields.StringButtonDialogField;
import org.erlide.basicui.dialogfields.StringDialogField;

public class AddRuntimeDialog extends StatusDialog implements
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

	private final IAddDialogRequestor<RuntimeInfo> fRequestor;

	private final RuntimeInfo fEditedVM;

	private StringDialogField fVMName;

	private StringButtonDialogField fOtpHome;

	private ListDialogField<String> fCodePath;

	private StringDialogField fDefaultArgs;

	private final IStatus[] fStatuses;

	public AddRuntimeDialog(IAddDialogRequestor<RuntimeInfo> requestor,
			Shell shell, RuntimeInfo editedVM) {
		super(shell);
		setShellStyle(getShellStyle() | SWT.RESIZE);
		fRequestor = requestor;
		fStatuses = new IStatus[5];
		for (int i = 0; i < fStatuses.length; i++) {
			fStatuses[i] = new StatusInfo();
		}

		fEditedVM = editedVM;
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

		fVMName = new StringDialogField();
		fVMName.setLabelText(PreferenceMessages.addVMDialog_ertsName);

		fOtpHome = new StringButtonDialogField(new IStringButtonAdapter() {

			public void changeControlPressed(DialogField field) {
				browseForInstallDir();
			}
		});
		fOtpHome.setLabelText("Location"); //$NON-NLS-1$
		fOtpHome.setButtonLabel("&Browse..."); //$NON-NLS-1$

		final String[] buttons = new String[] {
				PreferenceMessages.AddVMDialog_3,
				PreferenceMessages.AddVMDialog_5, "Move up", "Move down" };
		fCodePath = new ListDialogField<String>(this, buttons,
				new StringLabelProvider());
		fCodePath.setLabelText("PathA");

		fDefaultArgs = new StringDialogField();
		fDefaultArgs.setLabelText("E&xtra args"); //$NON-NLS-1$
	}

	protected void createFieldListeners() {
		fVMName.setDialogFieldListener(new IDialogFieldListener() {

			public void dialogFieldChanged(DialogField field) {
				setVMNameStatus(validateVMName());
				updateStatusLine();
			}
		});

		fOtpHome.setDialogFieldListener(new IDialogFieldListener() {

			public void dialogFieldChanged(DialogField field) {
				setVMLocationStatus(validateVMLocation());
				updateStatusLine();
			}
		});

	}

	protected String getVMName() {
		return fVMName.getText();
	}

	protected File getInstallLocation() {
		return new File(fOtpHome.getText());
	}

	@Override
	protected Control createDialogArea(Composite ancestor) {
		createDialogFields();
		final Composite parent = (Composite) super.createDialogArea(ancestor);
		((GridLayout) parent.getLayout()).numColumns = 3;

		fVMName.doFillIntoGrid(parent, 3);
		fOtpHome.doFillIntoGrid(parent, 3);
		fCodePath.doFillIntoGrid(parent, 3);
		fDefaultArgs.doFillIntoGrid(parent, 3);

		final Text t = fVMName.getTextControl(parent);
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
		fVMName.setFocus();
	}

	private void initializeFields() {
		if (fEditedVM == null) {
			fVMName.setText(""); //$NON-NLS-1$
			fOtpHome.setText(""); //$NON-NLS-1$
			fCodePath.setElements(new ArrayList<String>(5));
			fDefaultArgs.setText(""); //$NON-NLS-1$
		} else {
			fVMName.setText(fEditedVM.getName());
			fOtpHome.setText(fEditedVM.getOtpHome());
			fCodePath.setElements(fEditedVM.getCodePath());
			fDefaultArgs.setText(fEditedVM.getArgs());
		}
		setVMNameStatus(validateVMName());
		setVMLocationStatus(validateVMLocation());
		updateStatusLine();
	}

	protected IStatus validateVMName() {
		final StatusInfo status = new StatusInfo();
		final String name = fVMName.getText();
		if (name == null || name.trim().length() == 0) {
			status.setInfo("Enter the VM's name"); //$NON-NLS-1$
		} else {
			if (fRequestor.isDuplicateName(name)
					&& (fEditedVM == null || !name.equals(fEditedVM.getName()))) {
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

	protected IStatus validateVMLocation() {
		final StatusInfo status = new StatusInfo();
		final String loc = fOtpHome.getText();
		if (loc == null || loc.trim().length() == 0) {
			status.setInfo("Enter the VM's location");
		} else {
			final File f = new File(loc);
			if (!f.exists()) {
				status.setError("Location doesn't exist");
			} else if (!f.isDirectory()) {
				status.setError("Location isn't a directory");
			} else if (!RuntimeInfo.validateLocation(loc)) {
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
				.setMessage(PreferenceMessages.addVMDialog_pickERTSRootDialog_message);
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
		if (fEditedVM == null) {
			final RuntimeInfo vm = new RuntimeInfo();
			setFieldValuesToVM(vm);
			fRequestor.itemAdded(vm);
		} else {
			setFieldValuesToVM(fEditedVM);
		}
	}

	protected void setFieldValuesToVM(RuntimeInfo vm) {
		vm.setOtpHome(fOtpHome.getText());
		vm.setName(fVMName.getText());

		vm.setCodePath(fCodePath.getElements());

		final String argString = fDefaultArgs.getText().trim();
		vm.setArgs(argString);

	}

	protected File getAbsoluteFileOrEmpty(String path) {
		if (path == null || path.length() == 0) {
			return new File(""); //$NON-NLS-1$
		}
		return new File(path).getAbsoluteFile();
	}

	protected void setVMNameStatus(IStatus status) {
		fStatuses[0] = status;
	}

	protected void setVMLocationStatus(IStatus status) {
		fStatuses[1] = status;
	}

	protected void setVMVersionStatus(IStatus status) {
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
	 * @see org.eclipse.jface.window.Window#getInitialLocation(org.eclipse.swt.graphics.Point)
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
