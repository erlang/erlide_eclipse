package org.erlide.ui.prefs;

import java.io.File;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;

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
import org.erlide.basiccore.StatusInfo;
import org.erlide.basicui.dialogfields.ComboDialogField;
import org.erlide.basicui.dialogfields.DialogField;
import org.erlide.basicui.dialogfields.IDialogFieldListener;
import org.erlide.basicui.dialogfields.IListAdapter;
import org.erlide.basicui.dialogfields.ListDialogField;
import org.erlide.basicui.dialogfields.StringDialogField;
import org.erlide.runtime.backend.BackendInfo;
import org.erlide.runtime.backend.RuntimeInfoManager;

public class AddBackendDialog

extends StatusDialog implements IListAdapter<String> {

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

	private final IAddDialogRequestor<BackendInfo> fRequestor;

	private final BackendInfo fEditedBackend;

	private StringDialogField fName;
	private StringDialogField fNodeName;
	private StringDialogField fCookie;
	private ComboDialogField fRuntime;
	private ListDialogField<String> fCodePath;
	private StringDialogField fArgs;

	private final IStatus[] fStatuses;

	public AddBackendDialog(IAddDialogRequestor<BackendInfo> requestor,
			Shell shell, BackendInfo editedVM) {
		super(shell);
		setShellStyle(getShellStyle() | SWT.RESIZE);
		fRequestor = requestor;
		fStatuses = new IStatus[5];
		for (int i = 0; i < fStatuses.length; i++) {
			fStatuses[i] = new StatusInfo();
		}
		fEditedBackend = editedVM;
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

		fName = new StringDialogField();
		fName.setLabelText(PreferenceMessages.addRuntimeDialog_ertsName);

		fNodeName = new StringDialogField();
		fNodeName.setLabelText("node name");

		fCookie = new StringDialogField();
		fCookie.setLabelText("cookie");

		fRuntime = new ComboDialogField(SWT.DROP_DOWN | SWT.READ_ONLY);
		fRuntime.setLabelText("Runtime"); //$NON-NLS-1$
		Collection<String> elementNames = RuntimeInfoManager.getDefault()
				.getElementNames();
		fRuntime
				.setItems(elementNames.toArray(new String[elementNames.size()]));
		fRuntime.selectItem(0);

		final String[] buttons = new String[] {
				PreferenceMessages.AddRuntimeDialog_3,
				PreferenceMessages.AddRuntimeDialog_5, "Move up", "Move down" };
		fCodePath = new ListDialogField<String>(this, buttons,
				new StringLabelProvider());
		fCodePath.setLabelText("PathA");

		fArgs = new StringDialogField();
		fArgs.setLabelText("E&xtra args"); //$NON-NLS-1$
	}

	protected void createFieldListeners() {
		fName.setDialogFieldListener(new IDialogFieldListener() {

			public void dialogFieldChanged(DialogField field) {
				setNameStatus(validateName());
				updateStatusLine();
			}
		});

	}

	protected String getBackendName() {
		return fName.getText();
	}

	protected File getInstallLocation() {
		return new File(fRuntime.getText());
	}

	@Override
	protected Control createDialogArea(Composite ancestor) {
		createDialogFields();
		final Composite parent = (Composite) super.createDialogArea(ancestor);
		((GridLayout) parent.getLayout()).numColumns = 3;

		fName.doFillIntoGrid(parent, 3);
		fRuntime.doFillIntoGrid(parent, 3);
		fNodeName.doFillIntoGrid(parent, 3);
		fCookie.doFillIntoGrid(parent, 3);
		fCodePath.doFillIntoGrid(parent, 3);
		fArgs.doFillIntoGrid(parent, 3);

		final Text t = fName.getTextControl(parent);
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
		fName.setFocus();
	}

	private void initializeFields() {
		if (fEditedBackend == null) {
			fName.setText(""); //$NON-NLS-1$
			fNodeName.setText("");
			fCookie.setText("");
			fRuntime.setText(""); //$NON-NLS-1$
			fCodePath.setElements(new ArrayList<String>(5));
			fArgs.setText(""); //$NON-NLS-1$
		} else {
			fName.setText(fEditedBackend.getName());
			fNodeName.setText(fEditedBackend.getNodeName());
			fCookie.setText(fEditedBackend.getCookie());
			fRuntime.setText(fEditedBackend.getRuntime());
			fCodePath.setElements(fEditedBackend.getCodePath());
			fArgs.setText(fEditedBackend.getArgs());
		}
		setNameStatus(validateName());
		setRuntimeStatus(validateRuntime());
		updateStatusLine();
	}

	protected IStatus validateName() {
		final StatusInfo status = new StatusInfo();
		final String name = fName.getText();
		if (name == null || name.trim().length() == 0) {
			status.setError("Enter the backend's name"); //$NON-NLS-1$
		} else {
			if (fRequestor.isDuplicateName(name)
					&& (fEditedBackend == null || !name.equals(fEditedBackend
							.getName()))) {
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

	protected IStatus validateRuntime() {
		final StatusInfo status = new StatusInfo();
		final String runtime = fRuntime.getItems()[fRuntime.getSelectionIndex()];
		if (runtime == null || runtime.trim().length() == 0) {
			status.setError("Choose a runtime"); //$NON-NLS-1$
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
		dialog.setFilterPath(fRuntime.getText());
		dialog
				.setMessage(PreferenceMessages.addRuntimeDialog_pickRuntimeRootDialog_message);
		final String newPath = dialog.open();
		if (newPath != null) {
			fRuntime.setText(newPath);
		}
	}

	@Override
	protected void okPressed() {
		doOkPressed();
		super.okPressed();
	}

	private void doOkPressed() {
		if (fEditedBackend == null) {
			final BackendInfo runtime = new BackendInfo();
			storeValues(runtime);
			fRequestor.itemAdded(runtime);
		} else {
			storeValues(fEditedBackend);
		}
	}

	protected void storeValues(BackendInfo vm) {
		vm.setRuntime(fRuntime.getItems()[fRuntime.getSelectionIndex()]);
		vm.setName(fName.getText());
		vm.setNodeName(fNodeName.getText());
		vm.setCookie(fCookie.getText());
		vm.setCodePath(fCodePath.getElements());
		final String argString = fArgs.getText().trim();
		vm.setArgs(argString);
	}

	protected void setNameStatus(IStatus status) {
		fStatuses[0] = status;
	}

	protected void setRuntimeStatus(IStatus status) {
		fStatuses[1] = status;
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
