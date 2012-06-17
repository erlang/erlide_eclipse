package org.erlide.ui.prefs;

import java.io.File;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.StatusDialog;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.StructuredSelection;
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
import org.erlide.backend.runtimeinfo.RuntimeInfo;
import org.erlide.ui.dialogfields.DialogField;
import org.erlide.ui.dialogfields.IDialogFieldListener;
import org.erlide.ui.dialogfields.IListAdapter;
import org.erlide.ui.dialogfields.IStringButtonAdapter;
import org.erlide.ui.dialogfields.ListDialogField;
import org.erlide.ui.dialogfields.StringButtonDialogField;
import org.erlide.ui.dialogfields.StringDialogField;
import org.erlide.ui.util.StatusInfo;

public class AddRuntimeDialog extends StatusDialog implements
        IListAdapter<String> {

    public static class StringLabelProvider implements ILabelProvider {

        @Override
        public Image getImage(final Object element) {
            return null;
        }

        @Override
        public String getText(final Object element) {
            return (String) element;
        }

        @Override
        public void addListener(final ILabelProviderListener listener) {
        }

        @Override
        public void dispose() {
        }

        @Override
        public boolean isLabelProperty(final Object element,
                final String property) {
            return false;
        }

        @Override
        public void removeListener(final ILabelProviderListener listener) {
        }

    }

    private final IAddDialogRequestor<RuntimeInfo> fRequestor;

    private final RuntimeInfo fEditedRuntime;

    private StringDialogField fName;
    private StringButtonDialogField fOtpHome;
    private ListDialogField<String> fCodePath;
    private StringDialogField fArgs;
    private final boolean returnNew;

    private final IStatus[] fStatuses;

    public AddRuntimeDialog(final IAddDialogRequestor<RuntimeInfo> requestor,
            final Shell shell, final RuntimeInfo editedVM,
            final boolean returnNew) {
        super(shell);
        setShellStyle(getShellStyle() | SWT.RESIZE);
        fRequestor = requestor;
        fStatuses = new IStatus[5];
        for (int i = 0; i < fStatuses.length; i++) {
            fStatuses[i] = new StatusInfo();
        }
        fEditedRuntime = editedVM;
        this.returnNew = returnNew;
    }

    /**
     * @see Windows#configureShell
     */
    @Override
    protected void configureShell(final Shell newShell) {
        super.configureShell(newShell);
        // PlatformUI.getWorkbench().getHelpSystem().setHelp(newShell,
        // IJavaDebugHelpContextIds.EDIT_JRE_DIALOG);
    }

    protected void createFieldListeners() {
        fOtpHome.setDialogFieldListener(new IDialogFieldListener() {

            @Override
            public void dialogFieldChanged(final DialogField field) {
                setLocationStatus(validateLocation());
                updateStatusLine();
            }
        });
        fName.setDialogFieldListener(new IDialogFieldListener() {

            @Override
            public void dialogFieldChanged(final DialogField field) {
                setNameStatus(validateName());
                updateStatusLine();
            }
        });
    }

    protected String getBackendName() {
        return fName.getText();
    }

    @Override
    protected Control createDialogArea(final Composite ancestor) {
        fOtpHome = new StringButtonDialogField(new IStringButtonAdapter() {

            @Override
            public void changeControlPressed(final DialogField field) {
                browseForInstallDir();
            }
        });
        fOtpHome.setLabelText("Location"); //$NON-NLS-1$
        fOtpHome.setButtonLabel("&Browse..."); //$NON-NLS-1$

        fName = new StringDialogField();
        fName.setLabelText(RuntimePreferenceMessages.addDialog_ertsName);

        final String[] buttons = new String[] {
                RuntimePreferenceMessages.addDialog_add,
                RuntimePreferenceMessages.addDialog_remove, "Move up",
                "Move down" };
        fCodePath = new ListDialogField<String>(this, buttons,
                new StringLabelProvider());
        fCodePath.setLabelText("Code path");
        // TODO enable this when it will work (#163)
        fCodePath.setEnabled(false);

        fArgs = new StringDialogField();
        fArgs.setLabelText("E&xtra args"); //$NON-NLS-1$

        final Composite parent = (Composite) super.createDialogArea(ancestor);
        ((GridLayout) parent.getLayout()).numColumns = 3;

        fOtpHome.doFillIntoGrid(parent, 3);
        fName.doFillIntoGrid(parent, 3);
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
        if (fEditedRuntime == null) {
            fName.setText(""); //$NON-NLS-1$
            fOtpHome.setText(""); //$NON-NLS-1$
            fCodePath.setElements(new ArrayList<String>(5));
            fArgs.setText(""); //$NON-NLS-1$
        } else {
            fName.setText(fEditedRuntime.getName());
            fOtpHome.setText(fEditedRuntime.getOtpHome());
            fCodePath.setElements(fEditedRuntime.getCodePath());
            fArgs.setText(fEditedRuntime.getArgs());
        }
        setNameStatus(validateName());
        // setNodeNameStatus(validateNodeName());
        setLocationStatus(validateLocation());
        updateStatusLine();
    }

    protected IStatus validateName() {
        final StatusInfo status = new StatusInfo();
        final String name = fName.getText();
        if (name == null || name.trim().length() == 0) {
            status.setError("Enter the runtime's name"); //$NON-NLS-1$
        } else {
            if (fRequestor.isDuplicateName(name)
                    && (fEditedRuntime == null || !name.equals(fEditedRuntime
                            .getName()))) {
                status.setError("The name is already used"); //$NON-NLS-1$
            } else {
                final IStatus s = ResourcesPlugin.getWorkspace().validateName(
                        name, IResource.FILE);
                if (!s.isOK()) {
                    status.setError(MessageFormat.format("Name is invalid: %s",
                            (Object[]) new String[] { s.getMessage() }));
                }
            }
        }
        return status;
    }

    protected IStatus validateLocation() {
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

    @Override
    protected void okPressed() {
        doOkPressed();
        super.okPressed();
    }

    private void doOkPressed() {
        if (returnNew) {
            final RuntimeInfo info = new RuntimeInfo();
            storeValues(info);
            fRequestor.itemAdded(info);
        } else {
            storeValues(fEditedRuntime);
        }
    }

    protected void storeValues(final RuntimeInfo runtime) {
        runtime.setOtpHome(fOtpHome.getText());
        runtime.setName(fName.getText());
        runtime.setCodePath(fCodePath.getElements());
        final String argString = fArgs.getText().trim();
        runtime.setArgs(argString);
    }

    protected void setNameStatus(final IStatus status) {
        fStatuses[0] = status;
    }

    protected void setNodeNameStatus(final IStatus status) {
        fStatuses[1] = status;
    }

    protected void setLocationStatus(final IStatus status) {
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
    protected void updateButtonsEnableState(final IStatus status) {
        final Button ok = getButton(IDialogConstants.OK_ID);
        if (ok != null && !ok.isDisposed()) {
            ok.setEnabled(status.getSeverity() == IStatus.OK);
        }
    }

    /**
     * @see org.eclipse.jface.dialogs.Dialog#setButtonLayoutData(org.eclipse.swt.widgets.Button)
     */
    @Override
    protected void setButtonLayoutData(final Button button) {
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

    @Override
    public boolean close() {
        // DialogSettingsHelper.persistShellGeometry(getShell(),
        // getDialogSettingsSectionName());
        return super.close();
    }

    @Override
    protected Point getInitialLocation(final Point initialSize) {
        // Point initialLocation = DialogSettingsHelper
        // .getInitialLocation(getDialogSettingsSectionName());
        // if (initialLocation != null)
        // {
        // return initialLocation;
        // }
        return super.getInitialLocation(initialSize);
    }

    @Override
    protected Point getInitialSize() {
        final Point size = super.getInitialSize();
        return size;
        // return DialogSettingsHelper.getInitialSize(
        // getDialogSettingsSectionName(), size);
    }

    @Override
    public void customButtonPressed(final ListDialogField<String> field,
            final int index) {
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
        }
    }

    private void moveDown(final ListDialogField<String> field) {
        final List<String> sel = field.getSelectedElements();
        if (sel.size() == 1) {
            final String value = sel.get(0);
            final int pos = field.getIndexOfElement(value);
            if (pos < field.getSize() - 1) {
                field.removeElement(value);
                field.addElement(value, pos + 1);
            }
            field.selectElements(new StructuredSelection(value));
            field.refresh();
        }
    }

    private void moveUp(final ListDialogField<String> field) {
        final List<String> sel = field.getSelectedElements();
        if (sel.size() == 1) {
            final String value = sel.get(0);
            final int pos = field.getIndexOfElement(value);
            if (pos > 0) {
                field.removeElement(value);
                field.addElement(value, pos - 1);
            }
            field.selectElements(new StructuredSelection(value));
            field.refresh();
        }
    }

    private void removePath(final ListDialogField<String> field) {
        final List<String> sel = field.getSelectedElements();
        field.removeElements(sel);
    }

    private void addPath(final ListDialogField<String> field) {
        final InputDialog dlg = new InputDialog(new Shell(), "Add path",
                "Enter a path to be added to ", "", null);
        dlg.setBlockOnOpen(true);
        dlg.open();
        final String value = dlg.getValue();

        if (value != null && value.length() > 0) {
            final File f = new File(value);
            if (f.exists()) {
                field.addElement(value);
            }
        }
    }

    @Override
    public void doubleClicked(final ListDialogField<String> field) {
    }

    @Override
    public void selectionChanged(final ListDialogField<String> field) {
    }

    protected void browseForInstallDir() {
        final DirectoryDialog dialog = new DirectoryDialog(getShell());
        dialog.setFilterPath(fOtpHome.getText());
        dialog.setMessage(RuntimePreferenceMessages.addDialog_pickInstallationRoot);
        final String newPath = dialog.open();
        if (newPath != null) {
            fOtpHome.setText(newPath);
            final File f = new File(newPath);
            if (fName.getText().equals("")) {
                fName.setText(f.getName());
            }
        }
    }

}
