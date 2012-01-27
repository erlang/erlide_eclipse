package org.erlide.debug.ui.properties;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.PropertyPage;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.erlide.jinterface.ErlLogger;
import org.erlide.launch.debug.IErlangBreakpoint;
import org.erlide.ui.editors.erl.IErlangHelpContextIds;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.util.PixelConverter;

public class ErlangBreakpointPropertyPage extends PropertyPage {

    protected Button fEnabledButton;
    protected Button fHitCountButton;
    // protected Text fHitCountText;
    protected Combo fBreakActionCombo;
    protected List<String> fErrorMessages = new ArrayList<String>();

    /**
     * Attribute used to indicate that a breakpoint should be deleted when
     * cancel is pressed.
     */
    public static final String ATTR_DELETE_ON_CANCEL = ErlideUIPlugin.PLUGIN_ID
            + ".ATTR_DELETE_ON_CANCEL"; //$NON-NLS-1$

    /**
     * Constant for the empty string
     */
    protected static final String EMPTY_STRING = ""; //$NON-NLS-1$

    /**
     * the hit count error message
     */
    // private static final String fgHitCountErrorMessage =
    // "Hit count must be a positive integer";

    /**
     * Store the breakpoint properties.
     * 
     * @see org.eclipse.jface.preference.IPreferencePage#performOk()
     */
    @Override
    public boolean performOk() {
        final IWorkspaceRunnable wr = new IWorkspaceRunnable() {
            @Override
            public void run(final IProgressMonitor monitor)
                    throws CoreException {
                final IErlangBreakpoint breakpoint = getBreakpoint();
                final boolean delOnCancel = breakpoint.getMarker()
                        .getAttribute(ATTR_DELETE_ON_CANCEL) != null;
                if (delOnCancel) {
                    // if this breakpoint is being created, remove the
                    // "delete on cancel" attribute
                    // and register with the breakpoint manager
                    breakpoint.getMarker().setAttribute(ATTR_DELETE_ON_CANCEL,
                            (String) null);
                    breakpoint.setRegistered(true);
                }
                doStore();
            }
        };
        try {
            ResourcesPlugin.getWorkspace().run(wr, null, 0, null);
        } catch (final CoreException e) {
            ErlLogger.error(e);
        }
        return super.performOk();
    }

    /**
     * Adds the given error message to the errors currently displayed on this
     * page. The page displays the most recently added error message. Clients
     * should retain messages that are passed into this method as the message
     * should later be passed into removeErrorMessage(String) to clear the
     * error. This method should be used instead of setErrorMessage(String).
     * 
     * @param message
     *            the error message to display on this page.
     */
    protected void addErrorMessage(final String message) {
        fErrorMessages.remove(message);
        fErrorMessages.add(message);
        setErrorMessage(message);
        setValid(message == null);
    }

    /**
     * Removes the given error message from the errors currently displayed on
     * this page. When an error message is removed, the page displays the error
     * that was added before the given message. This is akin to popping the
     * message from a stack. Clients should call this method instead of
     * setErrorMessage(null).
     * 
     * @param message
     *            the error message to clear
     */
    protected void removeErrorMessage(final String message) {
        fErrorMessages.remove(message);
        if (fErrorMessages.isEmpty()) {
            addErrorMessage(null);
        } else {
            addErrorMessage(fErrorMessages.get(fErrorMessages.size() - 1));
        }
    }

    /**
     * Stores the values configured in this page. This method should be called
     * from within a workspace runnable to reduce the number of resource deltas.
     */
    protected void doStore() throws CoreException {
        final IErlangBreakpoint breakpoint = getBreakpoint();
        // storeHitCount(breakpoint);
        storeEnabled(breakpoint);
        storeBreakAction(breakpoint);
    }

    /**
     * Stores the value of the enabled state in the breakpoint.
     * 
     * @param breakpoint
     *            the breakpoint to update
     * @throws CoreException
     *             if an exception occurs while setting the enabled state
     */
    private void storeEnabled(final IErlangBreakpoint breakpoint)
            throws CoreException {
        breakpoint.setEnabled(fEnabledButton.getSelection());
    }

    /**
     * Stores the value of the suspend policy in the breakpoint.
     * 
     * @param breakpoint
     *            the breakpoint to update
     * @throws CoreException
     *             if an exception occurs while setting the suspend policy
     */
    private void storeBreakAction(final IErlangBreakpoint breakpoint)
            throws CoreException {
        breakpoint.setBreakAction(fBreakActionCombo.getSelectionIndex());
    }

    /**
     * Stores the value of the hit count in the breakpoint.
     * 
     * @param breakpoint
     *            the breakpoint to update
     * @throws CoreException
     *             if an exception occurs while setting the hit count
     */
    // private void storeHitCount(final IErlangBreakpoint breakpoint)
    // throws CoreException {
    // int hitCount = -1;
    // if (fHitCountButton.getSelection()) {
    // try {
    // hitCount = Integer.parseInt(fHitCountText.getText());
    // } catch (final NumberFormatException e) {
    // ErlLogger.warn(e);
    // // JDIDebugUIPlugin
    // // .log(new Status(
    // // IStatus.ERROR,
    // // JDIDebugUIPlugin.getUniqueIdentifier(),
    // // IStatus.ERROR,
    // // MessageFormat
    // // .format(
    //				//												"ErlangBreakpointPage allowed input of invalid string for hit count value: {0}.", new String[] { fHitCountText.getText() }), e)); //$NON-NLS-1$
    // }
    // }
    // breakpoint.setHitCount(hitCount);
    // }
    /**
     * Creates the labels and editors displayed for the breakpoint.
     * 
     * @see org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.swt.widgets.Composite)
     */
    @Override
    protected Control createContents(final Composite parent) {
        setTitle("<erlang breakpoint page, should be overridden>");
        noDefaultAndApplyButton();
        final Composite mainComposite = createComposite(parent, 1);
        createLabels(mainComposite);
        try {
            createEnabledButton(mainComposite);
            // createHitCountEditor(mainComposite);
            createTypeSpecificEditors(mainComposite);
            createActionCombo(mainComposite);
            // considered uncommon.
            // Add it last.
        } catch (final CoreException e) {
            ErlLogger.error(e);
        }
        setValid(true);
        // if this breakpoint is being created, change the shell title to
        // indicate 'creation'
        try {
            if (getBreakpoint().getMarker().getAttribute(ATTR_DELETE_ON_CANCEL) != null) {
                getShell().addShellListener(new ShellListener() {
                    @Override
                    public void shellActivated(final ShellEvent e) {
                        final Shell shell = (Shell) e.getSource();
                        shell.setText(MessageFormat.format(
                                "Create Breakpoint for {0}",
                                new Object[] { getName(getBreakpoint()) }));
                        shell.removeShellListener(this);
                    }

                    @Override
                    public void shellClosed(final ShellEvent e) {
                    }

                    @Override
                    public void shellDeactivated(final ShellEvent e) {
                    }

                    @Override
                    public void shellDeiconified(final ShellEvent e) {
                    }

                    @Override
                    public void shellIconified(final ShellEvent e) {
                    }
                });
            }
        } catch (final CoreException e) {
        }
        return mainComposite;
    }

    private void createActionCombo(final Composite parent) {
        final Composite comp = createComposite(parent, 2);
        createLabel(comp, "Break Action");
        fBreakActionCombo = new Combo(comp, SWT.BORDER | SWT.READ_ONLY);
        fBreakActionCombo.add("Break");
        fBreakActionCombo.add("Trace And Continue");
        fBreakActionCombo.select(getBreakpoint().getBreakAction());
    }

    /**
     * Returns the name of the given element.
     * 
     * @param element
     *            the element
     * @return the name of the element
     */
    String getName(final IAdaptable element) {
        final IWorkbenchAdapter adapter = (IWorkbenchAdapter) element
                .getAdapter(IWorkbenchAdapter.class);
        if (adapter != null) {
            return adapter.getLabel(element);
        }
        return EMPTY_STRING;
    }

    /**
     * Creates the labels displayed for the breakpoint.
     * 
     * @param parent
     */
    protected void createLabels(final Composite parent) {
        final Composite labelComposite = createComposite(parent, 2);
        // final String typeName = getBreakpoint().getTypeName();
        // if (typeName != null) {
        // createLabel(labelComposite, "&Type:");
        // final Text t = new Text(labelComposite, SWT.READ_ONLY);
        // t.setFont(labelComposite.getFont());
        // final GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        // gd.horizontalSpan = 1;
        // t.setLayoutData(gd);
        // t.setText(typeName);
        // t.setBackground(parent.getBackground());
        // }
        createTypeSpecificLabels(labelComposite);
    }

    // /**
    // * @param parent
    // * the composite in which the hit count editor will be created
    // */
    // private void createHitCountEditor(final Composite parent)
    // throws CoreException {
    // final Composite hitCountComposite = createComposite(parent, 2);
    // fHitCountButton = createCheckButton(hitCountComposite, "&Hit Count:");
    // fHitCountButton.addSelectionListener(new SelectionAdapter() {
    // @Override
    // public void widgetSelected(final SelectionEvent event) {
    // fHitCountText.setEnabled(fHitCountButton.getSelection());
    // hitCountChanged();
    // }
    // });
    // final int hitCount = getBreakpoint().getHitCount();
    // String hitCountString = EMPTY_STRING;
    // if (hitCount > 0) {
    // hitCountString = new Integer(hitCount).toString();
    // fHitCountButton.setSelection(true);
    // } else {
    // fHitCountButton.setSelection(false);
    // }
    // fHitCountText = createText(hitCountComposite, hitCountString);
    // if (hitCount <= 0) {
    // fHitCountText.setEnabled(false);
    // }
    // fHitCountText.addModifyListener(new ModifyListener() {
    // public void modifyText(final ModifyEvent e) {
    // hitCountChanged();
    // }
    // });
    // }

    // /**
    // * Validates the current state of the hit count editor. Hit count value
    // must
    // * be a positive integer.
    // */
    // private void hitCountChanged() {
    // if (!fHitCountButton.getSelection()) {
    // removeErrorMessage(fgHitCountErrorMessage);
    // return;
    // }
    // final String hitCountText = fHitCountText.getText();
    // int hitCount = -1;
    // try {
    // hitCount = Integer.parseInt(hitCountText);
    // } catch (final NumberFormatException e1) {
    // addErrorMessage(fgHitCountErrorMessage);
    // return;
    // }
    // if (hitCount < 1) {
    // addErrorMessage(fgHitCountErrorMessage);
    // } else {
    // removeErrorMessage(fgHitCountErrorMessage);
    // }
    // }

    /**
     * Creates the button to toggle enablement of the breakpoint
     * 
     * @param parent
     * @throws CoreException
     */
    protected void createEnabledButton(final Composite parent)
            throws CoreException {
        fEnabledButton = createCheckButton(parent, "&Enabled");
        fEnabledButton.setSelection(getBreakpoint().isEnabled());
    }

    /**
     * Returns the breakpoint that this preference page configures
     * 
     * @return the breakpoint this page configures
     */
    protected IErlangBreakpoint getBreakpoint() {
        return (IErlangBreakpoint) getElement();
    }

    /**
     * Allows subclasses to add type specific labels to the common Erlang
     * breakpoint page.
     * 
     * @param parent
     */
    protected void createTypeSpecificLabels(final Composite parent) {
    }

    /**
     * Allows subclasses to add type specific editors to the common Erlang
     * breakpoint page.
     * 
     * @param parent
     */
    protected void createTypeSpecificEditors(final Composite parent)
            throws CoreException {
    }

    /**
     * Creates a fully configured text editor with the given initial value
     * 
     * @param parent
     * @param initialValue
     * @return the configured text editor
     */
    protected Text createText(final Composite parent, final String initialValue) {
        return createText(parent, SWT.BORDER | SWT.SINGLE, initialValue);
    }

    protected Text createText(final Composite parent, final int style,
            final String initialValue) {
        final Text t = new Text(parent, style);
        t.setFont(parent.getFont());
        final GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = 1;
        t.setLayoutData(gd);
        t.setText(initialValue);
        return t;
    }

    /**
     * Creates a fully configured composite with the given number of columns
     * 
     * @param parent
     * @param numColumns
     * @return the configured composite
     */
    protected Composite createComposite(final Composite parent,
            final int numColumns) {
        final Composite g = new Composite(parent, SWT.NONE);
        final GridLayout layout = new GridLayout(numColumns, false);
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        g.setLayout(layout);
        g.setFont(parent.getFont());
        final GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = 1;
        g.setLayoutData(gd);
        return g;
    }

    /**
     * Creates a fully configured check button with the given text.
     * 
     * @param parent
     *            the parent composite
     * @param text
     *            the label of the returned check button
     * @return a fully configured check button
     */
    protected Button createCheckButton(final Composite parent, final String text) {
        final Button button = new Button(parent, SWT.CHECK);
        button.setFont(parent.getFont());
        button.setSelection(false);
        button.setText(text);
        final GridData gd = new GridData();
        gd.horizontalSpan = 1;
        button.setLayoutData(gd);
        setButtonDimensionHint(button);
        return button;
    }

    private void setButtonDimensionHint(final Button button) {
        Assert.isNotNull(button);
        final Object o = button.getLayoutData();
        if (o instanceof GridData) {
            final GridData gd = (GridData) o;
            button.setFont(JFaceResources.getDialogFont());
            final PixelConverter converter = new PixelConverter(button);
            final int widthHint1 = converter
                    .convertHorizontalDLUsToPixels(IDialogConstants.BUTTON_WIDTH);
            gd.widthHint = Math.max(widthHint1,
                    button.computeSize(SWT.DEFAULT, SWT.DEFAULT, true).x);
            gd.horizontalAlignment = GridData.FILL;
        }
    }

    /**
     * Creates a fully configured label with the given text.
     * 
     * @param parent
     *            the parent composite
     * @param text
     *            the test of the returned label
     * @return a fully configured label
     */
    protected Label createLabel(final Composite parent, final String text) {
        final Label l = new Label(parent, SWT.NONE);
        l.setFont(parent.getFont());
        l.setText(text);
        final GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = 1;
        gd.grabExcessHorizontalSpace = false;
        l.setLayoutData(gd);
        return l;
    }

    /**
     * Creates a fully configured radio button with the given text.
     * 
     * @param parent
     *            the parent composite
     * @param text
     *            the label of the returned radio button
     * @return a fully configured radio button
     */
    protected Button createRadioButton(final Composite parent, final String text) {
        final Button button = new Button(parent, SWT.RADIO);
        button.setFont(parent.getFont());
        button.setText(text);
        final GridData gd = new GridData();
        button.setLayoutData(gd);
        setButtonDimensionHint(button);
        return button;
    }

    /**
     * Check to see if the breakpoint should be deleted.
     */
    @Override
    public boolean performCancel() {
        try {
            if (getBreakpoint().getMarker().getAttribute(ATTR_DELETE_ON_CANCEL) != null) {
                // if this breakpoint is being created, delete on cancel
                getBreakpoint().delete();
            }
        } catch (final CoreException e) {
            ErlLogger.error(e);
            // JDIDebugUIPlugin.statusDialog(
            // PropertyPageMessages.ErlangBreakpointPage_9, e.getStatus());
        }
        return super.performCancel();
    }

    @Override
    public void createControl(final Composite parent) {
        super.createControl(parent);
        PlatformUI
                .getWorkbench()
                .getHelpSystem()
                .setHelp(getControl(),
                        IErlangHelpContextIds.ERLANG_BREAKPOINT_PROPERTY_PAGE);
    }
}
