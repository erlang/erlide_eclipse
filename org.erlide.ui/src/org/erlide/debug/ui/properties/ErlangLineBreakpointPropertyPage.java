package org.erlide.debug.ui.properties;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.model.ILineBreakpoint;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.PlatformUI;
import org.erlide.core.model.root.IErlElement;
import org.erlide.debug.ui.utils.BreakpointUtils;
import org.erlide.jinterface.ErlLogger;
import org.erlide.launch.debug.IErlangBreakpoint;
import org.erlide.ui.editors.erl.IErlangHelpContextIds;

public class ErlangLineBreakpointPropertyPage extends
        ErlangBreakpointPropertyPage {

    Button fEnableConditionButton;
    private BreakpointConditionEditor fConditionEditor;

    // private Button fConditionIsTrue;
    // private Button fConditionHasChanged;

    // private Label fSuspendWhenLabel;
    // // Watchpoint editors
    // private Button fFieldAccess;
    // private Button fFieldModification;
    // // Method breakpoint editors
    // private Button fMethodEntry;
    // private Button fMethodExit;

    @Override
    protected void doStore() throws CoreException {
        final IErlangBreakpoint breakpoint = getBreakpoint();
        super.doStore();
        if (fConditionEditor != null) {
            final boolean enableCondition = fEnableConditionButton
                    .getSelection();
            final String condition = fConditionEditor.getCondition();
            // final boolean suspendOnTrue = fConditionIsTrue.getSelection();
            if (breakpoint.isConditionEnabled() != enableCondition) {
                breakpoint.setConditionEnabled(enableCondition);
            }
            if (!condition.equals(breakpoint.getCondition())) {
                breakpoint.setCondition(condition);
            }
            // if (breakpoint.isConditionSuspendOnTrue() != suspendOnTrue) {
            // breakpoint.setConditionSuspendOnTrue(suspendOnTrue);
            // }
        }
        // if (breakpoint instanceof IJavaWatchpoint) {
        // final IJavaWatchpoint watchpoint = (IJavaWatchpoint) getBreakpoint();
        // final boolean access = fFieldAccess.getSelection();
        // final boolean modification = fFieldModification.getSelection();
        // if (access != watchpoint.isAccess()) {
        // watchpoint.setAccess(access);
        // }
        // if (modification != watchpoint.isModification()) {
        // watchpoint.setModification(modification);
        // }
        // }
        // if (breakpoint instanceof IJavaMethodBreakpoint) {
        // final IJavaMethodBreakpoint methodBreakpoint =
        // (IJavaMethodBreakpoint) getBreakpoint();
        // final boolean entry = fMethodEntry.getSelection();
        // final boolean exit = fMethodExit.getSelection();
        // if (entry != methodBreakpoint.isEntry()) {
        // methodBreakpoint.setEntry(entry);
        // }
        // if (exit != methodBreakpoint.isExit()) {
        // methodBreakpoint.setExit(exit);
        // }
        // }
    }

    @Override
    protected void createTypeSpecificLabels(final Composite parent) {
        createLabel(parent, "Module:");
        final String moduleName = getBreakpoint().getMarker().getResource()
                .getName();
        createText(parent, SWT.READ_ONLY, moduleName).setBackground(
                parent.getBackground());
        // Line number
        final ILineBreakpoint breakpoint = (ILineBreakpoint) getBreakpoint();
        final StringBuffer lineNumber = new StringBuffer(4);
        try {
            final int lNumber = breakpoint.getLineNumber();
            if (lNumber > 0) {
                lineNumber.append(lNumber);
            }
        } catch (final CoreException ce) {
            ErlLogger.error(ce);
        }
        if (lineNumber.length() > 0) {
            createLabel(parent, "&Line Number:");
            final String string = lineNumber.toString();
            createText(parent, SWT.READ_ONLY, string).setBackground(
                    parent.getBackground());
        }
        final IErlElement element = BreakpointUtils.getElement(breakpoint);
        if (element == null) {
            return;
        }
        createLabel(parent, "Function:");
        createText(parent, SWT.READ_ONLY, element.toString()).setBackground(
                parent.getBackground());
    }

    /**
     * Create the condition editor and associated editors.
     * 
     * @see org.eclipse.jdt.internal.debug.ui.propertypages.JavaBreakpointPage#createTypeSpecificEditors(org.eclipse.swt.widgets.Composite)
     */
    @Override
    protected void createTypeSpecificEditors(final Composite parent)
            throws CoreException {
        setTitle("Line Breakpoint");
        final IErlangBreakpoint breakpoint = getBreakpoint();
        if (breakpoint.supportsCondition()) {
            createConditionEditor(parent);
        }
        // if (breakpoint instanceof IJavaWatchpoint) {
        // setTitle(PropertyPageMessages.JavaLineBreakpointPage_19);
        // final IJavaWatchpoint watchpoint = (IJavaWatchpoint) getBreakpoint();
        // final SelectionAdapter watchpointValidator = new SelectionAdapter() {
        // @Override
        // public void widgetSelected(final SelectionEvent e) {
        // validateWatchpoint();
        // }
        // };
        // createLabel(parent, PropertyPageMessages.JavaLineBreakpointPage_6);
        // fEnabledButton.addSelectionListener(watchpointValidator);
        // fFieldAccess = createCheckButton(parent,
        // PropertyPageMessages.JavaLineBreakpointPage_7);
        // fFieldAccess.setSelection(watchpoint.isAccess());
        // fFieldAccess.addSelectionListener(watchpointValidator);
        // fFieldModification = createCheckButton(parent,
        // PropertyPageMessages.JavaLineBreakpointPage_8);
        // fFieldModification.setSelection(watchpoint.isModification());
        // fFieldModification.addSelectionListener(watchpointValidator);
        // }
        // if (breakpoint instanceof IJavaMethodBreakpoint) {
        // setTitle(PropertyPageMessages.JavaLineBreakpointPage_20);
        // final IJavaMethodBreakpoint methodBreakpoint =
        // (IJavaMethodBreakpoint) getBreakpoint();
        // final SelectionAdapter methodBreakpointValidator = new
        // SelectionAdapter() {
        // @Override
        // public void widgetSelected(final SelectionEvent e) {
        // validateMethodBreakpoint();
        // }
        // };
        // createLabel(parent, PropertyPageMessages.JavaLineBreakpointPage_9);
        // fEnabledButton.addSelectionListener(methodBreakpointValidator);
        // fMethodEntry = createCheckButton(parent,
        // PropertyPageMessages.JavaLineBreakpointPage_10);
        // fMethodEntry.setSelection(methodBreakpoint.isEntry());
        // fMethodEntry.addSelectionListener(methodBreakpointValidator);
        // fMethodExit = createCheckButton(parent,
        // PropertyPageMessages.JavaLineBreakpointPage_11);
        // fMethodExit.setSelection(methodBreakpoint.isExit());
        // fMethodExit.addSelectionListener(methodBreakpointValidator);
        // }
    }

    /**
     * Validates the watchpoint...if we are one
     */
    // private void validateWatchpoint() {
    // if (fEnabledButton.getSelection()
    // && !(fFieldAccess.getSelection() || fFieldModification
    // .getSelection())) {
    // addErrorMessage(fgWatchpointError);
    // } else {
    // removeErrorMessage(fgWatchpointError);
    // }
    // }
    /**
     * Validates the method breakpoint, if we are one
     */
    // private void validateMethodBreakpoint() {
    // boolean valid = true;
    // if (fEnabledButton.getSelection()
    // && !(fMethodEntry.getSelection() || fMethodExit.getSelection())) {
    // setErrorMessage(fgMethodBreakpointError);
    // valid = false;
    // } else {
    // setErrorMessage(null);
    // }
    // setValid(valid);
    // }
    /**
     * Creates the controls that allow the user to specify the breakpoint's
     * condition
     * 
     * @param parent
     *            the composite in which the condition editor should be created
     * @throws CoreException
     *             if an exception occurs accessing the breakpoint
     */
    private void createConditionEditor(final Composite parent)
            throws CoreException {
        final IErlangBreakpoint breakpoint = getBreakpoint();
        // String label = null;
        // if (BreakpointUtils.getType(breakpoint) != null) {
        // final IBindingService bindingService = (IBindingService) PlatformUI
        // .getWorkbench().getAdapter(IBindingService.class);
        // if (bindingService != null) {
        // final TriggerSequence keyBinding = bindingService
        // .getBestActiveBindingFor(ITextEditorActionDefinitionIds.CONTENT_ASSIST_PROPOSALS);
        // if (keyBinding != null) {
        // label = MessageFormat.format(
        // PropertyPageMessages.JavaLineBreakpointPage_12,
        // new String[] { keyBinding.format() });
        // }
        // }
        // }
        // if (label == null) {
        // label = PropertyPageMessages.JavaLineBreakpointPage_13;
        // }
        final Group g = new Group(parent, SWT.NONE);
        g.setLayout(new GridLayout(1, false));
        g.setText(EMPTY_STRING);
        g.setFont(parent.getFont());
        final GridData gd = new GridData(GridData.FILL_BOTH);
        gd.horizontalSpan = 1;
        g.setLayoutData(gd);
        final Composite conditionComposite = g;
        fEnableConditionButton = createCheckButton(conditionComposite, "Enable");
        fEnableConditionButton.setSelection(breakpoint.isConditionEnabled());
        fEnableConditionButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                setConditionEnabled(fEnableConditionButton.getSelection());
            }
        });
        fConditionEditor = new BreakpointConditionEditor(conditionComposite,
                this);
        // fSuspendWhenLabel = createLabel(conditionComposite,
        // PropertyPageMessages.JavaLineBreakpointPage_15);
        // fConditionIsTrue = createRadioButton(conditionComposite,
        // "condition is 'tr&ue'");
        // fConditionHasChanged = createRadioButton(conditionComposite,
        // "value of condition ch&anges");
        // if (breakpoint.isConditionSuspendOnTrue()) {
        // fConditionIsTrue.setSelection(true);
        // } else {
        // fConditionHasChanged.setSelection(true);
        // }
        setConditionEnabled(fEnableConditionButton.getSelection());
    }

    /**
     * Sets the enabled state of the condition editing controls.
     * 
     * @param enabled
     */
    void setConditionEnabled(final boolean enabled) {
        fConditionEditor.setEnabled(enabled);
        // fSuspendWhenLabel.setEnabled(enabled);
        // fConditionIsTrue.setEnabled(enabled);
        // fConditionHasChanged.setEnabled(enabled);
    }

    @Override
    public int convertHeightInCharsToPixels(final int chars) {
        return super.convertHeightInCharsToPixels(chars);
    }

    @Override
    public int convertWidthInCharsToPixels(final int chars) {
        return super.convertWidthInCharsToPixels(chars);
    }

    @Override
    public void dispose() {
        if (fConditionEditor != null) {
            fConditionEditor.dispose();
        }
        super.dispose();
    }

    @Override
    public void createControl(final Composite parent) {
        super.createControl(parent);
        PlatformUI
                .getWorkbench()
                .getHelpSystem()
                .setHelp(
                        getControl(),
                        IErlangHelpContextIds.ERLANG_LINE_BREAKPOINT_PROPERTY_PAGE);
    }
}
