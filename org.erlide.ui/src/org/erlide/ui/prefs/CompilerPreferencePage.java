/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.prefs;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.dialogs.ControlEnableState;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.eclipse.ui.dialogs.PropertyPage;
import org.erlide.core.builder.CompilerPreferences;
import org.erlide.core.builder.CompilerPreferencesConstants;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlProject;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.backend.ErlBackend;
import org.erlide.jinterface.backend.NoBackendException;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.ErlideBackend;
import org.osgi.service.prefs.BackingStoreException;

public class CompilerPreferencePage extends PropertyPage implements
        IWorkbenchPreferencePage {

    CompilerPreferences prefs;
    private Composite prefsComposite;
    private Composite curGroup;
    private IProject fProject;
    private Button fUseProjectSettings;
    private Link fChangeWorkspaceSettings;
    private Text text;
    protected ControlEnableState fBlockEnableState;

    public CompilerPreferencePage() {
        super();
        setTitle("Compiler options");
        setDescription("Select the compiler options to be used.");
    }

    @Override
    protected Control createContents(final Composite parent) {

        performDefaults();

        prefsComposite = new Composite(parent, SWT.NONE);
        final GridLayout gridLayout_1 = new GridLayout();
        prefsComposite.setLayout(gridLayout_1);

        final String message = "Until we make available a better user interface, \n"
                + "please enter the required options as Erlang terms. \n"
                + "For example, \n\n    export_all, {d, 'DEBUG'}\n\n"
                + "Enclosing all options in a list is also accepted.\n\n"
                + "Note: Some options may not make sense here, for example \n"
                + "'outdir' and will be filtered out.\n";
        final Link lblCompilerOptions = new Link(prefsComposite, SWT.NONE);
        lblCompilerOptions.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                final MessageBox box = new MessageBox(parent.getShell());
                box.setMessage(message);
                box.setText("Compiler options format");
                box.open();
            }
        });
        lblCompilerOptions
                .setText("Compiler options in Erlang format       <a>Important info...</a>");
        lblCompilerOptions.setToolTipText(message);

        text = new Text(prefsComposite, SWT.BORDER | SWT.WRAP | SWT.MULTI);
        text.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                final String txt = text.getText().trim();
                final OptionStatus optionsAreOk = optionsAreOk(txt);
                setValid(optionsAreOk == OptionStatus.OK);
                prefs.setAllOptions(txt);
                switch (optionsAreOk) {
                case OK:
                    setErrorMessage(null);
                    break;
                case ERROR:
                    setErrorMessage("Malformed options (not Erlang terms)");
                    break;
                case NO_RUNTIME:
                    setErrorMessage("No Erlang runtime is defined, go to 'Installed runtimes' preferences");
                    break;
                }
            }
        });
        {
            final GridData gridData = new GridData(SWT.LEFT, SWT.FILL, true,
                    true, 1, 1);
            gridData.widthHint = 386;
            gridData.heightHint = 80;
            text.setLayoutData(gridData);
        }
        text.setText(prefs.getAllOptions());

        final Label label = new Label(prefsComposite, SWT.SEPARATOR
                | SWT.HORIZONTAL);
        {
            final GridData gridData = new GridData(SWT.LEFT, SWT.CENTER, false,
                    false, 1, 1);
            gridData.widthHint = 399;
            label.setLayoutData(gridData);
        }

        final Group optionsGroup = new Group(prefsComposite, SWT.NONE);
        {
            final GridData gridData = new GridData(SWT.LEFT, SWT.CENTER, false,
                    false, 1, 1);
            gridData.widthHint = 397;
            optionsGroup.setLayoutData(gridData);
        }
        optionsGroup.setLayout(new GridLayout(2, false));
        curGroup = optionsGroup;
        newCheckButton("Debug info", "Include debug info",
                CompilerPreferencesConstants.DEBUG_INFO);
        newCheckButton("Export all", "Export all defined functions",
                CompilerPreferencesConstants.EXPORT_ALL);
        newCheckButton("Encrypt debug info",
                "Encrypt debug info, the key will be read from .erlang.crypt",
                CompilerPreferencesConstants.ENCRYPT_DEBUG_INFO);

        final Group warningsGroup = new Group(prefsComposite, SWT.NONE);
        {
            final GridData gridData = new GridData(SWT.LEFT, SWT.FILL, false,
                    false, 1, 1);
            gridData.widthHint = 401;
            warningsGroup.setLayoutData(gridData);
        }
        warningsGroup.setText("Warnings");
        final GridLayout gridLayout = new GridLayout(2, false);
        warningsGroup.setLayout(gridLayout);
        curGroup = warningsGroup;
        newCheckButton(
                "invalid format strings",
                "Malformed format strings as arguments to io:format and similar functions",
                CompilerPreferencesConstants.WARN_FORMAT_STRINGS);
        newCheckButtonInvertedOption("deprecated functions",
                "Call to a function known by the compiler to be deprecated",
                CompilerPreferencesConstants.NOWARN_DEPRECATED_FUNCTIONS);

        newCheckButtonInvertedOption(
                "Name Clashes with Built-in",
                "an exported function with the same name as an auto-imported BIF (such as size/1)\n AND there is a call to it without a qualifying module name.",
                CompilerPreferencesConstants.NOWARN_BIF_CLASH);
        newCheckButton("Obsolete Guards",
                "Old type testing BIFs such as pid/1 and list/1",
                CompilerPreferencesConstants.WARN_OBSOLETE_GUARD);
        newCheckButton("Use of export_all", "The compiler option export_all",
                CompilerPreferencesConstants.WARN_EXPORT_ALL);
        newCheckButton("Unused Imports", "Unused imported functions",
                CompilerPreferencesConstants.WARN_UNUSED_IMPORT);

        // final Button variablesExportedOutsideButton = new
        // Button(warningsGroup,
        // SWT.CHECK);
        // variablesExportedOutsideButton.setEnabled(false);
        // variablesExportedOutsideButton
        // .setToolTipText("implicitly exported variables referred to after the primitives where they were first defined");
        // variablesExportedOutsideButton.setSelection(true);
        // variablesExportedOutsideButton.setText("exported implicit variables");
        //
        // final Button unusedVariablesButton = new Button(warningsGroup,
        // SWT.CHECK);
        // unusedVariablesButton.setEnabled(false);
        // unusedVariablesButton
        // .setToolTipText("variables which are not used, with the exception of variables beginning with an underscore");
        // unusedVariablesButton.setSelection(true);
        // unusedVariablesButton.setText("unused variables");
        //
        // newCheckButtonInvertedOption(
        // "Shadows Variables",
        // "Variable name in fun or list comprehension shadows already defined variable",
        // CompilerPreferencesConstants.NOWARN_SHADOW_VARS);
        //
        // final Button unusedRecordsButton = new Button(warningsGroup,
        // SWT.CHECK);
        // unusedRecordsButton.setEnabled(false);
        // unusedRecordsButton
        // .setToolTipText("unused locally defined record types");
        // unusedRecordsButton.setSelection(true);
        // unusedRecordsButton.setText("unused records");
        //
        // final Button unusedFunctionsButton = new Button(warningsGroup,
        // SWT.CHECK);
        // unusedFunctionsButton.setEnabled(false);
        // unusedFunctionsButton
        // .setToolTipText("local functions that are not called directly or indirectly by an exported function");
        // unusedFunctionsButton.setSelection(true);
        // unusedFunctionsButton.setText("unused functions");
        // new Label(warningsGroup, SWT.NONE);
        //
        // final Button warnForSourceButton = new Button(prefsComposite,
        // SWT.CHECK);
        // warnForSourceButton.setEnabled(false);
        // warnForSourceButton
        // .setText("Warn for source files not on the project's source path");
        // warnForSourceButton.setSelection(prefs.doWarnModuleNotOnSourcePath());

        if (isProjectPreferencePage()) {
            final boolean useProjectSettings = hasProjectSpecificOptions(fProject);
            enableProjectSpecificSettings(useProjectSettings);
        }

        return prefsComposite;
    }

    protected boolean hasProjectSpecificOptions(final IProject project) {
        final CompilerPreferences p = new CompilerPreferences(project);
        return p.hasOptionsAtLowestScope();
    }

    private boolean isProjectPreferencePage() {
        return fProject != null;
    }

    @Override
    protected Label createDescriptionLabel(final Composite parent) {
        createProjectSpecificSettingsCheckBoxAndLink(parent);
        return super.createDescriptionLabel(parent);
    }

    protected void enableProjectSpecificSettings(
            final boolean useProjectSpecificSettings) {
        fUseProjectSettings.setSelection(useProjectSpecificSettings);
        enablePreferenceContent(useProjectSpecificSettings);
        fChangeWorkspaceSettings.setEnabled(!useProjectSpecificSettings);
        // doStatusChanged();
    }

    private void enablePreferenceContent(
            final boolean useProjectSpecificSettings) {
        if (useProjectSpecificSettings) {
            if (fBlockEnableState != null) {
                fBlockEnableState.restore();
                fBlockEnableState = null;
            }
        } else {
            if (fBlockEnableState == null) {
                fBlockEnableState = ControlEnableState.disable(prefsComposite);
            }
        }
    }

    private void createProjectSpecificSettingsCheckBoxAndLink(
            final Composite parent) {
        if (isProjectPreferencePage()) {
            final Composite composite = new Composite(parent, SWT.NONE);
            composite.setFont(parent.getFont());
            final GridLayout layout = new GridLayout(2, false);
            layout.marginHeight = 0;
            layout.marginWidth = 0;
            composite.setLayout(layout);
            composite.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true,
                    false));

            // final IDialogFieldListener listener = new IDialogFieldListener()
            // {
            // public void dialogFieldChanged(final DialogField field) {
            // final boolean enabled = ((SelectionButtonDialogField) field)
            // .isSelected();
            // enableProjectSpecificSettings(enabled);
            //
            // if (enabled && getData() != null) {
            // applyData(getData());
            // }
            // }
            // };

            fUseProjectSettings = new Button(composite, SWT.CHECK);
            fUseProjectSettings.setText("Enable project specific settings");
            fUseProjectSettings.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(final SelectionEvent e) {
                    final boolean sel = fUseProjectSettings.getSelection();
                    enableProjectSpecificSettings(sel);
                    super.widgetSelected(e);
                }
            });
            // fUseProjectSettings.setDialogFieldListener(listener);
            // fUseProjectSettings
            // .setLabelText(PreferencesMessages.PropertyAndPreferencePage_useprojectsettings_label);
            // LayoutUtil.setHorizontalGrabbing(fUseProjectSettings
            // .getSelectionButton(null));

            if (true) { // if (offerLink()) {
                fChangeWorkspaceSettings = createLink(composite,
                        "Configure Workspace settings...");
                fChangeWorkspaceSettings.setLayoutData(new GridData(SWT.END,
                        SWT.CENTER, false, false));
            }
            // else {
            // LayoutUtil.setHorizontalSpan(fUseProjectSettings
            // .getSelectionButton(null), 2);
            // }

            final Label horizontalLine = new Label(composite, SWT.SEPARATOR
                    | SWT.HORIZONTAL);
            horizontalLine.setLayoutData(new GridData(GridData.FILL,
                    GridData.FILL, true, false, 2, 1));
            horizontalLine.setFont(composite.getFont());
        } else { // if (supportsProjectSpecificOptions() && offerLink()) {
            fChangeWorkspaceSettings = createLink(parent,
                    "Configure project specific settings..");
            fChangeWorkspaceSettings.setLayoutData(new GridData(SWT.END,
                    SWT.CENTER, true, false));
        }

    }

    private Link createLink(final Composite composite, final String theText) {
        final Link link = new Link(composite, SWT.NONE);
        link.setFont(composite.getFont());
        link.setText("<A>" + theText + "</A>"); //$NON-NLS-1$//$NON-NLS-2$
        link.addSelectionListener(new SelectionListener() {
            public void widgetSelected(final SelectionEvent e) {
                doLinkActivated((Link) e.widget);
            }

            public void widgetDefaultSelected(final SelectionEvent e) {
                doLinkActivated((Link) e.widget);
            }
        });
        return link;
    }

    void doLinkActivated(final Link widget) {
        if (isProjectPreferencePage()) {
            openWorkspacePreferences(null);
        } else {
            final List<IProject> erlProjects = new ArrayList<IProject>();
            final Set<IProject> projectsWithSpecifics = new HashSet<IProject>();
            final IErlModel model = ErlangCore.getModel();
            try {
                for (final IErlProject ep : model.getErlangProjects()) {
                    final IProject p = ep.getProject();
                    if (hasProjectSpecificOptions(p)) {
                        projectsWithSpecifics.add(p);
                    }
                    erlProjects.add(p);
                }
            } catch (final ErlModelException e) {
            }
            final ProjectSelectionDialog dialog = new ProjectSelectionDialog(
                    getShell(), erlProjects, projectsWithSpecifics);
            if (dialog.open() == Window.OK) {
                final IProject res = (IProject) dialog.getFirstResult();
                openProjectProperties(res);
            }
        }
    }

    private void openProjectProperties(final IProject project) {
        final String id = getPropertyPageID();
        if (id != null) {
            PreferencesUtil.createPropertyDialogOn(getShell(), project, id,
                    new String[] { id }, null).open();
        }
    }

    protected final void openWorkspacePreferences(final Object data) {
        final String id = getPreferencePageID();
        PreferencesUtil.createPreferenceDialogOn(getShell(), id,
                new String[] { id }, data).open();
    }

    protected String getPreferencePageID() {
        return "org.erlide.ui.preferences.compiler";
    }

    protected String getPropertyPageID() {
        return "org.erlide.ui.properties.compilerPreferencePage";
    }

    private void newCheckButtonInvertedOption(final String theText,
            final String toolTipText, final String optionKey) {
        newCheckButtonWithInvertOption(theText, toolTipText, optionKey, true);
    }

    private void newCheckButton(final String theText, final String toolTipText,
            final String optionKey) {
        newCheckButtonWithInvertOption(theText, toolTipText, optionKey, false);
    }

    private void newCheckButtonWithInvertOption(final String theText,
            final String toolTipText, final String optionKey,
            final boolean inverted) {
        final Button b = new Button(curGroup, SWT.CHECK);
        b.setText(theText);
        b.setToolTipText(toolTipText);
        b.setSelection(prefs.getBooleanOption(optionKey) == !inverted);
        b.setData(optionKey);
        b.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                final Button button = (Button) e.widget;
                prefs.setBooleanOption((String) button.getData(),
                        button.getSelection() == !inverted);
            }
        });
    }

    enum OptionStatus {
        OK, ERROR, NO_RUNTIME
    }

    OptionStatus optionsAreOk(final String string) {
        final ErlideBackend b = ErlangCore.getBackendManager().getIdeBackend();
        try {
            ErlBackend.parseTerm(b, string + " .");
        } catch (final NoBackendException e) {
            return OptionStatus.NO_RUNTIME;
        } catch (final BackendException e) {
            try {
                final String string2 = "[" + string + "].";
                ErlBackend.parseTerm(b, string2);
            } catch (final BackendException e1) {
                return OptionStatus.ERROR;
            }
        }
        return OptionStatus.OK;
    }

    @Override
    public boolean performOk() {
        try {
            if (fUseProjectSettings != null
                    && !fUseProjectSettings.getSelection()
                    && isProjectPreferencePage()) {
                prefs.removeAllProjectSpecificSettings();
            } else {
                prefs.store();
            }
        } catch (final BackingStoreException e) {
            ErlLogger.warn(e);
        }
        return super.performOk();
    }

    @Override
    protected void performDefaults() {
        if (fProject == null) {
            prefs = new CompilerPreferences();
        } else {
            prefs = new CompilerPreferences(fProject);
        }
        try {
            prefs.load();
        } catch (final BackingStoreException e) {
            ErlLogger.warn(e);
        }
        super.performDefaults();
    }

    @Override
    public void setElement(final IAdaptable element) {
        fProject = (IProject) element.getAdapter(IResource.class);
        super.setElement(element);
    }

    public void init(final IWorkbench workbench) {
        performDefaults();
    }
}
