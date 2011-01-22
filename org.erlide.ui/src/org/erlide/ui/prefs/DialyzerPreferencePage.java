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

import java.io.File;
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
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.eclipse.ui.dialogs.PropertyPage;
import org.erlide.core.builder.DialyzerPreferences;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlProject;
import org.erlide.jinterface.backend.util.PreferencesUtils;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.ui.handlers.CheckDialyzerPltFileHandler;
import org.erlide.ui.internal.util.CommandRunnerSelectionAdapter;
import org.osgi.service.prefs.BackingStoreException;

public class DialyzerPreferencePage extends PropertyPage implements
        IWorkbenchPreferencePage {

    DialyzerPreferences prefs;
    private IProject fProject;
    private Button fUseProjectSettings;
    private Link fChangeWorkspaceSettings;
    protected ControlEnableState fBlockEnableState;
    // private final Text pltEdit = null;
    private Combo fromCombo;
    private Button dialyzeCheckbox;
    private Composite prefsComposite;
    private org.eclipse.swt.widgets.List fPLTList;
    private Button fAddButton;
    private Button fEditButton;
    private Button fRemoveButton;
    private Button fCheckPLTButton;

    public DialyzerPreferencePage() {
        super();
        setTitle("Dialyzer options");
        setDescription("Select the options for dialyzer.");
    }

    @Override
    protected Control createContents(final Composite parent) {

        prefsComposite = new Composite(parent, SWT.NONE);
        prefsComposite.setLayout(new GridLayout());

        // final Group group = new Group(prefsComposite, SWT.NONE);
        final Composite group = prefsComposite;// new Composite(prefsComposite,
                                               // SWT.NONE);
        group.setLayoutData(new GridData(SWT.FILL, SWT.BEGINNING, true, false));
        group.setLayout(new GridLayout(1, false));
        createDialyzeCheckbox(group);
        createPltSelection(group);
        createPltCheck(group);
        createFromSelection(group);
        enableButtons();

        if (isProjectPreferencePage()) {
            final boolean useProjectSettings = hasProjectSpecificOptions(fProject);
            enableProjectSpecificSettings(useProjectSettings);
        }

        performDefaults();

        return prefsComposite;
    }

    private void createPltCheck(final Composite group) {
        final Composite comp = new Composite(group, SWT.NONE);
        comp.setLayout(new GridLayout(2, false));
        fCheckPLTButton = new Button(comp, SWT.PUSH);
        fCheckPLTButton.setText("Check PLT");
        fCheckPLTButton.addSelectionListener(new CommandRunnerSelectionAdapter(
                CheckDialyzerPltFileHandler.COMMAND_ID));
        final Label l = new Label(comp, SWT.NONE);
        l.setText("Warning: this can take some time");
    }

    private void createDialyzeCheckbox(final Composite group) {
        final Composite comp = new Composite(group, SWT.NONE);
        // comp.setLayoutData(new GridData(SWT.FILL, SWT.BEGINNING, true,
        // false));
        comp.setLayout(new GridLayout(1, false));
        dialyzeCheckbox = new Button(comp, SWT.CHECK);
        dialyzeCheckbox.setText("Run dialyzer when compiling");
        dialyzeCheckbox.setSelection(prefs.getDialyzeOnCompile());
    }

    private void createFromSelection(final Composite group) {
        final Composite comp = new Composite(group, SWT.NONE);
        // comp.setLayoutData(new GridData(SWT.FILL, SWT.BEGINNING, true,
        // false));
        comp.setLayout(new GridLayout(2, false));
        final Label l = new Label(comp, SWT.NONE);
        l.setText("Analyze from ");
        fromCombo = new Combo(comp, SWT.READ_ONLY);
        fromCombo.setItems(new String[] { "Source", "Binaries" });
        fromCombo.setText(fromCombo.getItem(prefs.getFromSource() ? 0 : 1));
    }

    private void createPltSelection(final Composite group) {
        final Composite comp = new Composite(group, SWT.NONE);
        comp.setLayout(new GridLayout(3, false));
        GridData gd = new GridData(SWT.FILL, SWT.BEGINNING, true, false);
        comp.setLayoutData(gd);
        final Label l = new Label(comp, SWT.NONE);
        l.setText("PLT files (multiple PLT requires Erlang/OTP R14B01 or later)");
        gd = new GridData();
        gd.horizontalSpan = 2;
        l.setLayoutData(gd);
        fPLTList = new org.eclipse.swt.widgets.List(comp, SWT.MULTI
                | SWT.V_SCROLL | SWT.BORDER);
        fPLTList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                enableButtons();
            }
        });
        // gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false, 3, 1);
        gd = new GridData(GridData.VERTICAL_ALIGN_BEGINNING
                | GridData.FILL_HORIZONTAL | GridData.FILL_VERTICAL);
        gd.horizontalSpan = 2;
        fPLTList.setLayoutData(gd);
        gd.heightHint = convertHeightInCharsToPixels(12);
        final Composite buttons = new Composite(comp, SWT.NULL);
        buttons.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_BEGINNING));
        final GridLayout layout = new GridLayout();
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        buttons.setLayout(layout);
        gd = new GridData();
        gd.horizontalAlignment = GridData.HORIZONTAL_ALIGN_FILL;
        fAddButton = new Button(buttons, SWT.PUSH);
        fAddButton.setText("Add...");
        fAddButton.setLayoutData(gd);
        fAddButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                final String result = selectPLTDialog(null);
                if (result != null) {
                    fPLTList.add(result);
                }
            }
        });

        fEditButton = new Button(buttons, SWT.PUSH);
        fEditButton.setText("Change...");
        fEditButton.setLayoutData(gd);
        fEditButton.addListener(SWT.Selection, new Listener() {
            public void handleEvent(final Event evt) {
                final String[] selection = fPLTList.getSelection();
                final int[] selectionIndices = fPLTList.getSelectionIndices();
                final String result = selectPLTDialog(selection[0]);
                if (result != null) {
                    fPLTList.setItem(selectionIndices[0], result);
                }
            }
        });

        fRemoveButton = new Button(buttons, SWT.PUSH);
        fRemoveButton.setText("Remove");
        fRemoveButton.setLayoutData(gd);
        fRemoveButton.addListener(SWT.Selection, new Listener() {
            public void handleEvent(final Event evt) {
                final int[] selectionIndices = fPLTList.getSelectionIndices();
                fPLTList.remove(selectionIndices[0]);
            }
        });
    }

    protected boolean hasProjectSpecificOptions(final IProject project) {
        final DialyzerPreferences p = new DialyzerPreferences(project);
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

    private Link createLink(final Composite composite, final String text) {
        final Link link = new Link(composite, SWT.NONE);
        link.setFont(composite.getFont());
        link.setText("<A>" + text + "</A>"); //$NON-NLS-1$//$NON-NLS-2$
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

    protected void enableButtons() {
        final int selectionCount = fPLTList.getSelectionCount();
        fEditButton.setEnabled(selectionCount == 1);
        fRemoveButton.setEnabled(selectionCount > 0);
        fCheckPLTButton.setEnabled(selectionCount == 1);
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
        return "org.erlide.ui.preferences.dialyzer";
    }

    protected String getPropertyPageID() {
        return "org.erlide.ui.properties.dialyzerPreferencePage";
    }

    boolean optionsAreOk() {
        for (final String s : fPLTList.getItems()) {
            final File f = new File(s);
            if (!f.exists()) {
                return false;
            }
        }
        return true;
    }

    @Override
    public boolean performOk() {
        try {
            prefs.setPltPath(PreferencesUtils.packArray(fPLTList.getItems()));
            prefs.setFromSource(fromCombo.getSelectionIndex() == 0);
            prefs.setDialyzeOnCompile(dialyzeCheckbox.getSelection());
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
            prefs = new DialyzerPreferences();
        } else {
            prefs = new DialyzerPreferences(fProject);
        }
        try {
            prefs.load();
            if (fPLTList != null) {
                final String[] strings = PreferencesUtils.unpackArray(prefs
                        .getPltPath());
                fPLTList.setItems(strings);
            }
            if (fromCombo != null) {
                fromCombo.setText(fromCombo.getItem(prefs.getFromSource() ? 0
                        : 1));
            }
            if (dialyzeCheckbox != null) {
                dialyzeCheckbox.setSelection(prefs.getDialyzeOnCompile());
            }
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

    private String selectPLTDialog(final String s) {
        final FileDialog dialog = new FileDialog(getShell(), SWT.SINGLE);
        dialog.setText("Select PLT file");
        dialog.setFileName(s);
        dialog.setFilterPath(s);
        dialog.setFilterNames(new String[] { "Dialyzer PLT file (*.plt)",
                "Any File" });
        dialog.setFilterExtensions(new String[] { "*.plt", "*.*" });
        final String result = dialog.open();
        return result;
    }
}
