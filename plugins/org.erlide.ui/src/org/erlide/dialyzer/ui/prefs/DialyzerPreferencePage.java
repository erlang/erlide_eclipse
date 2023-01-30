/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others. All rights reserved. This program and
 * the accompanying materials are made available under the terms of the Eclipse Public
 * License v1.0 which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors: Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.dialyzer.ui.prefs;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.MultiRule;
import org.eclipse.jface.dialogs.ControlEnableState;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Table;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.eclipse.ui.dialogs.PropertyPage;
import org.eclipse.wb.swt.SWTResourceManager;
import org.erlide.dialyzer.builder.DialyzerPreferences;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.root.IErlModel;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.ui.prefs.ProjectSelectionDialog;
import org.erlide.util.ErlLogger;
import org.osgi.service.prefs.BackingStoreException;

import com.google.common.collect.Lists;

public class DialyzerPreferencePage extends PropertyPage
        implements IWorkbenchPreferencePage {

    public class ContentProvider implements IStructuredContentProvider {

        @Override
        public void dispose() {
        }

        @Override
        public void inputChanged(final Viewer viewer, final Object oldInput,
                final Object newInput) {
        }

        @Override
        public Object[] getElements(final Object inputElement) {
            return shownPLTFiles.toArray();
        }

    }

    private static class LabelProvider implements ILabelProvider {

        @Override
        public void addListener(final ILabelProviderListener listener) {
        }

        @Override
        public void dispose() {
        }

        @Override
        public boolean isLabelProperty(final Object element, final String property) {
            return true;
        }

        @Override
        public void removeListener(final ILabelProviderListener listener) {
        }

        @Override
        public Image getImage(final Object element) {
            return null;
        }

        @Override
        public String getText(final Object element) {
            if (element instanceof String) {
                final String s = (String) element;
                return s;
            }
            return null;
        }

    }

    private static final int MAX_PLT_FILES = 256;

    DialyzerPreferences prefs;
    private IProject fProject;
    private Button fUseProjectSettings;
    private Link fChangeWorkspaceSettings;
    protected ControlEnableState fBlockEnableState;
    private Combo fromCombo;
    private Button dialyzeCheckbox;
    private Composite prefsComposite;
    private CheckboxTableViewer fPLTTableViewer;
    private Button fAddButton;
    private Button fEditButton;
    private Button fRemoveButton;
    private Button noCheckPLTCheckbox;
    private Button removeWarningsOnCleanCheckbox;
    private final List<String> shownPLTFiles;

    public DialyzerPreferencePage() {
        setTitle("Dialyzer options");
        setDescription("Select the options for dialyzer.");
        shownPLTFiles = Lists.newArrayList();
    }

    @Override
    protected Control createContents(final Composite parent) {
        loadPrefs();
        prefsComposite = new Composite(parent, SWT.NONE);
        final GridLayout gl_prefsComposite = new GridLayout();
        gl_prefsComposite.numColumns = 2;
        prefsComposite.setLayout(gl_prefsComposite);

        final Composite group = prefsComposite;
        group.setLayoutData(new GridData(SWT.FILL, SWT.BEGINNING, true, false));
        createDialyzeCheckbox(group);
        createPltSelection(group);
        createFromSelection(group);
        createPltNoCheckbox(group);
        createRemoveWarningsOnCleanCheckbox(group);
        enableButtons();

        if (isProjectPreferencePage()) {
            final boolean useProjectSettings = hasProjectSpecificOptions(fProject);
            enableProjectSpecificSettings(useProjectSettings);
        }

        performDefaults();

        return prefsComposite;
    }

    private void createRemoveWarningsOnCleanCheckbox(final Composite group) {
        new Label(prefsComposite, SWT.NONE);
        final Composite comp = new Composite(group, SWT.NONE);
        comp.setLayout(new GridLayout(2, false));
        removeWarningsOnCleanCheckbox = new Button(comp, SWT.CHECK);
        removeWarningsOnCleanCheckbox.setText("Remove dialyzer warning on clean project");
        new Label(comp, SWT.NONE);
    }

    private void createPltNoCheckbox(final Composite group) {
        new Label(prefsComposite, SWT.NONE);
        final Composite comp = new Composite(group, SWT.NONE);
        comp.setLayout(new GridLayout(2, false));
        noCheckPLTCheckbox = new Button(comp, SWT.CHECK);
        noCheckPLTCheckbox.setText("Do not check PLT on dialyzer run");
        new Label(comp, SWT.NONE);
    }

    private void createDialyzeCheckbox(final Composite group) {
        final Label lblNewLabel = new Label(prefsComposite, SWT.NONE);
        lblNewLabel.setToolTipText(
                "Only useful for small projects, the project is locked while running dialyzer");
        lblNewLabel.setImage(SWTResourceManager.getImage(DialyzerPreferencePage.class,
                "/icons/full/obj16/dialyzer_warning.gif"));
        final Composite comp = new Composite(group, SWT.NONE);
        comp.setLayout(new GridLayout(1, false));
        dialyzeCheckbox = new Button(comp, SWT.CHECK);
        dialyzeCheckbox.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
            }
        });
        dialyzeCheckbox.setText("Run dialyzer when compiling. Not recommended.");
        dialyzeCheckbox.setToolTipText(
                "Only useful for small projects, the project is locked while running dialyzer");
        dialyzeCheckbox.setSelection(prefs.getDialyzeOnCompile());
    }

    private void createFromSelection(final Composite group) {
        final Composite comp = new Composite(group, SWT.NONE);
        comp.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 2, 1));
        comp.setLayout(new GridLayout(2, false));
        final Label l = new Label(comp, SWT.NONE);
        l.setText("Analyze from ");
        fromCombo = new Combo(comp, SWT.READ_ONLY);
        fromCombo.setItems("Source", "Binaries");
        fromCombo.setText(fromCombo.getItem(prefs.getFromSource() ? 0 : 1));
    }

    private void createPltSelection(final Composite group) {
        final Composite composite = new Composite(group, SWT.NONE);
        composite.setLayout(new GridLayout(3, false));
        GridData gd = new GridData(SWT.FILL, SWT.BEGINNING, true, false);
        gd.horizontalSpan = 2;
        composite.setLayoutData(gd);
        final Label l = new Label(composite, SWT.NONE);
        l.setText("PLT files");
        gd = new GridData();
        gd.horizontalSpan = 2;
        l.setLayoutData(gd);
        new Label(composite, SWT.NONE);
        fPLTTableViewer = CheckboxTableViewer.newCheckList(composite, SWT.BORDER);
        fPLTTableViewer.setLabelProvider(new LabelProvider());
        fPLTTableViewer.setContentProvider(new ContentProvider());
        fPLTTableViewer.setInput(this);
        final Table table = fPLTTableViewer.getTable();
        table.

                addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(final SelectionEvent e) {
                        enableButtons();
                    }
                });
        gd = new GridData(GridData.VERTICAL_ALIGN_BEGINNING | GridData.FILL_HORIZONTAL
                | GridData.FILL_VERTICAL);
        gd.horizontalSpan = 2;
        table.setLayoutData(gd);
        gd.heightHint = convertHeightInCharsToPixels(12);
        final Composite buttons = new Composite(composite, SWT.NULL);
        buttons.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_BEGINNING));
        final GridLayout layout = new GridLayout();
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        buttons.setLayout(layout);
        fAddButton = createButton(buttons, "Add...", new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                addPLTFile();
            }
        });
        fEditButton = createButton(buttons, "Change...", new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                changeSelectedPLTFiles();
            }
        });
        fRemoveButton = createButton(buttons, "Remove", new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                removeSelectedPLTFiles();
            }
        });

        if (isProjectPreferencePage()) {
            fAddButton.setVisible(false);
            fEditButton.setVisible(false);
            fRemoveButton.setVisible(false);
        }
    }

    private Button createButton(final Composite buttons, final String text,
            final SelectionListener selectionListener) {
        GridData gd;
        final Button button = new Button(buttons, SWT.PUSH);
        button.setText(text);
        gd = new GridData();
        gd.horizontalAlignment = GridData.HORIZONTAL_ALIGN_FILL;
        button.setLayoutData(gd);
        button.addSelectionListener(selectionListener);
        return button;
    }

    protected boolean hasProjectSpecificOptions(final IProject project) {
        final DialyzerPreferences p = DialyzerPreferences.get(project);
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

    private void enablePreferenceContent(final boolean useProjectSpecificSettings) {
        if (useProjectSpecificSettings) {
            if (fBlockEnableState != null) {
                fBlockEnableState.restore();
                fBlockEnableState = null;
            }
        } else if (fBlockEnableState == null) {
            fBlockEnableState = ControlEnableState.disable(prefsComposite);
        }
    }

    private void createProjectSpecificSettingsCheckBoxAndLink(final Composite parent) {
        if (isProjectPreferencePage()) {
            final Composite composite = new Composite(parent, SWT.NONE);
            composite.setFont(parent.getFont());
            final GridLayout layout = new GridLayout(2, false);
            layout.marginHeight = 0;
            layout.marginWidth = 0;
            composite.setLayout(layout);
            composite.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

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

            fChangeWorkspaceSettings = createLink(composite,
                    "Configure Workspace settings...");
            fChangeWorkspaceSettings
                    .setLayoutData(new GridData(SWT.END, SWT.CENTER, false, false));

            final Label horizontalLine = new Label(composite,
                    SWT.SEPARATOR | SWT.HORIZONTAL);
            horizontalLine.setLayoutData(
                    new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
            horizontalLine.setFont(composite.getFont());
        } else { // if (supportsProjectSpecificOptions() && offerLink()) {
            fChangeWorkspaceSettings = createLink(parent,
                    "Configure project specific settings..");
            fChangeWorkspaceSettings
                    .setLayoutData(new GridData(SWT.END, SWT.CENTER, true, false));
        }

    }

    private Link createLink(final Composite composite, final String text) {
        final Link link = new Link(composite, SWT.NONE);
        link.setFont(composite.getFont());
        link.setText("<A>" + text + "</A>"); //$NON-NLS-1$//$NON-NLS-2$
        link.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                doLinkActivated((Link) e.widget);
            }
        });
        return link;
    }

    void doLinkActivated(final Link widget) {
        if (isProjectPreferencePage()) {
            openWorkspacePreferences(null);
        } else {
            final List<IProject> erlProjects = new ArrayList<>();
            final Set<IProject> projectsWithSpecifics = new HashSet<>();
            final IErlModel model = ErlangEngine.getInstance().getModel();
            try {
                for (final IErlProject ep : model.getErlangProjects()) {
                    final IProject p = ep.getWorkspaceProject();
                    if (hasProjectSpecificOptions(p)) {
                        projectsWithSpecifics.add(p);
                    }
                    erlProjects.add(p);
                }
            } catch (final ErlModelException e) {
            }
            final ProjectSelectionDialog dialog = new ProjectSelectionDialog(getShell(),
                    erlProjects, projectsWithSpecifics);
            if (dialog.open() == Window.OK) {
                final IProject res = (IProject) dialog.getFirstResult();
                openProjectProperties(res);
            }
        }
    }

    protected void enableButtons() {
        final IStructuredSelection selection = (IStructuredSelection) fPLTTableViewer
                .getSelection();
        final int selectionCount = selection.size();
        fEditButton.setEnabled(selectionCount == 1);
        fRemoveButton.setEnabled(selectionCount > 0);
        fAddButton
                .setEnabled(shownPLTFiles.size() < DialyzerPreferencePage.MAX_PLT_FILES);
    }

    private void openProjectProperties(final IProject project) {
        final String id = DialyzerPreferencePage.getPropertyPageID();
        if (id != null) {
            PreferencesUtil.createPropertyDialogOn(getShell(), project, id,
                    new String[] { id }, null).open();
        }
    }

    protected final void openWorkspacePreferences(final Object data) {
        final String id = DialyzerPreferencePage.getPreferencePageID();
        PreferencesUtil
                .createPreferenceDialogOn(getShell(), id, new String[] { id }, data)
                .open();
    }

    protected static String getPreferencePageID() {
        return "org.erlide.ui.preferences.dialyzer";
    }

    protected static String getPropertyPageID() {
        return "org.erlide.ui.properties.dialyzerPreferencePage";
    }

    boolean optionsAreOk() {
        for (final String s : shownPLTFiles) {
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
            if (fUseProjectSettings != null && !fUseProjectSettings.getSelection()
                    && isProjectPreferencePage()) {
                prefs.removeAllProjectSpecificSettings();
            } else {
                if (fPLTTableViewer != null) {
                    prefs.setPltPaths(shownPLTFiles);
                    prefs.setEnabledPltPaths(getCheckedPltFiles());
                }
                prefs.setFromSource(fromCombo.getSelectionIndex() == 0);
                prefs.setDialyzeOnCompile(dialyzeCheckbox.getSelection());
                prefs.setNoCheckPLT(noCheckPLTCheckbox.getSelection());
                prefs.setRemoveWarningsOnClean(
                        removeWarningsOnCleanCheckbox.getSelection());
                prefs.store();
            }
        } catch (final BackingStoreException e) {
            ErlLogger.warn(e);
        }
        return super.performOk();
    }

    @Override
    protected void performDefaults() {
        loadPrefs();
        shownPLTFiles.clear();
        shownPLTFiles.addAll(prefs.getPltPaths());
        if (fPLTTableViewer != null) {
            fPLTTableViewer.refresh();
            fPLTTableViewer.setAllChecked(false);
            for (final String s : prefs.getEnabledPltPaths()) {
                fPLTTableViewer.setChecked(s, true);
            }
        }
        if (fromCombo != null) {
            fromCombo.setText(fromCombo.getItem(prefs.getFromSource() ? 0 : 1));
        }
        if (dialyzeCheckbox != null) {
            dialyzeCheckbox.setSelection(prefs.getDialyzeOnCompile());
        }
        if (noCheckPLTCheckbox != null) {
            noCheckPLTCheckbox.setSelection(prefs.getNoCheckPLT());
        }
        if (removeWarningsOnCleanCheckbox != null) {
            removeWarningsOnCleanCheckbox.setSelection(prefs.getRemoveWarningsOnClean());
        }
        super.performDefaults();
    }

    private void loadPrefs() {
        try {
            prefs = DialyzerPreferences.get(fProject);
        } catch (final Exception e) {
            setErrorMessage("Error loading preferences: " + e.getMessage());
            ErlLogger.error(e);
        }
    }

    @Override
    public void setElement(final IAdaptable element) {
        fProject = (IProject) element.getAdapter(IResource.class);
        super.setElement(element);
    }

    @Override
    public void init(final IWorkbench workbench) {
    }

    private String selectPLTDialog(final String s) {
        final FileDialog dialog = new FileDialog(getShell(), SWT.SINGLE);
        dialog.setText("Select PLT file");
        dialog.setFileName(s);
        dialog.setFilterPath(s);
        dialog.setFilterNames(new String[] { "Dialyzer PLT file (*.plt)", "Any File" });
        dialog.setFilterExtensions(new String[] { "*.plt", "*.*" });
        final String result = dialog.open();
        return result;
    }

    private void changeSelectedPLTFiles() {
        final IStructuredSelection selection = (IStructuredSelection) fPLTTableViewer
                .getSelection();
        if (selection.size() != 1) {
            return;
        }
        final Object selectedElement = selection.getFirstElement();
        final int i = shownPLTFiles.indexOf(selectedElement);
        if (i == -1) {
            return;
        }
        final String result = selectPLTDialog((String) selectedElement);
        if (result == null) {
            return;
        }
        shownPLTFiles.set(i, result);
        fPLTTableViewer.refresh();
    }

    protected void removeSelectedPLTFiles() {
        final IStructuredSelection selection = (IStructuredSelection) fPLTTableViewer
                .getSelection();
        for (final Object o : selection.toList()) {
            shownPLTFiles.remove(o);
        }
        fPLTTableViewer.refresh();
    }

    protected void addPLTFile() {
        final String result = selectPLTDialog(null);
        if (result == null) {
            return;
        }
        shownPLTFiles.add(result);
        fPLTTableViewer.refresh();
    }

    private List<String> getCheckedPltFiles() {
        final List<String> l = Lists.newArrayList();
        for (final Object o : fPLTTableViewer.getCheckedElements()) {
            l.add((String) o);
        }
        return l;
    }

    public ISchedulingRule createRule(final Set<IProject> projects) {
        ISchedulingRule combinedRule = null;
        for (final IProject project : projects) {
            combinedRule = MultiRule.combine(project, combinedRule);
        }
        return combinedRule;
    }

}
