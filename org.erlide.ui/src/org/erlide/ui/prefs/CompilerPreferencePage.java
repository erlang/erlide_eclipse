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
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.eclipse.ui.dialogs.PropertyPage;
import org.erlide.backend.BackendCore;
import org.erlide.backend.BackendException;
import org.erlide.backend.BackendHelper;
import org.erlide.backend.IBackend;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlModel;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.services.builder.CompilerOption;
import org.erlide.core.services.builder.CompilerOption.PathsOption;
import org.erlide.core.services.builder.CompilerOptions;
import org.erlide.jinterface.ErlLogger;
import org.osgi.service.prefs.BackingStoreException;

import com.google.common.collect.Lists;

public class CompilerPreferencePage extends PropertyPage implements
        IWorkbenchPreferencePage {
    CompilerOptions prefs;
    private Composite prefsComposite;
    private IProject fProject;
    private Button fUseProjectSettings;
    private Link fChangeWorkspaceSettings;
    protected ControlEnableState fBlockEnableState;
    private final List<Button> optionButtons;
    private Text text;
    private Text text_1;
    private Text text_2;

    public CompilerPreferencePage() {
        super();
        setTitle("Compiler options");
        setDescription("Select the compiler options to be used.");
        optionButtons = Lists.newArrayList();
    }

    @Override
    protected Control createContents(final Composite parent) {
        prefsComposite = new Composite(parent, SWT.NONE);
        final GridLayout gridLayout_1 = new GridLayout();
        gridLayout_1.numColumns = 2;
        prefsComposite.setLayout(gridLayout_1);

        final Group optionsGroup = new Group(prefsComposite, SWT.NONE);
        {
            final GridData gd_optionsGroup = new GridData(SWT.FILL, SWT.CENTER,
                    false, false, 2, 1);
            gd_optionsGroup.widthHint = 400;
            optionsGroup.setLayoutData(gd_optionsGroup);
        }
        optionsGroup.setLayout(new GridLayout(2, true));
        final Button b = newCheckButton(optionsGroup, CompilerOption.DEBUG_INFO);
        b.setEnabled(false);
        b.setSelection(true);
        newCheckButton(optionsGroup, CompilerOption.ENCRYPT_DEBUG_INFO);
        newCheckButton(optionsGroup, CompilerOption.COMPRESSED);

        final Label lblNewLabel = new Label(prefsComposite, SWT.NONE);
        lblNewLabel.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false,
                false, 1, 1));
        lblNewLabel.setText(CompilerOption.INCLUDE_DIRS.getDescription());

        text = new Text(prefsComposite, SWT.BORDER);
        text.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
        text.setToolTipText(CompilerOption.INCLUDE_DIRS.getTooltip());
        text.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(final ModifyEvent e) {
                prefs.setPathOption(CompilerOption.INCLUDE_DIRS,
                        PathsOption.fromString(text.getText()));
            }
        });

        final Group warningsGroup = new Group(prefsComposite, SWT.NONE);
        {
            final GridData gridData = new GridData(SWT.FILL, SWT.FILL, false,
                    false, 2, 1);
            gridData.widthHint = 400;
            warningsGroup.setLayoutData(gridData);
        }
        warningsGroup.setText("Warnings");
        final GridLayout gridLayout = new GridLayout(2, true);
        warningsGroup.setLayout(gridLayout);
        for (final CompilerOption option : CompilerOption.WARNINGS) {
            newCheckButton(warningsGroup, option);
        }
        new Label(optionsGroup, SWT.NONE);

        final Label lblNewLabel_1 = new Label(prefsComposite, SWT.NONE);
        lblNewLabel_1.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false,
                false, 1, 1));
        lblNewLabel_1.setText(CompilerOption.PARSE_TRANSFORM.getDescription());

        text_1 = new Text(prefsComposite, SWT.BORDER);
        text_1.setToolTipText(CompilerOption.PARSE_TRANSFORM.getTooltip());
        text_1.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1,
                1));
        text_1.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(final ModifyEvent e) {
                prefs.setSimpleOption(CompilerOption.PARSE_TRANSFORM,
                        text_1.getText());
            }
        });
        new Label(prefsComposite, SWT.NONE);
        new Label(prefsComposite, SWT.NONE);

        final Label lblNewLabel_2 = new Label(prefsComposite, SWT.NONE);
        lblNewLabel_2.setText("Custom options:");

        text_2 = new Text(prefsComposite, SWT.BORDER | SWT.WRAP | SWT.MULTI);
        final GridData gd_text_2 = new GridData(SWT.FILL, SWT.CENTER, true,
                false, 1, 1);
        gd_text_2.heightHint = 60;
        text_2.setLayoutData(gd_text_2);
        text_2.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(final ModifyEvent e) {
                prefs.setSimpleOption(CompilerOption.CUSTOM, text_2.getText());
            }
        });

        if (isProjectPreferencePage()) {
            final boolean useProjectSettings = hasProjectSpecificOptions(fProject);
            enableProjectSpecificSettings(useProjectSettings);
        }

        initPrefs();

        return prefsComposite;
    }

    private void initPrefs() {
        if (fProject == null) {
            prefs = new CompilerOptions();
        } else {
            prefs = new CompilerOptions(fProject);
        }
        try {
            prefs.load();
        } catch (final BackingStoreException e) {
            ErlLogger.warn(e);
        }
        updateUI();
    }

    protected boolean hasProjectSpecificOptions(final IProject project) {
        final CompilerOptions p = new CompilerOptions(project);
        return p.hasOptionsAtLowestScope();
    }

    private boolean isProjectPreferencePage() {
        return fProject != null;
    }

    @Override
    protected Label createDescriptionLabel(final Composite parent) {
        createProjectSpecificSettingsCheckBoxAndLink(parent);
        final Label lblSelectTheCompiler = super.createDescriptionLabel(parent);
        final String suffix = isProjectPreferencePage() ? "in this project."
                : "by default.";
        lblSelectTheCompiler.setText("Select the compiler options to be used "
                + suffix);
        return lblSelectTheCompiler;
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
            @Override
            public void widgetSelected(final SelectionEvent e) {
                doLinkActivated((Link) e.widget);
            }

            @Override
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
            final IErlModel model = ErlModelManager.getErlangModel();
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

    private Button newCheckButton(final Composite parent,
            final CompilerOption option) {
        final Button b = new Button(parent, SWT.CHECK);
        b.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
        b.setText(option.getDescription());
        b.setToolTipText(option.getTooltip());
        b.setData(option);
        b.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                final Button button = (Button) e.widget;
                prefs.setBooleanOption((CompilerOption) button.getData(),
                        button.getSelection());
            }
        });
        optionButtons.add(b);
        return b;
    }

    enum OptionStatus {
        //@formatter:off
        OK, 
        @SuppressWarnings("hiding") ERROR, 
        NO_RUNTIME
        //@formatter:off
    }

    OptionStatus optionsAreOk(final String string) {
        final IBackend b = BackendCore.getBackendManager().getIdeBackend();
        if (b == null) {
            return OptionStatus.NO_RUNTIME;
        }
        try {
            BackendHelper.parseTerm(b, string + " .");
        } catch (final BackendException e) {
            try {
                final String string2 = "[" + string + "].";
                BackendHelper.parseTerm(b, string2);
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
            prefs = new CompilerOptions();
        } else {
            prefs = new CompilerOptions(fProject);
        }
        updateUI();
        super.performDefaults();
    }

    private void updateUI() {
        for(final Button b: optionButtons){
            final CompilerOption key = (CompilerOption) b.getData();
            b.setSelection(prefs.getBooleanOption(key));
        }
        final Iterable<String> paths = prefs.getPathsOption(CompilerOption.INCLUDE_DIRS);
        if(paths!=null) {
            text.setText(PathsOption.toString(paths));
        }
        final String parseTransform = prefs.getSimpleOption(CompilerOption.PARSE_TRANSFORM);
        if(parseTransform!=null) {
            text_1.setText(parseTransform);
        }
        final String custom = prefs.getSimpleOption(CompilerOption.CUSTOM);
        if(custom!=null) {
            text_2.setText(custom);
        }
    }

    @Override
    public void setElement(final IAdaptable element) {
        fProject = (IProject) element.getAdapter(IResource.class);
        super.setElement(element);
    }

    @Override
    public void init(final IWorkbench workbench) {
        performDefaults();
    }

    private static class MacrosTableContentProvider implements IStructuredContentProvider {
        @Override
        public Object[] getElements(final Object inputElement) {
            return new Object[]{"aaa", "vvv"};
        }

        @Override
        public void dispose() {
        }

        @Override
        public void inputChanged(final Viewer viewer, final Object oldInput,
                final Object newInput) {
        }
    }

    private class MacrosTableLabelProvider extends LabelProvider implements ITableLabelProvider {
        @Override
        public Image getColumnImage(final Object element, final int columnIndex) {
            return null;
        }
        @Override
        public String getColumnText(final Object element, final int columnIndex) {
            return element.toString();
        }
    }

}
