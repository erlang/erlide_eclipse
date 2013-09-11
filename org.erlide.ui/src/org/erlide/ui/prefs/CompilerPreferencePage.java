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

import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.erlide.core.builder.CompilerOption;
import org.erlide.core.builder.CompilerOption.PathsOption;
import org.erlide.core.builder.CompilerOptions;
import org.erlide.util.ErlLogger;
import org.osgi.service.prefs.BackingStoreException;

import com.google.common.collect.Lists;

public class CompilerPreferencePage extends ProjectSpecificPreferencePage {
    CompilerOptions prefs;
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
        prefs.load();
        updateUI();
    }

    @Override
    protected boolean hasProjectSpecificOptions(final IProject project) {
        final CompilerOptions p = new CompilerOptions(project);
        return p.hasOptionsAtLowestScope();
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

    @Override
    protected String getPreferencePageID() {
        return "org.erlide.ui.preferences.compiler";
    }

    @Override
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
        for (final Button b : optionButtons) {
            final CompilerOption key = (CompilerOption) b.getData();
            b.setSelection(prefs.getBooleanOption(key));
        }
        final Iterable<String> paths = prefs
                .getPathsOption(CompilerOption.INCLUDE_DIRS);
        if (paths != null) {
            text.setText(PathsOption.toString(paths));
        }
        final String parseTransform = prefs
                .getSimpleOption(CompilerOption.PARSE_TRANSFORM);
        if (parseTransform != null) {
            text_1.setText(parseTransform);
        }
        final String custom = prefs.getSimpleOption(CompilerOption.CUSTOM);
        if (custom != null) {
            text_2.setText(custom);
        }
    }

    @Override
    public void setElement(final IAdaptable element) {
        fProject = (IProject) element.getAdapter(IResource.class);
        super.setElement(element);
    }

    @SuppressWarnings("unused")
    private static class MacrosTableContentProvider implements
            IStructuredContentProvider {
        @Override
        public Object[] getElements(final Object inputElement) {
            return new Object[] { "aaa", "vvv" };
        }

        @Override
        public void dispose() {
        }

        @Override
        public void inputChanged(final Viewer viewer, final Object oldInput,
                final Object newInput) {
        }
    }

    @SuppressWarnings("unused")
    private class MacrosTableLabelProvider extends LabelProvider implements
            ITableLabelProvider {
        @Override
        public Image getColumnImage(final Object element, final int columnIndex) {
            return null;
        }

        @Override
        public String getColumnText(final Object element, final int columnIndex) {
            return element.toString();
        }
    }

    @Override
    protected void openProjectPreferences() {
        // TODO Auto-generated method stub

    }

}
