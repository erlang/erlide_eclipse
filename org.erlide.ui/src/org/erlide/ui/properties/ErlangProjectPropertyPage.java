/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.properties;

import java.util.Arrays;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Path;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.PropertyPage;
import org.eclipse.xtext.xbase.lib.Functions;
import org.eclipse.xtext.xbase.lib.ListExtensions;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.model.root.PathSerializer;
import org.erlide.engine.model.root.ProjectConfigType;
import org.erlide.engine.model.root.ProjectPreferencesConstants;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.SystemConfiguration;

public class ErlangProjectPropertyPage extends PropertyPage {

    private Text outputText;
    private Text sourcesText;
    private Text includesText;
    private Text testsText;
    private Combo runtimeCombo;
    private Text extModsText;
    private Text extIncsText;

    private IErlProject erlProject;

    private ErlangProjectProperties model;

    public ErlangProjectPropertyPage() {
        super();
        // noDefaultAndApplyButton();
        model = new ErlangProjectProperties();
    }

    @Override
    public boolean performOk() {
        if (!isValid()) {
            return false;
        }
        erlProject.clearCaches();
        erlProject.setProperties(model);
        return super.performOk();
    }

    @Override
    protected void performDefaults() {
        model = erlProject.getProperties();
        super.performDefaults();
    }

    @Override
    public boolean isValid() {
        boolean ok = true;
        ok &= !outputText.getText().isEmpty();
        ok &= !sourcesText.getText().isEmpty();
        return (erlProject.getConfigType() != ProjectConfigType.INTERNAL || ok)
                && super.isValid();
    }

    @Override
    protected Control createContents(final Composite parent) {
        final IProject project = (IProject) getElement().getAdapter(IProject.class);
        erlProject = ErlangEngine.getInstance().getModel().getErlangProject(project);
        model = erlProject.getProperties();

        final Composite composite = new Composite(parent, SWT.NONE);

        boolean globalEnable = true;
        final ProjectConfigType configType = erlProject.getConfigType();
        if (configType != ProjectConfigType.INTERNAL) {
            globalEnable = false;
            setMessage("Please edit " + configType.getConfigName()
                    + " to change settings for this project");
        }

        composite.setLayout(new GridLayout(2, false));
        {
            final Label lblRequiredErlangVersion = new Label(composite, SWT.NONE);
            lblRequiredErlangVersion.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER,
                    false, false, 1, 1));
            lblRequiredErlangVersion.setText("Required Erlang version");
        }
        {
            runtimeCombo = new Combo(composite, SWT.NONE);
            final GridData gd_combo = new GridData(SWT.LEFT, SWT.CENTER, true, false, 1,
                    1);
            gd_combo.widthHint = 83;
            runtimeCombo.setLayoutData(gd_combo);
            runtimeCombo.setEnabled(globalEnable);
            final RuntimeVersion[] runtimeVersions = ProjectPreferencesConstants.SUPPORTED_VERSIONS;
            runtimeCombo.setItems(ListExtensions.map(Arrays.asList(runtimeVersions),
                    new Functions.Function1<RuntimeVersion, String>() {
                        @Override
                        public String apply(final RuntimeVersion p) {
                            return p.toString();
                        }
                    }).toArray(new String[] {}));
            runtimeCombo.setText(model.getRequiredRuntimeVersion().asMajor().toString());
            runtimeCombo.addSelectionListener(new SelectionListener() {

                @Override
                public void widgetSelected(final SelectionEvent e) {
                    model.setRequiredRuntimeVersion(RuntimeVersion.Serializer
                            .parse(runtimeCombo.getText()));
                }

                @Override
                public void widgetDefaultSelected(final SelectionEvent e) {
                }
            });
        }
        new Label(composite, SWT.NONE);
        new Label(composite, SWT.NONE);
        {
            final Label lblNewLabel = new Label(composite, SWT.NONE);
            lblNewLabel.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false,
                    1, 1));
            lblNewLabel.setText("Output directory");
        }
        {
            outputText = new Text(composite, SWT.BORDER);
            outputText
                    .setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
            outputText.setEnabled(globalEnable);
            outputText.setText(model.getOutputDir().toPortableString());
            outputText.addModifyListener(new ModifyListener() {

                @Override
                public void modifyText(final ModifyEvent e) {
                    model.setOutputDir(new Path(outputText.getText()));
                }
            });
        }
        {
            final Label lblNewLabel = new Label(composite, SWT.NONE);
            lblNewLabel.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false,
                    1, 1));
            lblNewLabel.setText("Source directories");
        }
        {
            sourcesText = new Text(composite, SWT.BORDER);
            sourcesText.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1,
                    1));
            sourcesText.setEnabled(globalEnable);
            sourcesText.setText(PathSerializer.packList(model.getSourceDirs()));
            sourcesText.addModifyListener(new ModifyListener() {

                @Override
                public void modifyText(final ModifyEvent e) {
                    model.setSourceDirs(PathSerializer.unpackList(sourcesText.getText()));
                }
            });
        }
        {
            final Label lblNewLabel = new Label(composite, SWT.NONE);
            lblNewLabel.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false,
                    1, 1));
            lblNewLabel.setText("Include directories");
        }
        {
            includesText = new Text(composite, SWT.BORDER);
            includesText.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1,
                    1));
            includesText.setEnabled(globalEnable);
            includesText.setText(PathSerializer.packList(model.getIncludeDirs()));
            includesText.addModifyListener(new ModifyListener() {

                @Override
                public void modifyText(final ModifyEvent e) {
                    model.setIncludeDirs(PathSerializer.unpackList(includesText.getText()));
                }
            });
        }
        {
            final Label lblNewLabel = new Label(composite, SWT.NONE);
            lblNewLabel.setEnabled(false);
            lblNewLabel.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false,
                    1, 1));
            lblNewLabel.setText("Test directories");
        }
        {
            testsText = new Text(composite, SWT.BORDER);
            testsText.setEnabled(globalEnable && false);
            testsText
                    .setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
            testsText.setText(PathSerializer.packList(model.getTestDirs()));
            testsText.addModifyListener(new ModifyListener() {

                @Override
                public void modifyText(final ModifyEvent e) {
                    model.setTestDirs(PathSerializer.unpackList(testsText.getText()));
                }
            });
        }
        new Label(composite, SWT.NONE);
        new Label(composite, SWT.NONE);
        {
            final Label lblExternalModules = new Label(composite, SWT.NONE);
            lblExternalModules.setVisible(true);
            lblExternalModules.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false,
                    false, 1, 1));
            lblExternalModules.setText("External modules");
        }
        {
            extModsText = new Text(composite, SWT.BORDER);
            extModsText.setEditable(false);
            extModsText.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1,
                    1));
            extModsText.setEnabled(globalEnable
                    && SystemConfiguration.getInstance().isDeveloper());
            extModsText.setText(model.getExternalModulesFile());
        }
        {
            final Label lblExternalIncludes = new Label(composite, SWT.NONE);
            lblExternalIncludes.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false,
                    false, 1, 1));
            lblExternalIncludes.setText("External includes");
        }
        {
            extIncsText = new Text(composite, SWT.BORDER);
            extIncsText.setEditable(false);
            extIncsText.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1,
                    1));
            extIncsText.setEnabled(globalEnable
                    && SystemConfiguration.getInstance().isDeveloper());
            extIncsText.setText(model.getExternalIncludesFile());
        }
        return composite;
    }

}
