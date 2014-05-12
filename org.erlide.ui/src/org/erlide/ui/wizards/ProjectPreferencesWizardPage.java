/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.wizards;

import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.erlide.engine.model.root.NewProjectData;
import org.erlide.engine.model.root.PathSerializer;
import org.erlide.engine.model.root.ProjectConfigType;
import org.erlide.ui.internal.ErlideUIPlugin;

public abstract class ProjectPreferencesWizardPage extends ErlangWizardPage {

    protected final NewProjectData info;
    protected ProjectConfigType configType;

    protected Text output;
    protected Text source;
    protected Text include;
    protected Text test;

    /**
     * Constructor inherited from parent
     *
     * @param pageName
     * @param builder
     * @param info
     * @wbp.parser.constructor
     */
    public ProjectPreferencesWizardPage(final String pageName, final NewProjectData info) {
        super(pageName);
        this.info = info;

        setTitle("Project layout configuration");
    }

    /**
     * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
     */
    @Override
    public void createControl(final Composite parent) {
        setDescription(getBuilderDescription());

        final Composite composite = new Composite(parent, SWT.NONE);
        setControl(composite);

        composite.setLayout(new FormLayout());

        final Label outLabel = new Label(composite, SWT.NONE);
        outLabel.setAlignment(SWT.RIGHT);
        FormData fd_outLabel;
        {
            fd_outLabel = new FormData();
            fd_outLabel.top = new FormAttachment(0, 28);
            fd_outLabel.left = new FormAttachment(0, 5);
            outLabel.setLayoutData(fd_outLabel);
        }
        final String resourceString = ErlideUIPlugin
                .getResourceString("wizards.labels.buildoutput");
        outLabel.setText(resourceString + ":");
        output = new Text(composite, SWT.BORDER);
        fd_outLabel.right = new FormAttachment(output, -10);
        {
            final FormData fd_output = new FormData();
            fd_output.top = new FormAttachment(0, 23);
            fd_output.right = new FormAttachment(0, 592);
            fd_output.left = new FormAttachment(0, 141);
            output.setLayoutData(fd_output);
        }
        output.setText(info.getOutputDir().toString());
        output.setEnabled(false);
        output.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(final ModifyEvent e) {
                info.setOutputDir(new Path(output.getText()));
            }
        });

        final Label l1 = new Label(composite, SWT.NONE);
        l1.setAlignment(SWT.RIGHT);
        FormData fd_l1;
        {
            fd_l1 = new FormData();
            fd_l1.top = new FormAttachment(0, 58);
            fd_l1.left = new FormAttachment(0, 5);
            l1.setLayoutData(fd_l1);
        }
        final String resourceString2 = ErlideUIPlugin
                .getResourceString("wizards.labels.source");
        l1.setText(resourceString2 + ":");
        source = new Text(composite, SWT.BORDER);
        fd_l1.right = new FormAttachment(source, -10);
        {
            final FormData fd_source = new FormData();
            fd_source.top = new FormAttachment(0, 53);
            fd_source.right = new FormAttachment(0, 592);
            fd_source.left = new FormAttachment(0, 141);
            source.setLayoutData(fd_source);
        }
        source.setToolTipText("enter a list of folders, using / in paths and ; as list separator");
        source.setText(PathSerializer.packList(info.getSourceDirs()));
        source.setEnabled(false);
        source.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(final ModifyEvent e) {
                info.setSourceDirs(PathSerializer.unpackList(source.getText()));
            }
        });

        final Label includesLabel = new Label(composite, SWT.NONE);
        includesLabel.setAlignment(SWT.RIGHT);
        FormData fd_includesLabel;
        {
            fd_includesLabel = new FormData();
            fd_includesLabel.top = new FormAttachment(0, 88);
            fd_includesLabel.left = new FormAttachment(0, 5);
            includesLabel.setLayoutData(fd_includesLabel);
        }
        final String resourceString3 = ErlideUIPlugin
                .getResourceString("wizards.labels.include");
        includesLabel.setText(resourceString3 + ":");
        include = new Text(composite, SWT.BORDER);
        fd_includesLabel.right = new FormAttachment(include, -10);
        {
            final FormData fd_include = new FormData();
            fd_include.top = new FormAttachment(0, 83);
            fd_include.right = new FormAttachment(0, 592);
            fd_include.left = new FormAttachment(0, 141);
            include.setLayoutData(fd_include);
        }
        include.setToolTipText("enter a list of folders, using / in paths and ; as list separator");
        include.setText(PathSerializer.packList(info.getIncludeDirs()));
        include.setEnabled(false);
        include.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(final ModifyEvent e) {
                info.setIncludeDirs(PathSerializer.unpackList(include.getText()));
            }
        });

        final Label lblTestSources = new Label(composite, SWT.NONE);
        lblTestSources.setEnabled(false);
        lblTestSources.setAlignment(SWT.RIGHT);
        FormData fd_lblTestSources;
        {
            fd_lblTestSources = new FormData();
            fd_lblTestSources.top = new FormAttachment(0, 118);
            fd_lblTestSources.left = new FormAttachment(0, 5);
            lblTestSources.setLayoutData(fd_lblTestSources);
        }
        final String resourceString4 = ErlideUIPlugin
                .getResourceString("wizards.labels.testsources");
        lblTestSources.setText(resourceString4 + ":");

        test = new Text(composite, SWT.BORDER);
        fd_lblTestSources.right = new FormAttachment(test, -10);
        test.setEnabled(false);
        {
            final FormData fd_test = new FormData();
            fd_test.top = new FormAttachment(0, 113);
            fd_test.right = new FormAttachment(0, 592);
            fd_test.left = new FormAttachment(0, 141);
            test.setLayoutData(fd_test);
        }
        test.setEditable(false);
        test.setEnabled(false);
        test.setToolTipText("enter a list of folders, using / in paths and ; as list separator");

        // for (final Control c : composite.getChildren()) {
        // c.setEnabled(!isReadOnly());
        // }
    }

    // protected void enableInputWidgets(final boolean b) {
    // output.setEnabled(b);
    // source.setEnabled(b);
    // include.setEnabled(b);
    // test.setEnabled(b);
    // }

    protected boolean testPageComplete() {
        if (null != output
                && (output.getText() == null || output.getText().trim().length() == 0)) {
            setErrorMessage(ErlideUIPlugin
                    .getResourceString("wizards.errors.outputrequired"));
            return false;
        }

        if (null != source
                && (source.getText() == null || source.getText().trim().length() == 0)) {
            setErrorMessage(ErlideUIPlugin
                    .getResourceString("wizards.errors.sourcerequired"));
            return false;
        }

        setErrorMessage(null);
        setMessage(null);
        return true;
    }

    protected String getBuilderDescription() {
        if (configType == ProjectConfigType.INTERNAL) {
            return null;
        }
        return "Configuration retrieved from " + configType.getConfigName();
    }

    @Override
    protected void onEntry() {
        if (info.getConfigType() != ProjectConfigType.INTERNAL) {
            final String op = info.isExistingProject() ? "editing" : "creating";
            setMessage("Please configure the project by " + op + " "
                    + info.getConfigType().getConfigName(), IMessageProvider.INFORMATION);
        }
        info.loadFromFile();
        output.setText(info.getOutputDir().toPortableString());
        source.setText(PathSerializer.packList(info.getSourceDirs()));
        include.setText(PathSerializer.packList(info.getIncludeDirs()));
    }

}
