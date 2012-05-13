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

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;
import org.eclipse.wb.swt.SWTResourceManager;
import org.erlide.backend.BackendCore;
import org.erlide.core.internal.model.root.OldErlangProjectProperties;
import org.erlide.core.internal.model.root.PathSerializer;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.utils.PreferencesUtils;
import org.erlide.utils.SystemUtils;

import com.ericsson.otp.erlang.RuntimeVersion;
import com.google.common.collect.Lists;

/**
 * 
 * @author Eric Merritt [cyberlync at yahoo dot com]
 */
public class ProjectPreferencesWizardPage extends WizardPage {

    Text output;
    Text source;
    Text include;
    Text backendCookie;
    Combo runtimeVersion;
    Text externalModules;
    Text externalIncludes;
    private Button externalModulesBrowse;
    private Button externalIncludesBrowse;

    OldErlangProjectProperties prefs;

    /**
     * Constructor inherited from parent
     * 
     * @param pageName
     * @wbp.parser.constructor
     */
    public ProjectPreferencesWizardPage(final String pageName) {
        super(pageName);
    }

    /**
     * Constructor inherited from parents parent
     * 
     * @param pageName
     * @param title
     * @param titleImage
     */
    public ProjectPreferencesWizardPage(final String pageName,
            final String title, final ImageDescriptor titleImage) {
        super(pageName, title, titleImage);
    }

    /**
     * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
     */
    @Override
    public void createControl(final Composite parent) {
        prefs = new OldErlangProjectProperties();
        prefs.setRuntimeVersion(BackendCore.getRuntimeInfoManager()
                .getDefaultRuntime().getVersion());

        // create the composite to hold the widgets
        final Composite composite = new Composite(parent, SWT.NONE);

        // create the desired layout for this wizard page
        final GridLayout gl = new GridLayout();
        gl.numColumns = 3;
        composite.setLayout(gl);

        final String resourceString = ErlideUIPlugin
                .getResourceString("wizards.labels.buildoutput");
        // create the widgets and their grid data objects
        final Label outLabel = new Label(composite, SWT.NONE);
        outLabel.setText("output Dir");
        final GridData gd_Label = new GridData();
        gd_Label.minimumWidth = 50;
        outLabel.setLayoutData(gd_Label);
        outLabel.setText(resourceString + ":");
        output = new Text(composite, SWT.BORDER);
        GridData gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        gd.minimumWidth = 50;
        gd.widthHint = 467;
        output.setLayoutData(gd);
        output.setText(prefs.getOutputDir().toString());
        output.addListener(SWT.Modify, nameModifyListener);
        final String resourceString2 = ErlideUIPlugin
                .getResourceString("wizards.labels.source");
        final String resourceString3 = ErlideUIPlugin
                .getResourceString("wizards.labels.include");
        final String resourceString4 = ErlideUIPlugin
                .getResourceString("wizards.labels.testsources");

        // set the composite as the control for this page
        setControl(composite);

        final Label label = new Label(composite, SWT.NONE);
        label.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false,
                1, 1));

        final Label l1 = new Label(composite, SWT.NONE);
        l1.setText(resourceString2 + ":");
        source = new Text(composite, SWT.BORDER);
        source.setToolTipText("enter a list of folders, using / in paths and ; as list separator");
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1);
        gd.widthHint = 371;
        source.setLayoutData(gd);
        source.setText(PathSerializer.packList(prefs.getSourceDirs()));
        source.addListener(SWT.Modify, nameModifyListener);
        new Label(composite, SWT.NONE);

        final Label includesLabel = new Label(composite, SWT.NONE);
        includesLabel.setText(resourceString3 + ":");
        include = new Text(composite, SWT.BORDER);
        include.setToolTipText("enter a list of folders, using / in paths and ; as list separator");
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1);
        include.setLayoutData(gd);
        include.setText(PathSerializer.packList(prefs.getIncludeDirs()));
        include.addListener(SWT.Modify, nameModifyListener);
        new Label(composite, SWT.NONE);

        final Label lblTestSources = new Label(composite, SWT.NONE);
        lblTestSources.setText(resourceString4 + ":");

        text = new Text(composite, SWT.BORDER);
        text.setEditable(false);
        text.setToolTipText("enter a list of folders, using / in paths and ; as list separator");
        text.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
        new Label(composite, SWT.NONE);
        new Label(composite, SWT.NONE);

        final Button discoverBtn = new Button(composite, SWT.PUSH);
        discoverBtn
                .setToolTipText("Tries to guess the project's configuration \nby finding all erl and hrl files");
        final GridData gd_discoverBtn = new GridData(SWT.RIGHT, SWT.FILL,
                false, false);
        gd_discoverBtn.heightHint = 26;
        discoverBtn.setLayoutData(gd_discoverBtn);
        discoverBtn.setText("Discover paths...");
        discoverBtn.addListener(SWT.Selection, new Listener() {

            @Override
            public void handleEvent(final Event event) {
                discoverPaths();
            }
        });
        new Label(composite, SWT.NONE);

        final Label nodeNameLabel = new Label(composite, SWT.NONE);
        nodeNameLabel.setText("Backend version");

        runtimeVersion = new Combo(composite, SWT.READ_ONLY);
        final GridData gd_backendName = new GridData(SWT.LEFT, SWT.CENTER,
                true, false);
        gd_backendName.widthHint = 62;
        runtimeVersion.setLayoutData(gd_backendName);
        final String[] runtimeNames = getAllRuntimeNames();
        runtimeVersion.setItems(runtimeNames);
        runtimeVersion.setText(prefs.getRuntimeVersion().asMinor().toString());
        runtimeVersion.addListener(SWT.Modify, nameModifyListener);

        new Label(composite, SWT.NONE);
        if (SystemUtils.getInstance().isTest()) {
            createExternalModuleEditor(composite);
            createExternalIncludeEditor(composite);
        }

    }

    private String[] getAllRuntimeNames() {
        final String[][] runtimes = BackendCore.getRuntimeInfoManager()
                .getAllRuntimesVersions();
        final List<String> runtimeNames = Lists.newArrayList();
        for (int i = 0; i < runtimes.length; i++) {
            if (!runtimeNames.contains(runtimes[i][0])) {
                runtimeNames.add(runtimes[i][0]);
            }
        }
        Collections.sort(runtimeNames);
        return runtimeNames.toArray(new String[runtimeNames.size()]);
    }

    protected void discoverPaths() {
        final WizardNewProjectCreationPage prev = (WizardNewProjectCreationPage) getPreviousPage();
        final IPath loc = prev.getLocationPath();
        final File dir = loc.toFile();

        if (dir.exists()) {
            final List<String> src = search("erl", dir);
            final String[] srcs = dirs(src, loc);

            final List<String> inc = search("hrl", dir);
            final String[] incs = dirs(inc, loc);

            source.setText(PreferencesUtils.packArray(srcs));
            include.setText(PreferencesUtils.packArray(incs));
        }
    }

    private String[] dirs(final List<String> list, final IPath ref) {
        final int n = ref.segmentCount();
        final List<String> res = new ArrayList<String>(10);
        for (final Iterator<String> iter = list.iterator(); iter.hasNext();) {
            final String element = iter.next();
            IPath p = new Path(element);
            p = p.removeLastSegments(1).removeFirstSegments(n).setDevice(null);
            String ps = p.toString();
            if ("".equals(ps)) {
                ps = ".";
            }
            if (res.indexOf(ps) < 0) {
                res.add(ps);
            }
        }
        return res.toArray(new String[res.size()]);
    }

    private List<String> search(final String ext, final File file) {
        return search(ext, file, new ArrayList<String>());
    }

    private List<String> search(final String ext, final File file,
            final List<String> list) {
        if (file.isFile()) {
            final IPath path = new Path(file.getPath());
            if (path.getFileExtension() != null
                    && path.getFileExtension().equals(ext)) {
                list.add(file.getPath());
            }
        } else if (file.isDirectory()) {
            final File[] fs = file.listFiles();
            for (final File f : fs) {
                search(ext, f, list);
            }
        }
        return list;
    }

    protected boolean testPageComplete() {
        if (null != output
                && (output.getText() == null || output.getText().trim()
                        .length() == 0)) {
            setErrorMessage(ErlideUIPlugin
                    .getResourceString("wizards.errors.outputrequired"));
            return false;
        }

        if (null != source
                && (source.getText() == null || source.getText().trim()
                        .length() == 0)) {
            setErrorMessage(ErlideUIPlugin
                    .getResourceString("wizards.errors.sourcerequired"));
            return false;
        }

        setErrorMessage(null);
        setMessage(null);
        return true;
    }

    private final Listener nameModifyListener = new Listener() {

        @Override
        public void handleEvent(final Event e) {
            prefs.setOutputDir(new Path(output.getText()));
            prefs.setSourceDirs(PathSerializer.unpackList(source.getText()));
            prefs.setIncludeDirs(PathSerializer.unpackList(include.getText()));
            final RuntimeVersion rv = new RuntimeVersion(
                    runtimeVersion.getText());
            prefs.setRuntimeVersion(rv);
            prefs.setExternalModulesFile(externalModules.getText());
            prefs.setExternalIncludesFile(externalIncludes.getText());

            setPageComplete(testPageComplete());
        }
    };
    private Text text;

    public OldErlangProjectProperties getPrefs() {
        return prefs;
    }

    private void createExternalModuleEditor(final Composite parent) {
        final Composite composite = parent;

        final String resourceString4 = "External modules file";
        final Label label = new Label(composite, SWT.NONE);
        label.setBackground(SWTResourceManager.getColor(255, 255, 183));
        label.setText(resourceString4 + ":");
        externalModules = new Text(composite, SWT.BORDER);
        externalModules.setToolTipText("enter a list of folders");
        externalModules.setText(prefs.getExternalModulesFile());
        final GridData gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        gd.minimumWidth = 50;
        gd.widthHint = 384;
        externalModules.setLayoutData(gd);
        externalModules.addListener(SWT.Modify, nameModifyListener);
        externalModulesBrowse = new Button(composite, SWT.NONE);
        externalModulesBrowse.setText("Browse...");
        externalModulesBrowse.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent evt) {
                handleExternalModulesBrowseSelected();
            }

        });
    }

    private void createExternalIncludeEditor(final Composite parent) {
        final Composite composite = parent;

        final String resourceString4 = "External includes file";
        final Label label = new Label(composite, SWT.NONE);
        label.setBackground(SWTResourceManager.getColor(255, 255, 183));
        label.setText(resourceString4 + ":");
        externalIncludes = new Text(composite, SWT.BORDER);
        externalIncludes.setToolTipText("enter a list of folders");
        externalIncludes.setText(prefs.getExternalModulesFile());
        final GridData gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        gd.minimumWidth = 50;
        gd.widthHint = 384;
        externalIncludes.setLayoutData(gd);
        externalIncludes.addListener(SWT.Modify, nameModifyListener);
        externalIncludesBrowse = new Button(composite, SWT.NONE);
        externalIncludesBrowse.setText("Browse...");
        externalIncludesBrowse.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent evt) {
                handleExternalIncludesBrowseSelected();
            }

        });
    }

    protected void handleExternalModulesBrowseSelected() {
        String last = externalModules.getText();
        if (last == null) {
            last = ""; //$NON-NLS-1$
        } else {
            last = last.trim();
        }
        final FileDialog dialog = new FileDialog(getShell(), SWT.SINGLE);
        dialog.setText("Select file with external modules");
        dialog.setFileName(last);
        dialog.setFilterExtensions(new String[] { "*.erlidex" });
        final String result = dialog.open();
        if (result == null) {
            return;
        }
        externalModules.setText(result);
    }

    protected void handleExternalIncludesBrowseSelected() {
        String last = externalIncludes.getText();
        if (last == null) {
            last = ""; //$NON-NLS-1$
        } else {
            last = last.trim();
        }
        final FileDialog dialog = new FileDialog(getShell(), SWT.SINGLE);
        dialog.setText("Select file with external include files");
        dialog.setFileName(last);
        dialog.setFilterExtensions(new String[] { "*.erlidex" });
        final String result = dialog.open();
        if (result == null) {
            return;
        }
        externalIncludes.setText(result);
    }
}
