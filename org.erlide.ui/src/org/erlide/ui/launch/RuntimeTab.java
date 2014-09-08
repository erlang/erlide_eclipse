/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.launch;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTab;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.erlide.backend.BackendCore;
import org.erlide.backend.api.ErlRuntimeAttributes;
import org.erlide.runtime.api.RuntimeData;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.util.ErlLogger;
import org.erlide.util.HostnameUtils;
import org.erlide.util.NodeHostClassifier;
import org.erlide.util.NodeHostClassifier.HostnameType;
import org.erlide.util.NodeHostClassifier.NodeType;

public class RuntimeTab extends AbstractLaunchConfigurationTab {

    private Button shortNameButton;
    private Button longNameButton;
    private Text argsText;
    private Text workingDirText;
    private Combo runtimesCombo;
    private Button startNodeCheckbox;
    private Text cookieText;
    private Text nameText;
    private Button distributedLoadCheck;

    private Collection<RuntimeInfo> runtimes;
    private Label nodeHostLabel;

    /**
     * @wbp.parser.entryPoint
     */
    @Override
    public void createControl(final Composite parent) {
        runtimes = BackendCore.getRuntimeInfoCatalog().getRuntimes();

        final Composite comp = new Composite(parent, SWT.NONE);
        setControl(comp);
        final GridLayout topLayout = new GridLayout();
        topLayout.numColumns = 3;
        comp.setLayout(topLayout);

        final Label runtimeLabel = new Label(comp, SWT.NONE);
        runtimeLabel.setText("Name");

        final List<String> rtl = new ArrayList<String>();
        for (final RuntimeInfo r : runtimes) {
            rtl.add(r.getName());
        }
        final String[] rts = rtl.toArray(new String[] {});
        final RuntimeInfo defaultRuntime = BackendCore.getRuntimeInfoCatalog()
                .getDefaultRuntime();
        final int db = defaultRuntime == null ? 0 : Arrays.binarySearch(rts,
                defaultRuntime.getName());

        runtimesCombo = new Combo(comp, SWT.READ_ONLY);
        final GridData gd_runtimesCombo = new GridData(174, SWT.DEFAULT);
        gd_runtimesCombo.horizontalAlignment = SWT.FILL;
        runtimesCombo.setLayoutData(gd_runtimesCombo);
        runtimesCombo.addSelectionListener(new SelectionAdapter() {
            @SuppressWarnings("synthetic-access")
            @Override
            public void widgetSelected(final SelectionEvent e) {
                updateLaunchConfigurationDialog();
            }
        });
        runtimesCombo.setItems(rts);
        runtimesCombo.select(db);
        new Label(comp, SWT.NONE);
        new Label(comp, SWT.NONE);
        new Label(comp, SWT.NONE);
        new Label(comp, SWT.NONE);

        final Label nodeNameLabel = new Label(comp, SWT.NONE);
        nodeNameLabel.setText("Node name");

        nameText = new Text(comp, SWT.BORDER);
        final GridData gd_nameText = new GridData(SWT.LEFT, SWT.CENTER, false, false, 1,
                1);
        gd_nameText.widthHint = 241;
        nameText.setLayoutData(gd_nameText);
        nameText.addModifyListener(new ModifyListener() {
            @Override
            @SuppressWarnings("synthetic-access")
            public void modifyText(final ModifyEvent e) {
                updateLaunchConfigurationDialog();
            }
        });

        nodeHostLabel = new Label(comp, SWT.NONE);
        nodeHostLabel.setText("@");
        new Label(comp, SWT.NONE);

        longNameButton = new Button(comp, SWT.RADIO);
        longNameButton.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 2,
                1));
        longNameButton.addSelectionListener(new SelectionAdapter() {
            @SuppressWarnings("synthetic-access")
            @Override
            public void widgetSelected(final SelectionEvent e) {
                updateLaunchConfigurationDialog();
            }
        });
        longNameButton.setSelection(true);
        longNameButton.setText("long name (-name)");
        new Label(comp, SWT.NONE);

        shortNameButton = new Button(comp, SWT.RADIO);
        shortNameButton.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 2,
                1));
        shortNameButton.addSelectionListener(new SelectionAdapter() {
            @SuppressWarnings("synthetic-access")
            @Override
            public void widgetSelected(final SelectionEvent e) {
                updateLaunchConfigurationDialog();
            }
        });
        shortNameButton.setText("short name (-sname)");

        final Label cookieLabel = new Label(comp, SWT.NONE);
        cookieLabel.setToolTipText("Leave empty to use default one");
        cookieLabel.setText("Cookie");

        cookieText = new Text(comp, SWT.BORDER);
        final GridData gd_cookieText = new GridData(SWT.FILL, SWT.CENTER, false, false,
                2, 1);
        gd_cookieText.widthHint = 232;
        cookieText.setLayoutData(gd_cookieText);
        cookieText.addModifyListener(new ModifyListener() {
            @Override
            @SuppressWarnings("synthetic-access")
            public void modifyText(final ModifyEvent e) {
                updateLaunchConfigurationDialog();
            }
        });
        cookieText.setToolTipText("Leave empty to use default one");

        startNodeCheckbox = new Button(comp, SWT.CHECK);
        startNodeCheckbox.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false,
                3, 1));
        startNodeCheckbox.setSelection(true);
        startNodeCheckbox.setText("Start the Erlang node if not running already");
        startNodeCheckbox.addSelectionListener(new SelectionAdapter() {
            @SuppressWarnings("synthetic-access")
            @Override
            public void widgetSelected(final SelectionEvent e) {
                updateLaunchConfigurationDialog();
            }
        });

        final Label workingDirectoryLabel = new Label(comp, SWT.NONE);
        workingDirectoryLabel.setText("Working directory");

        workingDirText = new Text(comp, SWT.BORDER);
        workingDirText.setToolTipText("may be relative to the workspace");
        workingDirText
                .setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1));
        workingDirText.addModifyListener(new ModifyListener() {
            @Override
            @SuppressWarnings("synthetic-access")
            public void modifyText(final ModifyEvent e) {
                updateLaunchConfigurationDialog();
            }
        });

        final Label extraArgumentsLabel = new Label(comp, SWT.NONE);
        extraArgumentsLabel.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false,
                1, 1));
        extraArgumentsLabel.setText("Extra arguments");

        argsText = new Text(comp, SWT.BORDER | SWT.MULTI);
        argsText.setToolTipText("as on the command line\nBe careful about proper quoting!");
        final GridData gd_argsText = new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1);
        gd_argsText.heightHint = 35;
        argsText.setLayoutData(gd_argsText);
        argsText.addModifyListener(new ModifyListener() {
            @Override
            @SuppressWarnings("synthetic-access")
            public void modifyText(final ModifyEvent e) {
                updateLaunchConfigurationDialog();
            }
        });
        new Label(comp, SWT.NONE);
        final Label extraArgumentsLabel2 = new Label(comp, SWT.NONE);
        extraArgumentsLabel2.setText("(overrides runtime settings)");
        extraArgumentsLabel2.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true,
                false, 2, 1));
        distributedLoadCheck = new Button(comp, SWT.CHECK);
        distributedLoadCheck.setText("Load project code on all connected nodes");
        distributedLoadCheck.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false,
                false, 2, 1));
        new Label(comp, SWT.NONE);
        distributedLoadCheck.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetDefaultSelected(final SelectionEvent e) {
                updateLaunchConfigurationDialog();
            }

            @Override
            public void widgetSelected(final SelectionEvent e) {
                updateLaunchConfigurationDialog();
            }
        });

        updateLaunchConfigurationDialog();
    }

    @Override
    public void setDefaults(final ILaunchConfigurationWorkingCopy config) {
        config.setAttribute(ErlRuntimeAttributes.START_ME, true);
        config.setAttribute(ErlRuntimeAttributes.RUNTIME_NAME,
                ErlRuntimeAttributes.DEFAULT_RUNTIME_NAME);
        config.setAttribute(ErlRuntimeAttributes.NODE_NAME, "");
        config.setAttribute(ErlRuntimeAttributes.COOKIE, "");
        config.setAttribute(ErlRuntimeAttributes.WORKING_DIR,
                ErlRuntimeAttributes.DEFAULT_WORKING_DIR);
        config.setAttribute(ErlRuntimeAttributes.EXTRA_ARGS, "");
        config.setAttribute(ErlRuntimeAttributes.LOAD_ALL_NODES, false);
    }

    @Override
    public void initializeFrom(final ILaunchConfiguration config) {
        try {
            final String runtimeName = config.getAttribute(
                    ErlRuntimeAttributes.RUNTIME_NAME, "");
            runtimesCombo.select(runtimesCombo.indexOf(runtimeName));
        } catch (final CoreException e) {
            runtimesCombo.setText("");
        }
        try {
            final String node = config.getAttribute(ErlRuntimeAttributes.NODE_NAME, "");
            nameText.setText(node);
        } catch (final CoreException e) {
            nameText.setText("");
        }
        try {
            final boolean longName = config.getAttribute(
                    ErlRuntimeAttributes.USE_LONG_NAME, true);
            longNameButton.setSelection(longName);
            shortNameButton.setSelection(!longName);
        } catch (final CoreException e) {
            nameText.setText("");
        }
        try {
            final String cookie = config.getAttribute(ErlRuntimeAttributes.COOKIE, "");
            cookieText.setText(cookie);
        } catch (final CoreException e) {
            cookieText.setText("");
        }
        try {
            final boolean startMe = config.getAttribute(ErlRuntimeAttributes.START_ME,
                    true);
            startNodeCheckbox.setSelection(startMe);
        } catch (final CoreException e) {
            startNodeCheckbox.setSelection(false);
        }
        try {
            final String wdir = config.getAttribute(ErlRuntimeAttributes.WORKING_DIR,
                    ErlRuntimeAttributes.DEFAULT_WORKING_DIR);
            workingDirText.setText(wdir);
        } catch (final CoreException e) {
            workingDirText.setText(ErlRuntimeAttributes.DEFAULT_WORKING_DIR);
        }
        try {
            final String xtra = config.getAttribute(ErlRuntimeAttributes.EXTRA_ARGS, "");
            argsText.setText(xtra);
        } catch (final CoreException e) {
            argsText.setText("");
        }
        try {
            final boolean loadAll = config.getAttribute(
                    ErlRuntimeAttributes.LOAD_ALL_NODES, false);
            distributedLoadCheck.setSelection(loadAll);
        } catch (final CoreException e) {
            distributedLoadCheck.setSelection(true);
        }
    }

    @Override
    public void performApply(final ILaunchConfigurationWorkingCopy config) {
        config.setAttribute(ErlRuntimeAttributes.RUNTIME_NAME, runtimesCombo.getText());
        config.setAttribute(ErlRuntimeAttributes.START_ME,
                startNodeCheckbox.getSelection());
        config.setAttribute(ErlRuntimeAttributes.NODE_NAME, nameText.getText().trim());
        config.setAttribute(ErlRuntimeAttributes.USE_LONG_NAME,
                longNameButton.getSelection());
        config.setAttribute(ErlRuntimeAttributes.COOKIE, cookieText.getText().trim());
        config.setAttribute(ErlRuntimeAttributes.WORKING_DIR, workingDirText.getText());
        config.setAttribute(ErlRuntimeAttributes.EXTRA_ARGS, argsText.getText().trim());
        config.setAttribute(ErlRuntimeAttributes.LOAD_ALL_NODES,
                distributedLoadCheck.getSelection());
    }

    @Override
    public String getName() {
        return "Runtimes";
    }

    @Override
    public boolean isValid(final ILaunchConfiguration config) {
        setErrorMessage(null);
        final String name = nameText.getText().trim();
        if (!"".equals(name) && !RuntimeData.validateNodeName(name)) {
            setErrorMessage(String.format("Node name '%s' is invalid.", name));
            return false;
        }
        String workingDir = workingDirText.getText();
        if (workingDir.equals(".")) {
            workingDir = "";
        }
        final File d = new File(workingDir);
        if (!d.isAbsolute()) {
            final String wspace = ResourcesPlugin.getWorkspace().getRoot().getLocation()
                    .toPortableString();
            workingDir = wspace + "/" + workingDir;
            workingDirText.setText(workingDir);
        }
        if (!d.exists()) {
            setErrorMessage(String.format("Working dir '%s' doesn't exist.", workingDir));
            return false;
        }
        return true;
    }

    @Override
    protected void updateLaunchConfigurationDialog() {
        updateDialogStatus();
        super.updateLaunchConfigurationDialog();
    }

    private void updateDialogStatus() {
        setMessage(null);
        final NodeHostClassifier state = new NodeHostClassifier(nameText.getText().trim());

        longNameButton.setEnabled(!(state.mode == NodeType.LOCAL_STANDALONE)
                && state.host == HostnameType.NONE && HostnameUtils.canUseLongNames());
        shortNameButton.setEnabled(!(state.mode == NodeType.LOCAL_STANDALONE)
                && state.host == HostnameType.NONE && HostnameUtils.canUseShortNames());
        ErlLogger.debug("state.mode=%s, state.host=%s", state.mode, state.host);

        switch (state.host) {
        case LONG:
            longNameButton.setSelection(true);
            shortNameButton.setSelection(false);
            break;
        case SHORT:
            longNameButton.setSelection(false);
            shortNameButton.setSelection(true);
            break;
        case NONE:
            if (!HostnameUtils.canUseShortNames()) {
                longNameButton.setSelection(true);
                shortNameButton.setSelection(false);
            }
            if (!HostnameUtils.canUseLongNames()) {
                longNameButton.setSelection(false);
                shortNameButton.setSelection(true);
            }
            break;
        }

        final boolean isLong = longNameButton.getSelection();
        if (state.host == HostnameType.NONE) {
            nodeHostLabel.setText("@" + HostnameUtils.getErlangHostName(isLong));
        } else {
            nodeHostLabel.setText("");
        }
        cookieText.setEnabled(!(state.mode == NodeType.LOCAL_STANDALONE));
        startNodeCheckbox.setEnabled(!(state.mode == NodeType.REMOTE));
        workingDirText.setEnabled(!(state.mode == NodeType.REMOTE)
                && startNodeCheckbox.getSelection());
        argsText.setEnabled(!(state.mode == NodeType.REMOTE)
                && startNodeCheckbox.getSelection());
        distributedLoadCheck.setEnabled(!(state.mode == NodeType.LOCAL_STANDALONE));
        distributedLoadCheck.setSelection(true);

        if (state.mode == NodeType.LOCAL_STANDALONE) {
            setMessage("NOTE: The Erlang node will be started as not distributed. "
                    + "Some functionality (like Eclipse debugger) won't work. "
                    + "If you want a distributed node, enter a node name in the field below.");
        }
    }
}
