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
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.erlide.core.backend.BackendCore;
import org.erlide.core.backend.ErlLaunchAttributes;
import org.erlide.core.backend.runtimeinfo.RuntimeInfo;
import org.erlide.ui.util.SWTUtil;

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

    /**
     * @wbp.parser.entryPoint
     */
    public void createControl(final Composite parent) {
        runtimes = BackendCore.getRuntimeInfoManager().getRuntimes();

        final Composite comp = new Composite(parent, SWT.NONE);
        setControl(comp);
        final GridLayout topLayout = new GridLayout();
        comp.setLayout(topLayout);

        final Group runtimeGroup = SWTUtil.createGroup(comp, "Backend", 3,
                GridData.FILL_HORIZONTAL);

        final GridData gd_runtimeGroup = new GridData(SWT.FILL, SWT.CENTER,
                false, false);
        gd_runtimeGroup.heightHint = 250;
        runtimeGroup.setLayoutData(gd_runtimeGroup);

        final Label runtimeLabel = new Label(runtimeGroup, SWT.NONE);
        runtimeLabel.setText("Name");

        final List<String> rtl = new ArrayList<String>();
        for (final RuntimeInfo r : runtimes) {
            rtl.add(r.getName());
        }
        final String[] rts = rtl.toArray(new String[] {});
        final RuntimeInfo defaultRuntime = BackendCore.getRuntimeInfoManager()
                .getDefaultRuntime();
        final int db = defaultRuntime == null ? 0 : Arrays.binarySearch(rts,
                defaultRuntime.getName());

        runtimesCombo = new Combo(runtimeGroup, SWT.READ_ONLY);
        runtimesCombo.setLayoutData(new GridData(174, SWT.DEFAULT));
        runtimesCombo.addSelectionListener(new SelectionAdapter() {
            @SuppressWarnings("synthetic-access")
            @Override
            public void widgetSelected(final SelectionEvent e) {
                updateLaunchConfigurationDialog();
            }
        });
        runtimesCombo.setItems(rts);
        runtimesCombo.select(db);
        new Label(runtimeGroup, SWT.NONE);

        final Label nodeNameLabel = new Label(runtimeGroup, SWT.NONE);
        nodeNameLabel.setText("Node name");

        nameText = new Text(runtimeGroup, SWT.BORDER);
        final GridData gd_nameText = new GridData(SWT.FILL, SWT.CENTER, false,
                false, 2, 1);
        gd_nameText.widthHint = 333;
        nameText.setLayoutData(gd_nameText);
        nameText.addModifyListener(new ModifyListener() {
            @SuppressWarnings("synthetic-access")
            public void modifyText(final ModifyEvent e) {
                final boolean isRemote = nameText.getText().contains("@");
                startNodeCheckbox.setEnabled(!isRemote);
                if (isRemote) {
                    startNodeCheckbox.setSelection(false);
                }
                final boolean isNotDistributed = nameText.getText().trim()
                        .equals("");
                longNameButton.setEnabled(!isNotDistributed);
                shortNameButton.setEnabled(!isNotDistributed);
                cookieText.setEnabled(!isNotDistributed);
                startNodeCheckbox.setEnabled(!isNotDistributed);
                if (isNotDistributed) {
                    setMessage("NOTE: The Erlang node will be started as not distributed. "
                            + "The integrated debugger won't work, use the OTP one instead.\n"
                            + "If you want a distributed node, enter a node name in the field below.");
                } else {
                    setMessage(null);
                }
                updateLaunchConfigurationDialog();
            }
        });
        new Label(runtimeGroup, SWT.NONE);

        longNameButton = new Button(runtimeGroup, SWT.RADIO);
        longNameButton.addSelectionListener(new SelectionAdapter() {
            @SuppressWarnings("synthetic-access")
            @Override
            public void widgetSelected(final SelectionEvent e) {
                updateLaunchConfigurationDialog();
            }
        });
        longNameButton.setSelection(true);
        longNameButton.setText("long name (-name)");

        shortNameButton = new Button(runtimeGroup, SWT.RADIO);
        shortNameButton.setText("short name (-sname)");

        final Label cookieLabel = new Label(runtimeGroup, SWT.NONE);
        cookieLabel.setToolTipText("Leave empty to use default one");
        cookieLabel.setText("Cookie");

        cookieText = new Text(runtimeGroup, SWT.BORDER);
        final GridData gd_cookieText = new GridData(SWT.FILL, SWT.CENTER,
                false, false, 2, 1);
        gd_cookieText.widthHint = 232;
        cookieText.setLayoutData(gd_cookieText);
        cookieText.addModifyListener(new ModifyListener() {
            @SuppressWarnings("synthetic-access")
            public void modifyText(final ModifyEvent e) {
                updateLaunchConfigurationDialog();
            }
        });
        cookieText.setToolTipText("Leave empty to use default one");
        new Label(runtimeGroup, SWT.NONE);

        startNodeCheckbox = new Button(runtimeGroup, SWT.CHECK);
        startNodeCheckbox.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER,
                false, false, 2, 1));
        startNodeCheckbox.setSelection(true);
        startNodeCheckbox
                .setText("Start the Erlang node if not running already");
        startNodeCheckbox.addSelectionListener(new SelectionAdapter() {
            @SuppressWarnings("synthetic-access")
            @Override
            public void widgetSelected(final SelectionEvent e) {
                updateLaunchConfigurationDialog();
            }
        });

        final Label workingDirectoryLabel = new Label(runtimeGroup, SWT.NONE);
        workingDirectoryLabel.setText("Working directory");

        workingDirText = new Text(runtimeGroup, SWT.BORDER);
        workingDirText.setToolTipText("may be relative to the workspace");
        workingDirText.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true,
                false, 2, 1));
        workingDirText.addModifyListener(new ModifyListener() {
            @SuppressWarnings("synthetic-access")
            public void modifyText(final ModifyEvent e) {
                updateLaunchConfigurationDialog();
            }
        });

        final Label extraArgumentsLabel = new Label(runtimeGroup, SWT.NONE);
        extraArgumentsLabel.setText("Extra arguments");

        argsText = new Text(runtimeGroup, SWT.BORDER);
        argsText.setToolTipText("as on the command line\nBe careful about proper quoting!");
        argsText.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false,
                2, 1));
        argsText.addModifyListener(new ModifyListener() {
            @SuppressWarnings("synthetic-access")
            public void modifyText(final ModifyEvent e) {
                updateLaunchConfigurationDialog();
            }
        });
        final Label extraArgumentsLabel2 = new Label(runtimeGroup, SWT.NONE);
        extraArgumentsLabel2.setText("(overrides runtime setting)");
        extraArgumentsLabel2.setLayoutData(new GridData(SWT.FILL, SWT.CENTER,
                true, false, 3, 1));

        new Label(runtimeGroup, SWT.NONE);
        distributedLoadCheck = createCheckButton(runtimeGroup,
                "Load code on all connected nodes");
        distributedLoadCheck.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER,
                false, false, 2, 1));
        distributedLoadCheck.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(final SelectionEvent e) {
                updateLaunchConfigurationDialog();
            }

            public void widgetSelected(final SelectionEvent e) {
                updateLaunchConfigurationDialog();
            }
        });

    }

    public void setDefaults(final ILaunchConfigurationWorkingCopy config) {
        config.setAttribute(ErlLaunchAttributes.START_ME, true);
        config.setAttribute(ErlLaunchAttributes.RUNTIME_NAME,
                ErlLaunchAttributes.DEFAULT_RUNTIME_NAME);
        config.setAttribute(ErlLaunchAttributes.NODE_NAME, "");
        config.setAttribute(ErlLaunchAttributes.COOKIE, "");
        config.setAttribute(ErlLaunchAttributes.WORKING_DIR,
                ErlLaunchAttributes.DEFAULT_WORKING_DIR);
        config.setAttribute(ErlLaunchAttributes.EXTRA_ARGS, "");
        config.setAttribute(ErlLaunchAttributes.LOAD_ALL_NODES, false);
    }

    public void initializeFrom(final ILaunchConfiguration config) {
        try {
            final String runtimeName = config.getAttribute(
                    ErlLaunchAttributes.RUNTIME_NAME, "");
            runtimesCombo.select(runtimesCombo.indexOf(runtimeName));
        } catch (final CoreException e) {
            runtimesCombo.setText("");
        }
        try {
            final String node = config.getAttribute(
                    ErlLaunchAttributes.NODE_NAME, "");
            nameText.setText(node);
        } catch (final CoreException e) {
            nameText.setText("");
        }
        try {
            final boolean longName = config.getAttribute(
                    ErlLaunchAttributes.USE_LONG_NAME, true);
            longNameButton.setSelection(longName);
            shortNameButton.setSelection(!longName);
        } catch (final CoreException e) {
            nameText.setText("");
        }
        try {
            final String cookie = config.getAttribute(
                    ErlLaunchAttributes.COOKIE, "");
            cookieText.setText(cookie);
        } catch (final CoreException e) {
            cookieText.setText("");
        }
        try {
            final boolean startMe = config.getAttribute(
                    ErlLaunchAttributes.START_ME, true);
            startNodeCheckbox.setSelection(startMe);
        } catch (final CoreException e) {
            startNodeCheckbox.setSelection(false);
        }
        try {
            final String wdir = config.getAttribute(
                    ErlLaunchAttributes.WORKING_DIR,
                    ErlLaunchAttributes.DEFAULT_WORKING_DIR);
            workingDirText.setText(wdir);
        } catch (final CoreException e) {
            workingDirText.setText(ErlLaunchAttributes.DEFAULT_WORKING_DIR);
        }
        try {
            final String xtra = config.getAttribute(
                    ErlLaunchAttributes.EXTRA_ARGS, "");
            argsText.setText(xtra);
        } catch (final CoreException e) {
            argsText.setText("");
        }
        try {
            final boolean loadAll = config.getAttribute(
                    ErlLaunchAttributes.LOAD_ALL_NODES, false);
            distributedLoadCheck.setSelection(loadAll);
        } catch (final CoreException e) {
            distributedLoadCheck.setSelection(false);
        }
    }

    public void performApply(final ILaunchConfigurationWorkingCopy config) {
        config.setAttribute(ErlLaunchAttributes.RUNTIME_NAME,
                runtimesCombo.getText());
        config.setAttribute(ErlLaunchAttributes.START_ME,
                startNodeCheckbox.getSelection());
        config.setAttribute(ErlLaunchAttributes.NODE_NAME, nameText.getText()
                .trim());
        config.setAttribute(ErlLaunchAttributes.USE_LONG_NAME,
                longNameButton.getSelection());
        config.setAttribute(ErlLaunchAttributes.COOKIE, cookieText.getText()
                .trim());
        config.setAttribute(ErlLaunchAttributes.WORKING_DIR,
                workingDirText.getText());
        config.setAttribute(ErlLaunchAttributes.EXTRA_ARGS, argsText.getText()
                .trim());
        config.setAttribute(ErlLaunchAttributes.LOAD_ALL_NODES,
                distributedLoadCheck.getSelection());
    }

    public String getName() {
        return "Runtimes";
    }

    @Override
    public boolean isValid(final ILaunchConfiguration config) {
        setErrorMessage(null);
        final String name = nameText.getText().trim();
        if (!name.equals("") && !RuntimeInfo.validateNodeName(name)) {
            setErrorMessage(String.format("Node name '%s' is invalid.", name));
            return false;
        }
        String workingDir = workingDirText.getText();
        File d = new File(workingDir);
        if (d.isAbsolute()) {
            if (!d.exists()) {
                setErrorMessage(String.format(
                        "Working dir '%s' doesn't exist.", workingDir));
                return false;
            }
        } else {
            final String wspace = ResourcesPlugin.getWorkspace().getRoot()
                    .getLocation().toPortableString();
            workingDir = wspace + "/" + workingDir;
            d = new File(workingDir);
            if (!d.exists()) {
                setErrorMessage(String.format(
                        "Working dir '%s' doesn't exist.", workingDir));
                return false;
            }
        }
        return true;
    }
}
