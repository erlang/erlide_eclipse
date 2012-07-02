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
package org.erlide.ui.prefs;

import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
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
import org.eclipse.wb.swt.ResourceManager;
import org.erlide.backend.BackendUtils;
import org.erlide.core.ErlangPlugin;
import org.erlide.ui.internal.ErlideUIPlugin;

public class ErlangPreferencePage extends PreferencePage implements
        IWorkbenchPreferencePage {
    private Text txtLocalErlangNodes;

    public ErlangPreferencePage() {
    }

    @Override
    protected Control createContents(final Composite parent) {
        noDefaultAndApplyButton();
        final Composite panel = new Composite(parent, SWT.NONE);
        final GridLayout layout = new GridLayout();
        layout.numColumns = 2;
        panel.setLayout(layout);

        final Label img = new Label(panel, SWT.NONE);
        img.setLayoutData(new GridData(79, SWT.DEFAULT));
        img.setImage(ResourceManager.getPluginImage(
                ErlideUIPlugin.getDefault(), "icons/full/obj16/erlang058.gif"));

        final Group composite = new Group(panel, SWT.NONE);
        final GridData gd_composite = new GridData(SWT.FILL, SWT.CENTER, false,
                false);
        gd_composite.widthHint = 356;
        composite.setLayoutData(gd_composite);
        composite.setLayout(new GridLayout());

        final Label text = new Label(composite, SWT.NONE);
        text.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false));
        text.setToolTipText("Vlad Dumitrescu, Jakob Cederlund and others");
        text.setText(PreferenceMessages.ErlangPreferencePage_2);

        final Label textv = new Label(composite, SWT.NONE);
        textv.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false));
        final String version = ErlangPlugin.getDefault().getCore()
                .getFeatureVersion();
        textv.setText("    version " + version);

        final Link erlideorgLink = new Link(composite, SWT.NONE);
        erlideorgLink.setText(PreferenceMessages.ErlangPreferencePage_3);

        final Link updateLink = new Link(composite, SWT.NONE);
        updateLink.setText(PreferenceMessages.ErlangPreferencePage_4);
        new Label(panel, SWT.NONE);

        final Button reportButton = new Button(panel, SWT.NONE);
        reportButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                PreferencesUtil.createPreferenceDialogOn(getShell(),
                        "org.erlide.ui.reporting", null, null);
            }
        });
        reportButton.setText("Report problems");
        new Label(panel, SWT.NONE);

        txtLocalErlangNodes = new Text(panel, SWT.BORDER | SWT.READ_ONLY
                | SWT.MULTI);
        txtLocalErlangNodes
                .setText("This machine supports local Erlang nodes with only short names \nbecause of its hostname configuration. \n\nTo enable long names locally, make sure that the machine \nhas a proper FQDN on the network. ");
        final GridData gd_txtLocalErlangNodes = new GridData(SWT.FILL,
                SWT.CENTER, true, false, 1, 1);
        gd_txtLocalErlangNodes.widthHint = 339;
        gd_txtLocalErlangNodes.heightHint = 87;
        txtLocalErlangNodes.setLayoutData(gd_txtLocalErlangNodes);
        txtLocalErlangNodes.setVisible(BackendUtils.longNamesDontWork());

        return panel;
    }

    @Override
    public void init(final IWorkbench workbench) {
    }
}
