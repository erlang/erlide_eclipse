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
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.erlide.core.ErlangPlugin;
import org.erlide.ui.internal.ErlideUIPlugin;

import com.swtdesigner.ResourceManager;

public class ErlangPreferencePage extends PreferencePage implements
        IWorkbenchPreferencePage {

    @Override
    protected Control createContents(final Composite parent) {
        noDefaultAndApplyButton();
        final Composite panel = new Composite(parent, SWT.NONE);
        final GridLayout layout = new GridLayout();
        layout.numColumns = 2;
        panel.setLayout(layout);

        final Label img = new Label(panel, SWT.NONE);
        final GridData gd_img = new GridData(79, SWT.DEFAULT);
        img.setLayoutData(gd_img);
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
        reportButton.setLayoutData(new GridData());
        reportButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                PreferencesUtil.createPreferenceDialogOn(getShell(),
                        "org.erlide.ui.reporting", null, null);
            }
        });
        reportButton.setText("Report problems");

        return panel;
    }

    @Override
    public void init(final IWorkbench workbench) {
    }

}
