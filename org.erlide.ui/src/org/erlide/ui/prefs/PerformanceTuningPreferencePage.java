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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.erlide.core.ErlangPlugin;
import org.erlide.ui.util.PerformanceTuning;

public class PerformanceTuningPreferencePage extends PreferencePage implements
        IWorkbenchPreferencePage {
    private Text foldingText;

    public PerformanceTuningPreferencePage() {
    }

    @Override
    protected Control createContents(final Composite parent) {
        final PerformanceTuning pt = PerformanceTuning.get();

        final Composite panel = new Composite(parent, SWT.NONE);
        panel.setLayout(new GridLayout(1, false));

        final Group grpLimitingFunctionalityFor = new Group(panel, SWT.NONE);
        grpLimitingFunctionalityFor
                .setText("Limiting functionality for large files");
        grpLimitingFunctionalityFor.setLayout(new GridLayout(3, false));
        grpLimitingFunctionalityFor.setLayoutData(new GridData(SWT.FILL,
                SWT.CENTER, true, false, 1, 1));

        final Label lblFoldingIsDisabled = new Label(
                grpLimitingFunctionalityFor, SWT.NONE);
        lblFoldingIsDisabled.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER,
                false, false, 1, 1));
        lblFoldingIsDisabled
                .setText("Folding is disabled for files larger than");

        foldingText = new Text(grpLimitingFunctionalityFor, SWT.BORDER);
        final GridData gd_foldingText = new GridData(SWT.LEFT, SWT.CENTER,
                false, false, 1, 1);
        gd_foldingText.widthHint = 40;
        foldingText.setLayoutData(gd_foldingText);
        foldingText.setText(Integer.toString(pt.getFoldingLimit()));

        final Label lblLines = new Label(grpLimitingFunctionalityFor, SWT.NONE);
        lblLines.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false,
                1, 1));
        lblLines.setText("lines.");

        final Label lblNewLabel = new Label(grpLimitingFunctionalityFor,
                SWT.NONE);
        lblNewLabel.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false,
                false, 3, 1));
        lblNewLabel
                .setText("In the future, we might add limits for other functions (outline, etc)");
        final String version = ErlangPlugin.getDefault().getCore()
                .getFeatureVersion();

        return panel;
    }

    @Override
    public void init(final IWorkbench workbench) {
    }

    @Override
    protected void performDefaults() {
        final PerformanceTuning pt = PerformanceTuning.getDefault();
        foldingText.setText(Integer.toString(pt.getFoldingLimit()));
    }

    @Override
    public boolean performOk() {
        final PerformanceTuning pt = PerformanceTuning.getDefault();
        pt.setFoldingLimit(Integer.parseInt(foldingText.getText()));
        pt.store();
        return true;
    }
}
