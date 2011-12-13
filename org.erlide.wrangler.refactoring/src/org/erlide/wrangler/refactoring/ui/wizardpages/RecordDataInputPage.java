/*******************************************************************************
 * Copyright (c) 2010 György Orosz.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     György Orosz - initial API and implementation
 ******************************************************************************/
package org.erlide.wrangler.refactoring.ui.wizardpages;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.erlide.wrangler.refactoring.core.internal.QuickCheckStateRefactoring;
import org.erlide.wrangler.refactoring.ui.validator.AtomValidator;
import org.erlide.wrangler.refactoring.ui.validator.IValidator;

/**
 * Wizard pages, on which the user can input the necessary data for a record
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class RecordDataInputPage extends MultiInputPage {

    IValidator validator;

    protected ArrayList<Text> fieldNames;
    protected ArrayList<Label> fieldNameLabels;

    protected Label recordNameLabel;
    protected Text recordName;

    protected Composite composite;

    protected QuickCheckStateRefactoring refactoring;

    /**
     * Constructor
     * 
     * @param name
     *            title
     */
    public RecordDataInputPage(final String name) {
        super(name);
    }

    @Override
    public void createControl(final Composite parent) {
        refactoring = (QuickCheckStateRefactoring) getRefactoring();
        composite = new Composite(parent, SWT.NONE);

        recordNameLabel = new Label(composite, SWT.LEFT);
        recordNameLabel.setText("Record name:");
        GridData gridData = new GridData();
        gridData.horizontalAlignment = GridData.FILL;
        gridData.horizontalSpan = 2;
        recordNameLabel.setLayoutData(gridData);

        recordName = new Text(composite, SWT.NONE);
        gridData = new GridData();
        gridData.horizontalAlignment = GridData.FILL;
        gridData.horizontalSpan = 2;
        gridData.grabExcessHorizontalSpace = true;
        recordName.setLayoutData(gridData);

        final ModifyListener modifyListener = new ModifyListener() {

            @Override
            public void modifyText(final ModifyEvent e) {
                isInputValid();
            }

        };

        recordName.addModifyListener(modifyListener);

        // adding field name inputs
        final int n = refactoring.getRecordFieldCount();
        fieldNameLabels = new ArrayList<Label>();
        fieldNames = new ArrayList<Text>();
        for (int i = 0; i < n; ++i) {
            final Label l = new Label(composite, SWT.LEFT);
            l.setText("Field name (" + i + "):");
            GridData gd = new GridData();
            gd.horizontalAlignment = GridData.FILL;
            gd.horizontalSpan = 2;
            l.setLayoutData(gridData);

            fieldNameLabels.add(l);

            final Text t = new Text(composite, SWT.NONE);
            gd = new GridData();
            gd.horizontalAlignment = GridData.FILL;
            gd.horizontalSpan = 2;
            gd.grabExcessHorizontalSpace = true;
            t.setLayoutData(gridData);

            fieldNames.add(t);

            t.addModifyListener(modifyListener);
        }

        final GridLayout layout = new GridLayout();
        layout.numColumns = 2;
        composite.setLayout(layout);

        setControl(composite);
    }

    @Override
    protected boolean isInputValid() {
        final IValidator theValidator = new AtomValidator();
        boolean valid = theValidator.isValid(recordName.getText());

        final ArrayList<String> fn = new ArrayList<String>();
        for (final Text t : fieldNames) {
            valid = valid && theValidator.isValid(t.getText());
            fn.add(t.getText());
            if (!valid) {
                break;
            }
        }

        if (valid) {
            refactoring.setRecordData(recordName.getText(), fn);
            setErrorMessage(null);
            setPageComplete(true);
        } else {
            setPageComplete(false);
            setErrorMessage("Please provide valid record name, and field names!");
        }

        return valid;
    }
}
