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
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.erlide.wrangler.refactoring.core.SimpleOneStepWranglerRefactoring;
import org.erlide.wrangler.refactoring.core.SimpleWranglerRefactoring;
import org.erlide.wrangler.refactoring.ui.validator.AtomValidator;
import org.erlide.wrangler.refactoring.ui.validator.IValidator;

/**
 * Input page which displays a combo input element, and offers to select on of
 * the listed elements
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class ComboInputPage extends InputPage {

    protected String labelText;

    protected Composite composite;

    protected Label inputLabel;

    protected ArrayList<String> moduleNames;

    protected Combo selectionList;

    /**
     * Constructor
     * 
     * @param name
     *            Input page title
     * @param description
     *            description of the input page
     * @param labelText
     *            label text of the input data
     * @param moduleNames
     *            list of possible selections
     */
    public ComboInputPage(final String name, final String description,
            final String labelText, final ArrayList<String> moduleNames) {
        super(name);
        setDescription(description);
        this.labelText = labelText;
        this.moduleNames = moduleNames;

        setPageComplete(false);
    }

    @Override
    public void createControl(final Composite parent) {
        composite = new Composite(parent, SWT.NONE);

        inputLabel = new Label(composite, SWT.LEFT);
        inputLabel.setText(labelText);

        // GridData gridData = new GridData();
        // gridData.horizontalAlignment = GridData.FILL;
        // gridData.horizontalSpan = 2;
        // inputLabel.setLayoutData(gridData);

        selectionList = new Combo(composite, SWT.DROP_DOWN);
        for (final String s : moduleNames) {
            selectionList.add(s);
        }
        // gridData = new GridData();
        // gridData.horizontalAlignment = GridData.FILL;
        // gridData.horizontalSpan = 2;
        // selectionList.setLayoutData(gridData);

        // GridLayout layout = new GridLayout();
        final RowLayout layout = new RowLayout();
        layout.spacing = 5;
        layout.center = true;

        composite.setLayout(layout);

        setControl(composite);

        selectionList.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetDefaultSelected(final SelectionEvent e) {
            }

            @Override
            @SuppressWarnings("synthetic-access")
            public void widgetSelected(final SelectionEvent e) {
                ((SimpleOneStepWranglerRefactoring) getRefactoring())
                        .setUserInput(selectionList.getText());
                setPageComplete(true);
            }
        });

        final IValidator validator = new AtomValidator();

        selectionList.addModifyListener(new ModifyListener() {

            @Override
            public void modifyText(final ModifyEvent e) {
                if (validator.isValid(selectionList.getText())) {
                    ((SimpleWranglerRefactoring) getRefactoring())
                            .setUserInput(selectionList.getText());
                    setErrorMessage(null);
                    setPageComplete(true);
                } else {
                    setPageComplete(false);
                    setErrorMessage("Module name must be a a valid atom!");
                }
            }

        });

    }

    @Override
    protected boolean isInputValid() {
        return false;
    }
}
