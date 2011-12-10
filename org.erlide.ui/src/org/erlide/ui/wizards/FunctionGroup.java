/*******************************************************************************
 * Copyright (c) 2004 Lukas Larsson and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Lukas Larsson
 *******************************************************************************/

package org.erlide.ui.wizards;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.erlide.jinterface.ErlLogger;

/**
 * @author Lukas Larsson
 */
public class FunctionGroup implements SelectionListener {

    public static final int FSM = 1;

    public static final int NONE = 2;

    // private final String currentSkeleton = "None";

    private Table fTable;
    private TableItem fEditingItem;
    Text functionNameText;
    Text arityText;
    private final TableColumn tableColumns[] = new TableColumn[2];
    Button addFunctionBtn;
    // Button addStateBtn;
    Button removeFunctionBtn;
    Button editFunctionBtn;
    Button exportButtonBtn;
    ErlangFileWizardPage fWizPage;

    FunctionGroup(final Composite parent, final ErlangFileWizardPage wizPage) {
        final Group root = new Group(parent, SWT.NULL);
        fWizPage = wizPage;
        root.setText("Functions ");
        GridData gd = new GridData(SWT.CENTER, SWT.CENTER, false, false);
        root.setLayoutData(gd);
        final GridLayout layout = new GridLayout(1, true);
        root.setLayout(layout);

        final Composite container = new Composite(root, SWT.NULL);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        container.setLayoutData(gd);
        final RowLayout rlayout = new RowLayout(SWT.HORIZONTAL);
        rlayout.fill = true;
        container.setLayout(rlayout);

        createTable(container);
        createButtons(container);

        createInputField(root);

        dialogChanged();
    }

    private void createTable(final Composite parent) {
        fTable = new Table(parent, SWT.SINGLE | SWT.V_SCROLL
                | SWT.FULL_SELECTION | SWT.CHECK);
        fTable.showSelection();
        fTable.setHeaderVisible(true);
        fTable.setLinesVisible(true);
        fTable.addSelectionListener(this);
        fTable.setItemCount(5);

        tableColumns[0] = new TableColumn(fTable, SWT.CENTER);
        tableColumns[0].setText("Export / Name");
        tableColumns[0].setWidth(200);
        tableColumns[0].addSelectionListener(this);

        tableColumns[1] = new TableColumn(fTable, SWT.CENTER);
        tableColumns[1].setText("Arity");
        tableColumns[1].setWidth(50);
        tableColumns[1].addSelectionListener(this);

    }

    private void createButtons(final Composite parent) {
        final Composite container = new Composite(parent, SWT.NULL);
        final GridLayout fill = new GridLayout(1, false);
        container.setLayout(fill);

        removeFunctionBtn = new Button(container, SWT.PUSH);
        removeFunctionBtn.setText("Remove Function");
        // removeFunction.setLayoutData(new
        // GridData(SWT.FILL,SWT.CENTER,true,false));
        removeFunctionBtn.addSelectionListener(this);

        editFunctionBtn = new Button(container, SWT.PUSH);
        editFunctionBtn.setText("    Edit Function   ");
        // editFunction.setLayoutData(new
        // GridData(SWT.FILL,SWT.CENTER,true,false));
        editFunctionBtn.addSelectionListener(this);

        /*
         * addState = new Button(container,SWT.PUSH); addState.setText(" Toggle
         * State "); // addState.setLayoutData(new
         * GridData(SWT.FILL,SWT.CENTER,true,false));
         * addState.addSelectionListener(this);
         */
    }

    private void createInputField(final Composite parent) {
        final Composite container = new Composite(parent, SWT.NULL);
        container
                .setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
        final GridLayout layout = new GridLayout(4, false);
        container.setLayout(layout);

        exportButtonBtn = new Button(container, SWT.CHECK);
        exportButtonBtn.setSelection(true);
        exportButtonBtn.addSelectionListener(this);

        functionNameText = new Text(container, SWT.BORDER | SWT.SINGLE);
        functionNameText.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true,
                false));
        functionNameText.addModifyListener(new ModifyListener() {

            @Override
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });

        arityText = new Text(container, SWT.BORDER | SWT.SINGLE);
        arityText.addModifyListener(new ModifyListener() {

            @Override
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        arityText.addKeyListener(new KeyListener() {

            @Override
            public void keyPressed(final KeyEvent e) {
                ErlLogger.debug(e.character + " was pressed.");
                fWizPage.gettingInput = true;
            }

            @Override
            public void keyReleased(final KeyEvent e) {
                ErlLogger.debug(e.character + " was released.");
                fWizPage.gettingInput = false;
            }
        });

        addFunctionBtn = new Button(container, SWT.PUSH);
        addFunctionBtn.setText("          Apply          ");
        addFunctionBtn
                .setToolTipText("Create a new function or edit an existing one.");
        addFunctionBtn.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetSelected(final SelectionEvent e) {
                addFunction(functionNameText.getText(),
                        Integer.parseInt(arityText.getText()),
                        FunctionGroup.NONE, exportButtonBtn.getSelection());
                functionNameText.setText("");
                arityText.setText("0");
            }

            @Override
            public void widgetDefaultSelected(final SelectionEvent e) {
            }
        });

    }

    /**
     * TODO Write description of addFunction in FunctionGroup.
     * 
     * @param name
     * @param arity
     * @param type
     * @param exported
     */
    void addFunction(final String name, final int arity, final int type,
            final boolean exported) {
        final TableItem item = fEditingItem != null ? fEditingItem
                : new TableItem(fTable, SWT.NULL, 0);
        item.setChecked(exported);
        item.setText(0, name + getPostfix(type));
        item.setText(1, "" + arity);
        fEditingItem = null;
        final TableItem lastItem = fTable.getItem(fTable.getItemCount() - 1);
        if ("".equals(lastItem.getText(0))) {
            fTable.remove(fTable.getItemCount() - 1);
        }
    }

    /**
     * TODO Write description of getPostfix in FunctionGroup.
     * 
     * @param type
     * @return
     */
    private String getPostfix(final int type) {
        switch (type) {
        case FunctionGroup.FSM:
            return "(State)";
        default:
            return "";
        }
    }

    /**
     * TODO Write description of dialogChanged in FunctionGroup.
     * 
     */
    void dialogChanged() {
        if (functionNameText.getText().length() != 0) {
            final TableItem allTableItems[] = fTable.getItems();
            for (final TableItem element : allTableItems) {
                if (element.getText(0).equals(functionNameText.getText())
                        && element.getText(1).equals(arityText.getText())) {
                    updateStatus("Function already exists!");
                    return;
                }
            }
            // check to see if this is a valid function name
            final int functionNameASCII = functionNameText.getText().charAt(0);
            if (!(functionNameASCII >= 'a' && functionNameASCII <= 'z')
                    && functionNameASCII != '\'') {
                updateStatus("Function name has to be a valid erlang atom.");
                return;
            }
            try {
                Integer.parseInt(arityText.getText());
            } catch (final NumberFormatException e) {
                updateStatus("The arity has to be an integer number.");
                return;
            }
            updateStatus(null);
        } else {
            updateStatus(null);
            addFunctionBtn.setEnabled(false);
        }

    }

    private void updateStatus(final String message) {
        // The wizard page
        fWizPage.setErrorMessage(message);
        fWizPage.setPageComplete(message == null);

        // local buttons used for data input
        addFunctionBtn.setEnabled(message == null);
    }

    public void enableFSM(final boolean value) {
        // addState.setEnabled(value);
    }

    public Function[] getFunctionData() {
        removeEdit();

        final TableItem function[] = fTable.getItems();
        final ArrayList<Function> functionList = new ArrayList<Function>();
        for (final TableItem element : function) {
            if (!"".equals(element.getText(0))) {
                final Function f = new Function();
                f.arity = Integer.parseInt(element.getText(1));
                f.isExported = element.getChecked();
                final String[] part = element.getText(0).split("\\(");
                f.isState = "State)".equals(part[part.length - 1]);
                if (f.isState) {
                    f.name = element.getText(0).substring(0,
                            element.getText(0).length() - 7);
                } else {
                    f.name = element.getText(0);
                }
                functionList.add(f);
            }
        }
        final Function[] functions = functionList
                .toArray(new Function[functionList.size()]);

        return functions;
    }

    @Override
    public void widgetDefaultSelected(final SelectionEvent e) {
        // do nothing, I don't even know when this event is triggered.
    }

    @Override
    public void widgetSelected(final SelectionEvent e) {
        ErlLogger.debug("widgetSelected: e.item = " + e.item);
        ErlLogger.debug("widgetSelected: e.widget = " + e.widget);

        if (e.widget == removeFunctionBtn) {
            final TableItem itemsToRemove[] = fTable.getSelection();
            for (final TableItem element : itemsToRemove) {
                fTable.remove(fTable.indexOf(element));
                if (element == fEditingItem) {
                    fEditingItem = null;
                }
            }

        } else if (e.widget == editFunctionBtn) {
            if (fEditingItem != null) {
                removeEdit();
            }
            final int selectedIndex = fTable.getSelectionIndex();
            final TableItem selectedItem = fTable.getItem(selectedIndex);
            if (!"".equals(selectedItem.getText(0))) {
                fEditingItem = selectedItem;
                functionNameText.setText(selectedItem.getText(0));
                arityText.setText(selectedItem.getText(1));
                exportButtonBtn.setSelection(selectedItem.getChecked());
                fEditingItem.setText(0, selectedItem.getText(0)
                        + "<<Being Edited>>");
            }
            // } else if (e.widget == addState) {

        }
        dialogChanged();
    }

    private void removeEdit() {
        if (fEditingItem != null) {
            final String[] parts = fEditingItem.getText(0).split("<");
            final StringBuilder functionName = new StringBuilder(parts[0]);
            for (int i = 1; i < parts.length - 2; i++) {
                functionName.append('<').append(parts[i]);
            }
            fEditingItem.setText(0, functionName.toString());
            fEditingItem = null;
        }
    }
}
