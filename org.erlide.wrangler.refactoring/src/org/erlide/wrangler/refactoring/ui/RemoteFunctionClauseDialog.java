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
package org.erlide.wrangler.refactoring.ui;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlFunctionClause;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlElement;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;
import org.erlide.wrangler.refactoring.util.WranglerUtils;

/**
 * Input dialog which shows up all the function clauses in all modules in the
 * project
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class RemoteFunctionClauseDialog extends AbstractInputDialog {

    IErlMemberSelection initianlSelection = (IErlMemberSelection) GlobalParameters
            .getWranglerSelection();

    /**
     * Sets back the initial Erlide selection
     */
    public void resetSelection() {
        WranglerUtils.highlightSelection(initianlSelection.getSelectionRange()
                .getOffset(),
                initianlSelection.getSelectionRange().getLength(),
                initianlSelection);
    }

    /**
     * Constructor
     * 
     * @param parentShell
     *            shell
     * @param title
     *            Dialog title
     */
    public RemoteFunctionClauseDialog(final Shell parentShell,
            final String title) {
        super(parentShell, title);
    }

    private IErlFunctionClause functionClause = null;

    @Override
    protected Control createDialogArea(final Composite parent) {

        final Composite composite = (Composite) super.createDialogArea(parent);
        final Tree functionClausesTree;

        final Label label = new Label(composite, SWT.WRAP);
        label.setText("Please select the function clause which against should fold!");
        final GridData minToksData = new GridData(GridData.GRAB_HORIZONTAL
                | GridData.GRAB_VERTICAL | GridData.HORIZONTAL_ALIGN_FILL
                | GridData.VERTICAL_ALIGN_CENTER);
        minToksData.widthHint = convertHorizontalDLUsToPixels(IDialogConstants.MINIMUM_MESSAGE_AREA_WIDTH);
        label.setLayoutData(minToksData);
        label.setFont(parent.getFont());

        functionClausesTree = new Tree(composite, SWT.BORDER);
        final GridData treeData = new GridData(GridData.GRAB_HORIZONTAL
                | GridData.GRAB_VERTICAL | GridData.HORIZONTAL_ALIGN_FILL
                | GridData.VERTICAL_ALIGN_CENTER);
        treeData.widthHint = convertHorizontalDLUsToPixels(IDialogConstants.MINIMUM_MESSAGE_AREA_WIDTH);
        functionClausesTree.setLayoutData(treeData);

        try {
            final Collection<IErlModule> erlmodules = GlobalParameters
                    .getWranglerSelection().getErlElement().getProject()
                    .getModules();

            for (final IErlModule m : erlmodules) {
                // must refresh the scanner!
                if (/* !m.isStructureKnown() */true) {
                    // FIXME: not permitted operation
                    m.open(null);
                }

                final TreeItem moduleName = new TreeItem(functionClausesTree, 0);
                moduleName.setText(m.getModuleName());
                moduleName.setData(m);
                final List<IErlFunction> functions = filterFunctions(m
                        .getChildren());
                for (final IErlFunction f : functions) {
                    final TreeItem functionName = new TreeItem(moduleName, 0);
                    functionName.setText(f.getNameWithArity());
                    final List<IErlFunctionClause> clauses = filterClauses(f
                            .getChildren());
                    functionName.setData(f);
                    for (final IErlFunctionClause c : clauses) {
                        final TreeItem clauseName = new TreeItem(functionName,
                                0);
                        clauseName.setText(String.valueOf(c.getName()));
                        clauseName.setData(c);
                    }
                }
            }

            // listen to treeitem selection
            functionClausesTree.addSelectionListener(new SelectionListener() {

                @Override
                public void widgetDefaultSelected(final SelectionEvent e) {
                }

                // if a function or a function clause is selected, then
                // highlight it
                // and store the selection
                @Override
                public void widgetSelected(final SelectionEvent e) {

                    final TreeItem[] selectedItems = functionClausesTree
                            .getSelection();

                    if (selectedItems.length > 0) {
                        final TreeItem treeItem = selectedItems[0];
                        final Object data = treeItem.getData();
                        if (data instanceof IErlFunctionClause) {
                            // enable the ok button
                            okButton.setEnabled(true);

                            // highlight
                            WranglerUtils
                                    .highlightSelection((IErlFunctionClause) data);

                            // store
                            functionClause = (IErlFunctionClause) data;
                        } else {
                            okButton.setEnabled(false);
                        }
                    }

                }

            });
        } catch (final ErlModelException e) {
            e.printStackTrace();
        }

        applyDialogFont(composite);
        return composite;
    }

    protected List<IErlFunctionClause> filterClauses(
            final Collection<IErlElement> children) {
        final ArrayList<IErlFunctionClause> clauses = new ArrayList<IErlFunctionClause>();
        for (final IErlElement e : children) {
            if (e instanceof IErlFunctionClause) {
                clauses.add((IErlFunctionClause) e);
            }
        }
        return clauses;
    }

    protected List<IErlFunction> filterFunctions(
            final Collection<IErlElement> elements) {
        final ArrayList<IErlFunction> functions = new ArrayList<IErlFunction>();
        for (final IErlElement e : elements) {
            if (e instanceof IErlFunction) {
                functions.add((IErlFunction) e);
            }
        }

        return functions;
    }

    @Override
    protected void validateInput() {
    }

    /**
     * Returns the selected functionClause if there is.
     * 
     * @return selected function clause
     */
    public IErlFunctionClause getFunctionClause() {
        return functionClause;
    }

}
