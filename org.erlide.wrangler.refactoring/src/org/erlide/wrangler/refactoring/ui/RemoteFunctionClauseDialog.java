package org.erlide.wrangler.refactoring.ui;

import java.util.ArrayList;
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
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlFunctionClause;
import org.erlide.core.erlang.IErlModule;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;
import org.erlide.wrangler.refactoring.util.WranglerUtils;

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

	public RemoteFunctionClauseDialog(final Shell parentShell,
			final String title) {
		super(parentShell, title);
	}

	private IErlFunctionClause functionClause = null;

	@Override
	protected Control createDialogArea(Composite parent) {

		Composite composite = (Composite) super.createDialogArea(parent);
		final Tree functionClausesTree;

		Label label = new Label(composite, SWT.WRAP);
		label
				.setText("Please select the function clause which against should fold!");
		GridData minToksData = new GridData(GridData.GRAB_HORIZONTAL
				| GridData.GRAB_VERTICAL | GridData.HORIZONTAL_ALIGN_FILL
				| GridData.VERTICAL_ALIGN_CENTER);
		minToksData.widthHint = convertHorizontalDLUsToPixels(IDialogConstants.MINIMUM_MESSAGE_AREA_WIDTH);
		label.setLayoutData(minToksData);
		label.setFont(parent.getFont());

		functionClausesTree = new Tree(composite, SWT.BORDER);
		GridData treeData = new GridData(GridData.GRAB_HORIZONTAL
				| GridData.GRAB_VERTICAL | GridData.HORIZONTAL_ALIGN_FILL
				| GridData.VERTICAL_ALIGN_CENTER);
		treeData.widthHint = convertHorizontalDLUsToPixels(IDialogConstants.MINIMUM_MESSAGE_AREA_WIDTH);
		functionClausesTree.setLayoutData(treeData);

		List<IErlModule> erlmodules;
		try {
			erlmodules = GlobalParameters.getWranglerSelection()
					.getErlElement().getErlProject().getModules();

			for (IErlModule m : erlmodules) {
				// must refresh the scanner!
				if (/* !m.isStructureKnown() */true) {
					// FIXME: not permitted operation
					m.open(null);
				}

				TreeItem moduleName = new TreeItem(functionClausesTree, 0);
				moduleName.setText(m.getModuleName());
				moduleName.setData(m);
				List<IErlFunction> functions = filterFunctions(m.getChildren());
				for (IErlFunction f : functions) {
					TreeItem functionName = new TreeItem(moduleName, 0);
					functionName.setText(f.getNameWithArity());
					List<IErlFunctionClause> clauses = filterClauses(f
							.getChildren());
					functionName.setData(f);
					int i = 0;
					for (IErlFunctionClause c : clauses) {
						TreeItem clauseName = new TreeItem(functionName, 0);
						clauseName.setText(String.valueOf(c.getName()));
						clauseName.setData(c);
					}
				}
			}

			// listen to treeitem selection
			functionClausesTree.addSelectionListener(new SelectionListener() {

				public void widgetDefaultSelected(SelectionEvent e) {
				}

				// if a function or a function clause is selected, then
				// highlight it
				// and store the selection
				public void widgetSelected(SelectionEvent e) {

					TreeItem[] selectedItems = functionClausesTree
							.getSelection();

					if (selectedItems.length > 0) {
						TreeItem treeItem = selectedItems[0];
						Object data = treeItem.getData();
						if (data instanceof IErlFunctionClause) {
							// enable the ok button
							okButton.setEnabled(true);

							// highlight
							WranglerUtils
									.highlightSelection(((IErlFunctionClause) data));

							// store
							functionClause = (IErlFunctionClause) data;
						} else {
							okButton.setEnabled(false);
						}
					}

				}

			});
		} catch (ErlModelException e) {
			e.printStackTrace();
		}

		applyDialogFont(composite);
		return composite;
	}

	protected List<IErlFunctionClause> filterClauses(List<IErlElement> children) {
		ArrayList<IErlFunctionClause> clauses = new ArrayList<IErlFunctionClause>();
		for (IErlElement e : children) {
			if (e instanceof IErlFunctionClause)
				clauses.add((IErlFunctionClause) e);
		}
		return clauses;
	}

	protected List<IErlFunction> filterFunctions(List<IErlElement> elements) {
		ArrayList<IErlFunction> functions = new ArrayList<IErlFunction>();
		for (IErlElement e : elements) {
			if (e instanceof IErlFunction)
				functions.add((IErlFunction) e);
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
