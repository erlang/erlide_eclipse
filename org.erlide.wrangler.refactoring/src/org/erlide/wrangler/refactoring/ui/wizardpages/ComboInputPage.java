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

public class ComboInputPage extends InputPage {

	protected String labelText;

	protected Composite composite;

	protected Label inputLabel;

	protected ArrayList<String> moduleNames;

	protected Combo selectionList;

	public ComboInputPage(String name, String description, String labelText,
			ArrayList<String> moduleNames) {
		super(name);
		this.setDescription(description);
		this.labelText = labelText;
		this.moduleNames = moduleNames;

		setPageComplete(false);
	}

	public void createControl(Composite parent) {
		composite = new Composite(parent, SWT.NONE);

		inputLabel = new Label(composite, SWT.LEFT);
		inputLabel.setText(labelText);

		// GridData gridData = new GridData();
		// gridData.horizontalAlignment = GridData.FILL;
		// gridData.horizontalSpan = 2;
		// inputLabel.setLayoutData(gridData);

		selectionList = new Combo(composite, SWT.DROP_DOWN);
		for (String s : moduleNames) {
			selectionList.add(s);
		}
		// gridData = new GridData();
		// gridData.horizontalAlignment = GridData.FILL;
		// gridData.horizontalSpan = 2;
		// selectionList.setLayoutData(gridData);

		// GridLayout layout = new GridLayout();
		RowLayout layout = new RowLayout();
		layout.spacing = 5;
		layout.center = true;

		composite.setLayout(layout);

		setControl(composite);

		selectionList.addSelectionListener(new SelectionListener() {

			public void widgetDefaultSelected(SelectionEvent e) {
			}

			@SuppressWarnings("synthetic-access")
			public void widgetSelected(SelectionEvent e) {
				((SimpleOneStepWranglerRefactoring) getRefactoring())
						.setUserInput(selectionList.getText());
				setPageComplete(true);
			}
		});

		final IValidator validator = new AtomValidator();

		selectionList.addModifyListener(new ModifyListener() {

			public void modifyText(ModifyEvent e) {
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
		// TODO Auto-generated method stub
		return false;
	}
}
