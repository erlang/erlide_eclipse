package org.erlide.wrangler.refactoring.ui.wizardpages;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.erlide.wrangler.refactoring.core.SimpleWranglerRefactoring;
import org.erlide.wrangler.refactoring.ui.validator.IValidator;

public class SimpleInputPage extends InputPage {
	protected String labelText;

	protected String inputErrorMsg;

	IValidator validator;

	protected Label inputLabel;

	protected Text inputText;

	protected Composite composite;

	/**
	 * Constructor
	 * 
	 * @param name
	 *            Refactoring name (title)
	 * @param description
	 *            description
	 * @param labelText
	 *            input label's text
	 * @param inputErrorMsg
	 *            error message in case of wrong input
	 * @param validator
	 *            validator object
	 */
	public SimpleInputPage(String name, String description, String labelText,
			String inputErrorMsg, IValidator validator) {
		super(name);
		this.setDescription(description);
		this.inputErrorMsg = inputErrorMsg;
		this.labelText = labelText;
		this.validator = validator;
		setPageComplete(false);

	}

	public void createControl(Composite parent) {
		composite = new Composite(parent, SWT.NONE);

		inputLabel = new Label(composite, SWT.LEFT);
		inputLabel.setText(labelText);
		GridData gridData = new GridData();
		gridData.horizontalAlignment = GridData.FILL;
		gridData.horizontalSpan = 2;
		inputLabel.setLayoutData(gridData);

		inputText = new Text(composite, SWT.NONE);
		gridData = new GridData();
		gridData.horizontalAlignment = GridData.FILL;
		gridData.horizontalSpan = 2;
		gridData.grabExcessHorizontalSpace = true;
		inputText.setLayoutData(gridData);

		GridLayout layout = new GridLayout();
		layout.numColumns = 2;
		composite.setLayout(layout);

		setControl(composite);

		inputText.addModifyListener(new ModifyListener() {

			public void modifyText(ModifyEvent e) {
				isInputValid();
			}

		});

	}

	protected boolean isInputValid() {
		if (validator.isValid(inputText.getText())) {
			((SimpleWranglerRefactoring) getRefactoring())
					.setUserInput(inputText.getText());
			setErrorMessage(null);
			setPageComplete(true);
			return true;
		} else {
			setPageComplete(false);
			setErrorMessage(inputErrorMsg);
			return false;
		}
	}
}
