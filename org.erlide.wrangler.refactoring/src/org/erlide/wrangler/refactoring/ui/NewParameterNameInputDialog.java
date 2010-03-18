package org.erlide.wrangler.refactoring.ui;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.erlide.wrangler.refactoring.ui.validator.IValidator;
import org.erlide.wrangler.refactoring.ui.validator.VariableNameValidator;

public class NewParameterNameInputDialog extends AbstractInputDialog {

	private Text newParameterName;
	private String data;

	public NewParameterNameInputDialog(Shell parentShell, String title) {
		super(parentShell, title);
	}

	public String getData() {
		return data;
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		Composite composite = (Composite) super.createDialogArea(parent);

		Label newParameterNameLabel = new Label(composite, SWT.WRAP);
		newParameterNameLabel.setText("New parameter name:");
		GridData minToksData = new GridData(GridData.GRAB_HORIZONTAL
				| GridData.GRAB_VERTICAL | GridData.HORIZONTAL_ALIGN_FILL
				| GridData.VERTICAL_ALIGN_CENTER);
		minToksData.widthHint = convertHorizontalDLUsToPixels(IDialogConstants.MINIMUM_MESSAGE_AREA_WIDTH);
		newParameterNameLabel.setLayoutData(minToksData);
		newParameterNameLabel.setFont(parent.getFont());

		newParameterName = new Text(composite, getInputTextStyle());
		newParameterName.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
				| GridData.HORIZONTAL_ALIGN_FILL));
		newParameterName.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				data = newParameterName.getText();
				validateInput();
			}
		});

		errorMessageText = new Text(composite, SWT.READ_ONLY | SWT.WRAP);
		errorMessageText.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
				| GridData.HORIZONTAL_ALIGN_FILL));
		errorMessageText.setBackground(errorMessageText.getDisplay()
				.getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));

		setErrorMessage("New parameter name must be a valid variable name!");
		newParameterName.setText("");

		return composite;

	}

	@Override
	protected void validateInput() {
		IValidator v = new VariableNameValidator();
		if (!v.isValid(newParameterName.getText()))
			setErrorMessage("New parameter name must be a valid variable name!");
		else
			setErrorMessage(null);

	}

}
