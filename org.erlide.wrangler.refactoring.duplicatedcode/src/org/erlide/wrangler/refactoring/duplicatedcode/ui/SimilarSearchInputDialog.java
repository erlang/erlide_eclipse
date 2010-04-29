package org.erlide.wrangler.refactoring.duplicatedcode.ui;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.erlide.wrangler.refactoring.ui.AbstractInputDialog;

public class SimilarSearchInputDialog extends AbstractInputDialog {

	private Button onlyInFileCheckBoxButton;
	private float simScore;
	private boolean workOnlyInCurrentFile;
	private Text simScoreText;

	public SimilarSearchInputDialog(Shell parentShell, String title) {
		super(parentShell, title);
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		Composite composite = (Composite) super.createDialogArea(parent);

		Label simScoreLabel = new Label(composite, SWT.WRAP);
		simScoreLabel.setText("Similarity score (between 0.1 and 1.0):");
		GridData simScoreData = new GridData(GridData.GRAB_HORIZONTAL
				| GridData.GRAB_VERTICAL | GridData.HORIZONTAL_ALIGN_FILL
				| GridData.VERTICAL_ALIGN_CENTER);
		simScoreData.widthHint = convertHorizontalDLUsToPixels(IDialogConstants.MINIMUM_MESSAGE_AREA_WIDTH);
		simScoreLabel.setLayoutData(simScoreData);
		simScoreLabel.setFont(parent.getFont());

		simScoreText = new Text(composite, getInputTextStyle());
		simScoreText.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
				| GridData.HORIZONTAL_ALIGN_FILL));
		simScoreText.setText("0.8");
		simScoreText.addModifyListener(new ModifyListener() {

			public void modifyText(ModifyEvent e) {
				validateInput();
			}

		});

		onlyInFileCheckBoxButton = new Button(composite, SWT.CHECK);
		onlyInFileCheckBoxButton
				.setText("Detect similar code snippets in the project");

		onlyInFileCheckBoxButton.setLayoutData(new GridData(
				GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL
						| GridData.HORIZONTAL_ALIGN_FILL
						| GridData.VERTICAL_ALIGN_CENTER));
		onlyInFileCheckBoxButton.addSelectionListener(new SelectionListener() {

			public void widgetSelected(SelectionEvent e) {
				validateInput();

			}

			public void widgetDefaultSelected(SelectionEvent e) {
				validateInput();

			}

		});

		errorMessageText = new Text(composite, SWT.READ_ONLY | SWT.WRAP);
		errorMessageText.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
				| GridData.HORIZONTAL_ALIGN_FILL));
		errorMessageText.setBackground(errorMessageText.getDisplay()
				.getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));

		setErrorMessage("");

		applyDialogFont(composite);
		return composite;
	}

	@Override
	protected void validateInput() {
		String errorMsg = null;
		workOnlyInCurrentFile = !onlyInFileCheckBoxButton.getSelection();
		try {
			simScore = Float.parseFloat(simScoreText.getText());
			setErrorMessage(null);
		} catch (Exception e) {
			errorMsg = "Please type correct values!";
			setErrorMessage(errorMsg);
		}

	}

	public double getSimScore() {
		return simScore;
	}

	public boolean onlyinFile() {
		return workOnlyInCurrentFile;
	}

}
