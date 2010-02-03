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
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

public class SimilarCodeInputDialog extends AbstractInputDialog {

	private Button okButton;

	private Button onlyInFileCheckBoxButton;
	private Text minToksText;
	private Text minFreqText;

	private int minToks;
	private int minFreq;
	private float simScore;
	private boolean workOnlyInCurrentFile;

	private Text simScoreText;

	public SimilarCodeInputDialog(Shell parentShell, String title) {
		super(parentShell, title);
	}

	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		okButton = createButton(parent, IDialogConstants.OK_ID,
				IDialogConstants.OK_LABEL, true);
		createButton(parent, IDialogConstants.CANCEL_ID,
				IDialogConstants.CANCEL_LABEL, false);

		okButton.addSelectionListener(new SelectionListener() {

			public void widgetDefaultSelected(SelectionEvent e) {
			}

			public void widgetSelected(SelectionEvent e) {
				isFinished = true;
			}

		});
	}

	@Override
	protected void validateInput() {
		String errorMsg = null;
		workOnlyInCurrentFile = !onlyInFileCheckBoxButton.getSelection();
		try {
			simScore = Float.parseFloat(simScoreText.getText());
			minToks = Integer.parseInt(minToksText.getText());
			minFreq = Integer.parseInt(minFreqText.getText());
			setErrorMessage(null);
		} catch (Exception e) {
			errorMsg = "Please type correct values!";
			setErrorMessage(errorMsg);
		}

	}

	@Override
	protected Control createDialogArea(Composite parent) {
		Composite composite = (Composite) super.createDialogArea(parent);

		Label minTokslabel = new Label(composite, SWT.WRAP);
		minTokslabel.setText("Minimum number of tokens:");
		GridData minToksData = new GridData(GridData.GRAB_HORIZONTAL
				| GridData.GRAB_VERTICAL | GridData.HORIZONTAL_ALIGN_FILL
				| GridData.VERTICAL_ALIGN_CENTER);
		minToksData.widthHint = convertHorizontalDLUsToPixels(IDialogConstants.MINIMUM_MESSAGE_AREA_WIDTH);
		minTokslabel.setLayoutData(minToksData);
		minTokslabel.setFont(parent.getFont());

		minToksText = new Text(composite, getInputTextStyle());
		minToksText.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
				| GridData.HORIZONTAL_ALIGN_FILL));
		minToksText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				validateInput();
			}
		});

		Label minFreqLabel = new Label(composite, SWT.WRAP);
		minFreqLabel.setText("Minimum number of frequency:");
		GridData minFreqData = new GridData(GridData.GRAB_HORIZONTAL
				| GridData.GRAB_VERTICAL | GridData.HORIZONTAL_ALIGN_FILL
				| GridData.VERTICAL_ALIGN_CENTER);
		minFreqData.widthHint = convertHorizontalDLUsToPixels(IDialogConstants.MINIMUM_MESSAGE_AREA_WIDTH);
		minFreqLabel.setLayoutData(minFreqData);
		minFreqLabel.setFont(parent.getFont());

		minFreqText = new Text(composite, getInputTextStyle());
		minFreqText.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
				| GridData.HORIZONTAL_ALIGN_FILL));
		minFreqText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				validateInput();
			}
		});

		Label simScoreLabel = new Label(composite, SWT.WRAP);
		simScoreLabel.setText("Similarity score:");
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

		errorMessageText = new Text(composite, SWT.READ_ONLY | SWT.WRAP);
		errorMessageText.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
				| GridData.HORIZONTAL_ALIGN_FILL));
		errorMessageText.setBackground(errorMessageText.getDisplay()
				.getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));

		setErrorMessage("");

		applyDialogFont(composite);
		return composite;
	}

	protected Spinner createValueSpinner(Composite parent, int min, int max,
			int digits, int value) {
		Spinner ret = new Spinner(parent, SWT.BORDER);
		ret.setMinimum(min);
		ret.setMaximum(max);
		ret.setDigits(digits);
		ret.setSelection(value);

		GridData gd = new GridData();
		gd.verticalIndent = 2;
		gd.horizontalAlignment = SWT.RIGHT;
		gd.grabExcessHorizontalSpace = true;
		ret.setLayoutData(gd);

		return ret;
	}

	public double getSimScore() {
		return simScore;
	}

	public int getMinToks() {
		return minToks;
	}

	public int getMinFreq() {
		return minFreq;
	}

	public boolean onlyinFile() {
		return workOnlyInCurrentFile;
	}
}
