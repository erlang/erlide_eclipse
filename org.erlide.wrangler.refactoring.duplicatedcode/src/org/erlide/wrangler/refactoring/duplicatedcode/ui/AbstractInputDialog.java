package org.erlide.wrangler.refactoring.duplicatedcode.ui;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.resource.StringConverter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

public abstract class AbstractInputDialog extends Dialog {

	protected Text errorMessageText;
	protected final String title;
	boolean isFinished = false;

	public AbstractInputDialog(Shell parentShell, String title) {
		super(parentShell);
		this.title = title;
	}

	abstract protected void validateInput();

	@Override
	abstract protected void createButtonsForButtonBar(Composite parent);

	/*
	 * Should be implemented as well.
	 * 
	 * protected Control createDialogArea(Composite parent);
	 */

	public boolean isFinished() {
		return isFinished;
	}

	public void setErrorMessage(final String errorMessage) {
		if (errorMessageText != null && !errorMessageText.isDisposed()) {
			errorMessageText.setText(errorMessage == null ? " \n "
					: errorMessage);

			boolean hasError = errorMessage != null
					&& (StringConverter.removeWhiteSpaces(errorMessage))
							.length() > 0;
			errorMessageText.setEnabled(hasError);
			errorMessageText.setVisible(hasError);
			errorMessageText.getParent().update();

			Control button = getButton(IDialogConstants.OK_ID);
			if (button != null) {
				button.setEnabled(errorMessage == null);
			}
		}
	}

	@Override
	protected void configureShell(Shell shell) {
		super.configureShell(shell);
		if (title != null) {
			shell.setText(title);
		}
	}

	protected int getInputTextStyle() {
		return SWT.SINGLE | SWT.BORDER;
	}

}
