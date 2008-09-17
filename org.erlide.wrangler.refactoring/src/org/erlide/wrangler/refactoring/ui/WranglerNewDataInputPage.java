package org.erlide.wrangler.refactoring.ui;

import org.eclipse.ltk.ui.refactoring.UserInputWizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.erlide.wrangler.refactoring.core.WranglerRefactoring;

/**
 * Abstract UI class for implementing input pages with at least one input field.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public abstract class WranglerNewDataInputPage extends UserInputWizardPage {

	protected Label renameLabel;
	protected Text newDataText;
	protected Composite composite;

	protected String refactoringName;

	/**
	 * Sole constructor. Saves the given name.
	 * 
	 * @param name
	 *            the refactoring's name
	 */
	public WranglerNewDataInputPage(String name) {
		super(name);

		refactoringName = name;
	}

	public void createControl(Composite parent) {

		setDescription(initDescription());
		setTitle(initTitle());

		composite = new Composite(parent, SWT.NONE);

		renameLabel = new Label(composite, SWT.LEFT);
		renameLabel.setText(initLabelText());
		GridData gridData = new GridData();
		gridData.horizontalAlignment = GridData.FILL;
		gridData.horizontalSpan = 2;
		renameLabel.setLayoutData(gridData);

		newDataText = new Text(composite, SWT.NONE);
		gridData = new GridData();
		gridData.horizontalAlignment = GridData.FILL;
		gridData.horizontalSpan = 2;
		gridData.grabExcessHorizontalSpace = true;
		newDataText.setLayoutData(gridData);

		setPageComplete(false);

		GridLayout layout = new GridLayout();
		layout.numColumns = 2;
		initExtraControls(layout);
		composite.setLayout(layout);

		initNewNameModifyListener();
		initListeners();

		setControl(composite);

	}

	/**
	 * Returns with the refactoring's description.
	 * 
	 * @return the refactoring's short description
	 */
	protected abstract String initDescription();

	/**
	 * Initializes extra controls for this widget. If more than one input filed
	 * is needed, should be overriden.
	 * 
	 * @param layout
	 */
	protected void initExtraControls(GridLayout layout) {
	}

	/**
	 * Returns the string of the Label placed in this widget.
	 * 
	 * @return placed Label's text
	 */
	protected abstract String initLabelText();

	/**
	 * Initializes listeners for the controls in this widget. This listeners
	 * checks the syntax of the typed data.
	 */
	protected abstract void initListeners();

	/**
	 * Initializes a listener, which stores the typed data.
	 */
	private void initNewNameModifyListener() {
		newDataText.addModifyListener(new ModifyListener() {

			public void modifyText(ModifyEvent e) {
				WranglerRefactoring refac = (WranglerRefactoring) getRefactoring();
				refac.setNewName(newDataText.getText());
			}

		});
	}

	/**
	 * Returns with this widget title.
	 * 
	 * @return the title of this widget
	 */
	protected abstract String initTitle();

}
