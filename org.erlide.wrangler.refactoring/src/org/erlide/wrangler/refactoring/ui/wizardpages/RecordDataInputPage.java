package org.erlide.wrangler.refactoring.ui.wizardpages;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.erlide.wrangler.refactoring.core.QuickCheckStateRefactoring;
import org.erlide.wrangler.refactoring.ui.validator.AtomValidator;
import org.erlide.wrangler.refactoring.ui.validator.IValidator;

/**
 * Wizard pages, on which the user can input the necessary data for a record
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class RecordDataInputPage extends MultiInputPage {

	IValidator validator;

	protected ArrayList<Text> fieldNames;
	protected ArrayList<Label> fieldNameLabels;

	protected Label recordNameLabel;
	protected Text recordName;

	protected Composite composite;

	protected QuickCheckStateRefactoring refactoring;

	/**
	 * Constructor
	 * 
	 * @param name
	 *            title
	 */
	public RecordDataInputPage(String name) {
		super(name);
	}

	@Override
	public void createControl(Composite parent) {
		refactoring = (QuickCheckStateRefactoring) getRefactoring();
		composite = new Composite(parent, SWT.NONE);

		recordNameLabel = new Label(composite, SWT.LEFT);
		recordNameLabel.setText("Record name:");
		GridData gridData = new GridData();
		gridData.horizontalAlignment = GridData.FILL;
		gridData.horizontalSpan = 2;
		recordNameLabel.setLayoutData(gridData);

		recordName = new Text(composite, SWT.NONE);
		gridData = new GridData();
		gridData.horizontalAlignment = GridData.FILL;
		gridData.horizontalSpan = 2;
		gridData.grabExcessHorizontalSpace = true;
		recordName.setLayoutData(gridData);

		ModifyListener modifyListener = new ModifyListener() {

			public void modifyText(ModifyEvent e) {
				isInputValid();
			}

		};

		recordName.addModifyListener(modifyListener);

		// adding field name inputs
		int n = refactoring.getRecordFieldCount();
		fieldNameLabels = new ArrayList<Label>();
		fieldNames = new ArrayList<Text>();
		for (int i = 0; i < n; ++i) {
			Label l = new Label(composite, SWT.LEFT);
			l.setText("Field name (" + i + "):");
			GridData gd = new GridData();
			gd.horizontalAlignment = GridData.FILL;
			gd.horizontalSpan = 2;
			l.setLayoutData(gridData);

			fieldNameLabels.add(l);

			Text t = new Text(composite, SWT.NONE);
			gd = new GridData();
			gd.horizontalAlignment = GridData.FILL;
			gd.horizontalSpan = 2;
			gd.grabExcessHorizontalSpace = true;
			t.setLayoutData(gridData);

			fieldNames.add(t);

			t.addModifyListener(modifyListener);
		}

		GridLayout layout = new GridLayout();
		layout.numColumns = 2;
		composite.setLayout(layout);

		setControl(composite);
	}

	@Override
	protected boolean isInputValid() {
		IValidator validator = new AtomValidator();
		boolean valid = validator.isValid(recordName.getText());

		ArrayList<String> fn = new ArrayList<String>();
		for (Text t : fieldNames) {
			valid = valid && validator.isValid(t.getText());
			fn.add(t.getText());
			if (!valid)
				break;
		}

		if (valid) {
			refactoring.setRecordData(recordName.getText(), fn);
			setErrorMessage(null);
			setPageComplete(true);
		} else {
			setPageComplete(false);
			setErrorMessage("Please provide valid record name, and field names!");
		}

		return valid;
	}
}
