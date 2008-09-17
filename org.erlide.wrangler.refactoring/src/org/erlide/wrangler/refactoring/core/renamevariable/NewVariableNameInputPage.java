package org.erlide.wrangler.refactoring.core.renamevariable;

import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.erlide.wrangler.refactoring.ui.WranglerNewDataInputPage;
import org.erlide.wrangler.refactoring.util.NameChecker;

public class NewVariableNameInputPage extends WranglerNewDataInputPage {

	public NewVariableNameInputPage(String name) {
		super(name);
	}

	@Override
	protected void initListeners() {
		newDataText.addModifyListener(new ModifyListener() {

			
			public void modifyText(ModifyEvent e) {
				String s = newDataText.getText();
				if (s.length() == 0) {
					setPageComplete(false);
					setErrorMessage(null);
				} else if (!NameChecker.checkIsVariable(s)) {
					setPageComplete(false);
					setErrorMessage("Please enter a valid variable name!");
				} else {
					setErrorMessage(null);
					setPageComplete(true);
				}
			}

		});
	}

	@Override
	protected String initDescription() {
		return "Rename the selected variable";
	}

	@Override
	protected String initTitle() {
		return "Rename variable";
	}

	@Override
	protected String initLabelText() {
		return "New variable name:";
	}

}
