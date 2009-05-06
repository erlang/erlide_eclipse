package org.erlide.wrangler.refactoring.core.renameprocess;

import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.erlide.wrangler.refactoring.ui.WranglerNewDataInputPage;
import org.erlide.wrangler.refactoring.util.NameChecker;

public class ProcessNameInputPage extends WranglerNewDataInputPage {

	public ProcessNameInputPage(String name) {
		super(name);
	}

	@Override
	protected String initDescription() {
		return "Rename the selected process";
	}

	@Override
	protected String initLabelText() {
		return "New process name:";
	}

	@Override
	protected void initListeners() {
		newDataText.addModifyListener(new ModifyListener() {

			@Override
			public void modifyText(ModifyEvent e) {
				String s = newDataText.getText();
				if (s.length() == 0) {
					setPageComplete(false);
					setErrorMessage(null);
				} else if (!NameChecker.checkIsProcess(s)) {
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
	protected String initTitle() {
		return "Rename process";
	}

}
