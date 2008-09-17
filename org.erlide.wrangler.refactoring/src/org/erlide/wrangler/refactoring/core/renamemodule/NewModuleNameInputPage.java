package org.erlide.wrangler.refactoring.core.renamemodule;

import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridLayout;
import org.erlide.wrangler.refactoring.ui.WranglerNewDataInputPage;
import org.erlide.wrangler.refactoring.util.NameChecker;

public class NewModuleNameInputPage extends WranglerNewDataInputPage {

	public NewModuleNameInputPage(String name) {
		super(name);
	}

	@Override
	protected String initDescription() {
		return "Rename the selected module";
	}

	@Override
	protected void initExtraControls(GridLayout layout) {
	}

	@Override
	protected String initLabelText() {
		return "New module name:";
	}

	@Override
	protected void initListeners() {
		newDataText.addModifyListener(new ModifyListener() {

			
			public void modifyText(ModifyEvent e) {
				String s = newDataText.getText();
				if (s.length() == 0) {
					setPageComplete(false);
					setErrorMessage(null);
				} else if (!NameChecker.checkIsAtom(s)) {
					setPageComplete(false);
					setErrorMessage("Module name must be an atom!");
				} else {
					setErrorMessage(null);
					setPageComplete(true);
				}
			}

		});
	}

	@Override
	protected String initTitle() {
		return "Rename module";
	}
	
}
