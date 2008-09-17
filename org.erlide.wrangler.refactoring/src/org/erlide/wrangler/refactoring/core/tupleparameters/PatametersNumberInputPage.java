package org.erlide.wrangler.refactoring.core.tupleparameters;

import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.erlide.wrangler.refactoring.ui.WranglerNewDataInputPage;

public class PatametersNumberInputPage extends WranglerNewDataInputPage {

	public PatametersNumberInputPage(String name) {
		super(name);
	}

	@Override
	protected String initDescription() {
		return "Tuple function parameters";
	}

	@Override
	protected String initLabelText() {
		return "How many parameters do you want to tuple?";
	}

	@Override
	protected void initListeners() {
		newDataText.addModifyListener(new ModifyListener() {

			
			public void modifyText(ModifyEvent e) {
				String s = newDataText.getText();
				if (s.length() == 0) {
					setPageComplete(false);
					setErrorMessage(null);
				} else {
					int num;
					try {
						num = Integer.valueOf(s);
						setPageComplete(true);
						setErrorMessage(null);
					} catch (NumberFormatException e1) {
						setPageComplete(false);
						setErrorMessage("Parameters number must be an integer!");
					}
				}
			}

		});

	}

	@Override
	protected String initTitle() {
		return "Tuple function parameters";
	}

}
