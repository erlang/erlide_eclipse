package org.erlide.wrangler.refactoring.ui.wizardpages;

import org.eclipse.swt.widgets.Composite;

public abstract class MultiInputPage extends InputPage {

	public MultiInputPage(String name) {
		super(name);
	}

	public abstract void createControl(Composite parent);
}
