/*******************************************************************************
 * Copyright (c) 2010 György Orosz.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     György Orosz - initial API and implementation
 ******************************************************************************/
package org.erlide.wrangler.refactoring.ui.wizard;

import java.util.ArrayList;

import org.eclipse.ltk.core.refactoring.Refactoring;
import org.erlide.wrangler.refactoring.ui.wizardpages.WranglerPage;

public class DefaultWranglerRefactoringWizard extends
		AbstractWranglerRefactoringWizard {

	private ArrayList<WranglerPage> pages;

	public DefaultWranglerRefactoringWizard(Refactoring refactoring, int flags,
			ArrayList<WranglerPage> pages) {
		super(refactoring, flags);
		this.pages = pages;
		setWindowTitle(refactoring.getName());
		setDefaultPageTitle(refactoring.getName());
	}

	@Override
	protected void addUserInputPages() {
		for (WranglerPage page : pages) {
			addPage(page);
		}
	}
}
