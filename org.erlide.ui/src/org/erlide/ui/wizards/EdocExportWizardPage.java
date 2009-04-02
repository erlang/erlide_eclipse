/*******************************************************************************
 * Copyright (c) 2009 * and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     *
 *******************************************************************************/
package org.erlide.ui.wizards;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IResource;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;

public class EdocExportWizardPage extends WizardPage {

	protected EdocExportWizardPage(String pageName,
			IStructuredSelection selection) {
		super(pageName);
		setTitle("eDoc export (work in progress!)");
		setDescription("Select the projects and files for which you want to generate eDoc documentation");
	}

	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		setControl(composite);
	}

	public List<IResource> getSelectedResources() {
		ArrayList<IResource> result = new ArrayList<IResource>();
		return result;
	}

}
