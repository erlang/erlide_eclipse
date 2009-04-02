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

import java.util.List;

import org.eclipse.core.resources.IResource;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IExportWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.ide.IDE;

public class EdocExportWizard extends Wizard implements IExportWizard {

	private EdocExportWizardPage page;
	private IWorkbench workbench;
	private IStructuredSelection selection;

	public EdocExportWizard() {
		// TODO Auto-generated constructor stub
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean performFinish() {
		if (!validateFinish()) {
			return false;
		}

		final List<IResource> selectedResources = page.getSelectedResources();
		for (IResource res : selectedResources) {

		}

		return true;
	}

	public void init(final IWorkbench workbench,
			final IStructuredSelection selection) {
		this.workbench = workbench;
		this.selection = selection;

		final List<?> selectedResources = IDE
				.computeSelectedResources(selection);
		if (!selectedResources.isEmpty()) {
			this.selection = new StructuredSelection(selectedResources);
		}
		setWindowTitle("eDoc Export Wizard"); // NON-NLS-1
		setNeedsProgressMonitor(true);

	}

	@Override
	public void addPages() {
		super.addPages();
		page = new EdocExportWizardPage("edoc", selection);
		addPage(page);
	}

	private boolean validateFinish() {
		return true;
	}
}
