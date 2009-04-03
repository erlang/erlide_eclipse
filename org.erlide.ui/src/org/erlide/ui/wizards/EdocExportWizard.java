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
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IExportWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.ide.IDE;
import org.erlide.core.ErlangProjectProperties;
import org.erlide.runtime.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

import erlang.ErlideEdoc;

public class EdocExportWizard extends Wizard implements IExportWizard {

	private EdocExportWizardPage page;
	private IWorkbench workbench;
	private IStructuredSelection selection;

	public EdocExportWizard() {
		// TODO Auto-generated constructor stub
	}

	@Override
	public boolean performFinish() {
		if (!validateFinish()) {
			return false;
		}

		final Collection<IProject> projects = page.getSelectedResources();
		final Map<String, OtpErlangObject> options = page.getOptions();
		for (IProject prj : projects) {
			System.out.println("EDOC: " + prj.getName());
			try {
				IFolder dest = prj.getFolder(page.getDestination());
				if (!dest.exists()) {
					dest.create(true, true, null);
				}
				options.put("dir", new OtpErlangString(dest.getLocation()
						.toString()));
				System.out.println("out: " + dest.getLocation().toString());
				final List<String> files = new ArrayList<String>();
				ErlangProjectProperties props = new ErlangProjectProperties(prj);
				for (String dir : props.getSourceDirs()) {
					IFolder folder = prj.getFolder(dir);
					folder.accept(new IResourceVisitor() {
						public boolean visit(IResource resource)
								throws CoreException {
							if ("erl".equals(resource.getFileExtension())) {
								files.add(resource.getLocation().toString());
							}
							return true;
						}
					});
				}
				try {
					ErlideEdoc.files(files, options);
				} catch (Exception e) {
					ErlLogger.warn(e.getMessage());
				}
			} catch (CoreException e) {
				ErlLogger.warn(e.getMessage());
			}
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
