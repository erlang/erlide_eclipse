/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.editors.scratch;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.part.ISetSelectionTarget;
import org.erlide.core.ErlangPlugin;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.internal.ExceptionHandler;

/**
 * Page to create a new Erlang scratch file.
 */
public class NewScratchFileWizardPage extends WizardNewFileCreationPage {

	private static final String fgDefaultExtension = ".epage"; //$NON-NLS-1$

	public NewScratchFileWizardPage(IStructuredSelection selection) {
		super("createScrapBookPage", selection); //$NON-NLS-1$
		setTitle(ScratchMessages.getString("NewScratchFileWizardPage.title")); //$NON-NLS-1$
	}

	public boolean finish() {
		// add extension if non is provided
		final String fileName = getFileName();
		if (fileName != null && !fileName.endsWith(fgDefaultExtension)) {
			setFileName(fileName + fgDefaultExtension);
		}

		final boolean retValue = super.validatePage();

		final IFile file = createNewFile();
		if (retValue && file != null) {
			final Shell shell = getShell();
			final IWorkbenchPage page = ErlideUIPlugin.getActivePage();
			if (shell == null || page == null) {
				return true;
			}
			final IWorkbenchPart focusPart = page.getActivePart();
			if (focusPart instanceof ISetSelectionTarget) {
				shell.getDisplay().asyncExec(new Runnable() {

					public void run() {
						final ISelection selection = new StructuredSelection(
								file);
						((ISetSelectionTarget) focusPart)
								.selectReveal(selection);
					}
				});
			}
			try {
				IDE.openEditor(page, file, true);
				return true;
			} catch (final PartInitException e) {
				ExceptionHandler
						.handle(
								e,
								shell,
								ScratchMessages
										.getString("NewScratchFileWizardPage.open_error.message"), e.getMessage()); //$NON-NLS-1$
			}
		}
		return false;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.dialogs.WizardNewFileCreationPage#validatePage()
	 */
	@Override
	protected boolean validatePage() {
		// check whether file with extension doesn't exist
		boolean valid = super.validatePage();
		if (!valid) {
			return false;
		}

		final IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace()
				.getRoot();
		final IPath containerPath = getContainerFullPath();
		if (containerPath != null && containerPath.segmentCount() > 0) {
			final IProject project = workspaceRoot.getProject(containerPath
					.segment(0));
			try {
				if (!project.hasNature(ErlangPlugin.NATURE_ID)) {
					setErrorMessage(ScratchMessages
							.getString("NewScratchFileWizardPage.error.OnlyInErlangProject")); //$NON-NLS-1$
					return false;
				}
			} catch (final CoreException e) {
				// JDIDebugUIPlugin.log(e.getStatus());
			}
		}

		String fileName = getFileName();
		if (fileName != null && !fileName.endsWith(fgDefaultExtension)) {
			fileName = fileName + fgDefaultExtension;
			final IPath path = getContainerFullPath();

			if (path != null && workspaceRoot.exists(path.append(fileName))) {
				setErrorMessage(ScratchMessages
						.getString("NewScratchFileWizardPage.error.AlreadyExists")); //$NON-NLS-1$
				return false;
			}
		}
		return true;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	public void createControl(Composite parent) {
		super.createControl(parent);
		// WorkbenchHelp.setHelp(getControl(),
		// IJavaDebugHelpContextIds.NEW_SNIPPET_WIZARD_PAGE);
	}
}