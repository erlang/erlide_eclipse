/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.editors.scratch;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.erlide.core.erlang.IErlElement;

/**
 * Creates a new scratch page
 */
public class NewScratchFileCreationWizard extends Wizard implements INewWizard {

	private NewScratchFileWizardPage fPage;

	private IStructuredSelection fSelection;

	public NewScratchFileCreationWizard() {
		setNeedsProgressMonitor(true);
		setWindowTitle(ScratchMessages
				.getString("NewScratchFileCreationWizard.title")); //$NON-NLS-1$
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.wizard.IWizard#addPages()
	 */
	@Override
	public void addPages() {
		super.addPages();
		if (fSelection == null) {
			final IErlElement elem = getActiveEditorErlangInput();
			if (elem != null) {
				fSelection = new StructuredSelection(elem);
			} else {
				fSelection = StructuredSelection.EMPTY;
			}
		}
		fPage = new NewScratchFileWizardPage(fSelection);
		addPage(fPage);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.wizard.IWizard#performFinish()
	 */
	@Override
	public boolean performFinish() {
		return fPage.finish();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
	 * org.eclipse.jface.viewers.IStructuredSelection)
	 */
	public void init(IWorkbench workbench, IStructuredSelection selection) {
		fSelection = selection;
		//setDefaultPageImageDescriptor(JavaDebugImages.DESC_WIZBAN_NEWSCRAPPAGE
		// );
	}

	/**
	 * If the current active editor edits a Java element return it, else return
	 * null
	 */
	@SuppressWarnings("null")
	private IErlElement getActiveEditorErlangInput() {
		final IWorkbenchPage page = null; // JDIDebugUIPlugin.getActivePage();
		if (page != null) {
			final IEditorPart part = page.getActiveEditor();
			if (part != null) {
				final IEditorInput editorInput = part.getEditorInput();
				if (editorInput != null) {
					return (IErlElement) editorInput
							.getAdapter(IErlElement.class);
				}
			}
		}
		return null;
	}
}