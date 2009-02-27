/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.gunit.servers.ui;

import java.lang.reflect.InvocationTargetException;
import java.util.Set;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModule;
import org.erlide.gunit.internal.Messages;
import org.erlide.gunit.internal.ui.GUnitMessages;
import org.erlide.gunit.internal.ui.IGUnitHelpContextIds;

/**
 * A dialog to select a test method.
 */
public class TestMethodSelectionDialog extends ElementListSelectionDialog {

	private IErlElement fElement;

	// public static class TestReferenceCollector extends SearchRequestor {
	// Set fResult = new HashSet(200);
	//
	// public void acceptSearchMatch(SearchMatch match) throws CoreException {
	// IErlElement enclosingElement = (IErlElement) match.getElement();
	// if (enclosingElement.getElementName().startsWith("test")) {
	// fResult.add(enclosingElement);
	// }
	// }
	//
	// public Object[] getResult() {
	// return fResult.toArray();
	// }
	// }

	public TestMethodSelectionDialog(Shell shell, IErlElement element) {
		super(shell, null);// new JavaElementLabelProvider(
		// JavaElementLabelProvider.SHOW_PARAMETERS
		// | JavaElementLabelProvider.SHOW_POST_QUALIFIED));
		this.fElement = element;
	}

	/*
	 * @see Windows#configureShell
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		PlatformUI.getWorkbench().getHelpSystem().setHelp(newShell,
				IGUnitHelpContextIds.TEST_SELECTION_DIALOG);
	}

	/*
	 * @see Window#open()
	 */
	@Override
	public int open() {
		Object[] elements;
		IErlModule testType = findTestType();

		if (testType == null) {
			return CANCEL;
		}

		try {
			elements = searchTestMethods(this.fElement, testType);
		} catch (InterruptedException e) {
			return CANCEL;
		} catch (InvocationTargetException e) {
			MessageDialog.openError(getParentShell(),
					GUnitMessages.TestMethodSelectionDialog_error_title, e
							.getTargetException().getMessage());
			return CANCEL;
		}

		if (elements.length == 0) {
			String msg = Messages.format(
					GUnitMessages.TestMethodSelectionDialog_notfound_message,
					this.fElement.getName());
			MessageDialog
					.openInformation(
							getParentShell(),
							GUnitMessages.TestMethodSelectionDialog_no_tests_title,
							msg);
			return CANCEL;
		}
		setElements(elements);
		return super.open();
	}

	private IErlModule findTestType() {
		// String qualifiedName = GUnitPlugin.TEST_INTERFACE_NAME;
		// IErlProject[] projects;
		// Set result = new HashSet();
		// try {
		// projects = ErlangCore.getModel().create(
		// ResourcesPlugin.getWorkspace().getRoot()).getJavaProjects();
		// for (int i = 0; i < projects.length; i++) {
		// IErlProject project = projects[i];
		// IErlModule type = project.findType(qualifiedName);
		// if (type != null) {
		// result.add(type);
		// }
		// }
		// } catch (JavaModelException e) {
		// ErrorDialog
		// .openError(
		// getParentShell(),
		// JUnitMessages.TestMethodSelectionDialog_error_notfound_title,
		// JUnitMessages.TestMethodSelectionDialog_error_notfound_message,
		// e.getStatus());
		// return null;
		// }
		// if (result.size() == 0) {
		// String msg = Messages.format(
		// JUnitMessages.TestMethodSelectionDialog_test_not_found,
		// GUnitPlugin.TEST_INTERFACE_NAME);
		// MessageDialog
		// .openError(
		// getParentShell(),
		// JUnitMessages.TestMethodSelectionDialog_select_dialog_title,
		// msg);
		// return null;
		// }
		// if (result.size() == 1) {
		// return (IErlModule) result.toArray()[0];
		// }
		//
		// return selectTestType(result);
		return null;
	}

	private IErlModule selectTestType(Set result) {
		// ILabelProvider labelProvider = new JavaElementLabelProvider(
		// JavaElementLabelProvider.SHOW_PARAMETERS
		// | JavaElementLabelProvider.SHOW_ROOT);
		// ElementListSelectionDialog dialog = new ElementListSelectionDialog(
		// getParentShell(), labelProvider);
		// dialog.setTitle(JUnitMessages.TestMethodSelectionDialog_dialog_title);
		// String msg = Messages.format(
		// JUnitMessages.TestMethodSelectionDialog_testproject,
		//				"junit.framework.Test"); //$NON-NLS-1$
		// dialog.setMessage(msg);
		// IErlProject[] projects = new IErlProject[result.size()];
		// IErlModule[] testTypes = (IErlModule[]) result
		// .toArray(new IErlModule[result.size()]);
		// for (int i = 0; i < projects.length; i++) {
		// projects[i] = testTypes[i].getJavaProject();
		// }
		// dialog.setElements(projects);
		// if (dialog.open() == Window.CANCEL) {
		// return null;
		// }
		// IErlProject project = (IErlProject) dialog.getFirstResult();
		// for (int i = 0; i < testTypes.length; i++) {
		// if (testTypes[i].getErlProject().equals(project)) {
		// return testTypes[i];
		// }
		// }
		return null;
	}

	public Object[] searchTestMethods(final IErlElement element,
			final IErlModule testType) throws InvocationTargetException,
			InterruptedException {
		// final TestReferenceCollector[] col = new TestReferenceCollector[1];
		//
		// IRunnableWithProgress runnable = new IRunnableWithProgress() {
		// public void run(IProgressMonitor pm)
		// throws InvocationTargetException {
		// try {
		// col[0] = doSearchTestMethods(element, testType, pm);
		// } catch (CoreException e) {
		// throw new InvocationTargetException(e);
		// }
		// }
		// };
		// PlatformUI.getWorkbench().getProgressService()
		// .busyCursorWhile(runnable);
		// return col[0].getResult();
		return null;
	}

	// private TestReferenceCollector doSearchTestMethods(IErlElement element,
	// IErlModule testType, IProgressMonitor pm) throws CoreException {
	// int matchRule = SearchPattern.R_EXACT_MATCH
	// | SearchPattern.R_CASE_SENSITIVE
	// | SearchPattern.R_ERASURE_MATCH;
	// SearchPattern pattern = SearchPattern.createPattern(element,
	// IJavaSearchConstants.REFERENCES, matchRule);
	// SearchParticipant[] participants = new SearchParticipant[] {
	// SearchEngine
	// .getDefaultSearchParticipant() };
	// IJavaSearchScope scope = SearchEngine.createHierarchyScope(testType);
	// TestReferenceCollector requestor = new TestReferenceCollector();
	// new SearchEngine().search(pattern, participants, scope, requestor,
	// pm);
	// return requestor;
	// }
}
