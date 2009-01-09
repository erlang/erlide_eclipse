/*******************************************************************************
 * Copyright (c) 2000, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.search;

import java.util.ArrayList;

import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.search.ui.NewSearchUI;
import org.eclipse.ui.IWorkbenchSite;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.progress.IProgressService;
import org.erlide.core.erlang.ErlScanner;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.search.ErlangExternalFunctionCallRef;
import org.erlide.jinterface.rpc.Tuple;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.Backend;
import org.erlide.ui.actions.SelectionDispatchAction;
import org.erlide.ui.editors.erl.ErlangEditor;

import erlang.ErlideOpen;
import erlang.OpenResult;

/**
 * Abstract class for Java search actions.
 * <p>
 * Note: This class is for internal use only. Clients should not use this class.
 * </p>
 * 
 * @since 2.0
 */
public abstract class FindAction extends SelectionDispatchAction {

	// A dummy which can't be selected in the UI
	// private static final IJavaElement RETURN_WITHOUT_BEEP = JavaCore
	// .create(JavaPlugin.getWorkspace().getRoot());

	private final Class<?>[] fValidTypes;
	private ErlangEditor fEditor;

	FindAction(final IWorkbenchSite site) {
		super(site);
		fValidTypes = getValidTypes();
		init();
	}

	FindAction(final ErlangEditor editor) {
		this(editor.getEditorSite());
		fEditor = editor;
		setEnabled(true); // FIXME kolla selection, sno grejer från open
		// kanske...
	}

	/**
	 * Called once by the constructors to initialize label, tooltip, image and
	 * help support of the action. To be overridden by implementors of this
	 * action.
	 */
	abstract void init();

	/**
	 * Called once by the constructors to get the list of the valid input types
	 * of the action. To be overridden by implementors of this action.
	 * 
	 * @return the valid input types of the action
	 */
	abstract Class<?>[] getValidTypes();

	private boolean canOperateOn(final IStructuredSelection sel) {
		return sel != null && !sel.isEmpty()
				&& canOperateOn(getErlElement(sel, true));
	}

	boolean canOperateOn(final IErlElement element) {
		if (element == null || fValidTypes == null || fValidTypes.length == 0) {
			// || !ActionUtil.isOnBuildPath(element)) {
			return false;
		}

		for (int i = 0; i < fValidTypes.length; i++) {
			if (fValidTypes[i].isInstance(element)) {
				return true;
			}
		}
		return false;
	}

	// private IErlElement getTypeIfPossible(IErlElement o, final boolean
	// silent) {
	// switch (o.getElementType()) {
	// case IJavaElement.COMPILATION_UNIT:
	// if (silent) {
	// return o;
	// } else {
	// return findType((ICompilationUnit) o, silent);
	// }
	// case IJavaElement.CLASS_FILE:
	// return ((IClassFile) o).getType();
	// default:
	// return o;
	// }
	// }

	IErlElement getErlElement(final IStructuredSelection selection,
			final boolean silent) {
		if (selection.size() == 1) {
			final Object firstElement = selection.getFirstElement();
			IErlElement elem = null;
			if (firstElement instanceof IErlElement) {
				elem = (IErlElement) firstElement;
			} else if (firstElement instanceof IAdaptable) {
				elem = (IErlElement) ((IAdaptable) firstElement)
						.getAdapter(IErlElement.class);
			}
			// if (elem != null) {
			// return getTypeIfPossible(elem, silent);
			// }
			return elem;
		}
		return null;
	}

	private void showOperationUnavailableDialog() {
		MessageDialog.openInformation(getShell(), "Operation unavailable",
				getOperationUnavailableMessage());
	}

	String getOperationUnavailableMessage() {
		return "Operation unavailable";
	}

	// private IJavaElement findType(ICompilationUnit cu, final boolean silent)
	// {
	// IType[] types = null;
	// try {
	// types = cu.getAllTypes();
	// } catch (JavaModelException ex) {
	// if (JavaModelUtil.isExceptionToBeLogged(ex)) {
	// ExceptionHandler.log(ex,
	// SearchMessages.JavaElementAction_error_open_message);
	// }
	// if (silent) {
	// return RETURN_WITHOUT_BEEP;
	// } else {
	// return null;
	// }
	// }
	// if (types.length == 1 || silent && types.length > 0) {
	// return types[0];
	// }
	// if (silent) {
	// return RETURN_WITHOUT_BEEP;
	// }
	// if (types.length == 0) {
	// return null;
	// }
	// final String title =
	// SearchMessages.JavaElementAction_typeSelectionDialog_title;
	// final String message =
	// SearchMessages.JavaElementAction_typeSelectionDialog_message;
	// final int flags = JavaElementLabelProvider.SHOW_DEFAULT;
	//
	// final ElementListSelectionDialog dialog = new ElementListSelectionDialog(
	// getShell(), new JavaElementLabelProvider(flags));
	// dialog.setTitle(title);
	// dialog.setMessage(message);
	// dialog.setElements(types);
	//
	// if (dialog.open() == Window.OK) {
	// return (IType) dialog.getFirstResult();
	// } else {
	// return RETURN_WITHOUT_BEEP;
	// }
	// }

	/*
	 * Method declared on SelectionChangedAction.
	 */
	@Override
	public void run(final IStructuredSelection selection) {
		final IErlElement element = getErlElement(selection, false);
		if (element == null || !element.exists()) {
			showOperationUnavailableDialog();
			return;
		}
		// else if (element == RETURN_WITHOUT_BEEP) {
		// return;
		// }

		run(element);
	}

	/*
	 * Method declared on SelectionChangedAction.
	 */
	@Override
	public void run(final ITextSelection selection) {
		// if (!ActionUtil.isProcessable(fEditor)) {
		// return;
		// }
		final Backend b = ErlangCore.getBackendManager().getIdeBackend();
		final ISelection sel = getSelection();
		final ITextSelection textSel = (ITextSelection) sel;
		final int offset = textSel.getOffset();
		try {
			final OpenResult res = ErlideOpen.open(b, ErlScanner
					.createScannerModuleName(fEditor.getModule()), offset, "",
					new ArrayList<Tuple>(0));
			ErlLogger.debug("open " + res);

			// final String title =
			// "SearchMessages.SearchElementSelectionDialog_title";
			// final String message =
			// "SearchMessages.SearchElementSelectionDialog_message";

			if (res.isExternalCall()) {
				performNewSearch(SearchUtil.getRefFromOpenRes(res));
			}
		} catch (final Exception e) {
			// final String title = "SearchMessages.Search_Error_search_title";
			// final String message = "SearchMessages.Search_Error_codeResolve";
			// ExceptionHandler.handle(e, getShell(), title, message);
			ErlLogger.debug(e);
		}
	}

	abstract protected String[] getScope();

	private void performNewSearch(final ErlangExternalFunctionCallRef ref) {

		final ErlSearchQuery query = new ErlSearchQuery(ref,
				IErlSearchConstants.REFERENCES, IErlSearchConstants.FUNCTION,
				getScope());
		if (query.canRunInBackground()) {
			/*
			 * This indirection with Object as parameter is needed to prevent
			 * the loading of the Search plug-in: the VM verifies the method
			 * call and hence loads the types used in the method signature,
			 * eventually triggering the loading of a plug-in (in this case
			 * ISearchQuery results in Search plug-in being loaded).
			 */
			SearchUtil.runQueryInBackground(query);
		} else {
			final IProgressService progressService = PlatformUI.getWorkbench()
					.getProgressService();
			/*
			 * This indirection with Object as parameter is needed to prevent
			 * the loading of the Search plug-in: the VM verifies the method
			 * call and hence loads the types used in the method signature,
			 * eventually triggering the loading of a plug-in (in this case it
			 * would be ISearchQuery).
			 */
			final IStatus status = SearchUtil.runQueryInForeground(
					progressService, query);
			if (status.matches(IStatus.ERROR | IStatus.INFO | IStatus.WARNING)) {
				ErrorDialog.openError(getShell(),
						"SearchMessages.Search_Error_search_title",
						"SearchMessages.Search_Error_search_message", status);
			}
		}
	}

	/*
	 * Method declared on SelectionChangedAction.
	 */
	@Override
	public void selectionChanged(final IStructuredSelection selection) {
		setEnabled(canOperateOn(selection));
	}

	/*
	 * Method declared on SelectionChangedAction.
	 */
	@Override
	public void selectionChanged(final ITextSelection selection) {
		setEnabled(true); // FIXME japps
	}

	/**
	 * Executes this action for the given java element.
	 * 
	 * @param element
	 *            The erlang element to be found.
	 */
	public void run(final IErlElement element) {

		// will return true except for debugging purposes.
		performNewSearch(element);

	}

	private void performNewSearch(final IErlElement element) {
		final ErlangExternalFunctionCallRef ref = SearchUtil
				.getRefFromErlElement(element);
		final ErlSearchQuery query = new ErlSearchQuery(ref,
				IErlSearchConstants.REFERENCES, IErlSearchConstants.FUNCTION,
				getScope());
		if (query.canRunInBackground()) {
			/*
			 * This indirection with Object as parameter is needed to prevent
			 * the loading of the Search plug-in: the VM verifies the method
			 * call and hence loads the types used in the method signature,
			 * eventually triggering the loading of a plug-in (in this case
			 * ISearchQuery results in Search plug-in being loaded).
			 */
			NewSearchUI.runQueryInBackground(query);
		} else {
			final IProgressService progressService = PlatformUI.getWorkbench()
					.getProgressService();
			/*
			 * This indirection with Object as parameter is needed to prevent
			 * the loading of the Search plug-in: the VM verifies the method
			 * call and hence loads the types used in the method signature,
			 * eventually triggering the loading of a plug-in (in this case it
			 * would be ISearchQuery).
			 */
			final IStatus status = NewSearchUI.runQueryInForeground(
					progressService, query);
			if (status.matches(IStatus.ERROR | IStatus.INFO | IStatus.WARNING)) {
				ErrorDialog.openError(getShell(),
						"SearchMessages.Search_Error_search_title",
						"SearchMessages.Search_Error_search_message", status);
			}
		}
	}

	/**
	 * @return the fEditor
	 */
	public ErlangEditor getEditor() {
		return fEditor;
	}

	// private ErlangEditor getEditor() {
	// return fEditor;
	// }

}
