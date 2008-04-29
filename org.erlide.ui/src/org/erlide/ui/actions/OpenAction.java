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
package org.erlide.ui.actions;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IStorage;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.util.OpenStrategy;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.erlide.basiccore.ErlLogger;
import org.erlide.basicui.util.IErlangStatusConstants;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlScanner;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlImport;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.ISourceReference;
import org.erlide.core.util.ErlangFunction;
import org.erlide.core.util.ResourceUtil;
import org.erlide.jinterface.rpc.Tuple;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.IBackend;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.util.EditorUtility;
import org.erlide.ui.util.ErlModelUtils;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

import erlang.ErlideOpen;
import erlang.OpenResult;

/**
 * This action opens a Erlang editor on a Erlang element or file.
 * <p>
 * The action is applicable to selections containing elements of type
 * <code>ICompilationUnit</code>, <code>IMember</code> or
 * <code>IFile</code>.
 * 
 * <p>
 * This class may be instantiated; it is not intended to be subclassed.
 * </p>
 * 
 * @since 2.0
 */
public class OpenAction extends SelectionDispatchAction {

	private ErlangEditor fEditor;
	private String fExternalModules;
	private List<Tuple> pathVars;

	/**
	 * Creates a new <code>OpenAction</code>. The action requires that the
	 * selection provided by the site's selection provider is of type <code>
	 * org.eclipse.jface.viewers.IStructuredSelection</code>.
	 * 
	 * @param site
	 *            the site providing context information for this action
	 * @param externalModules
	 *            the externalModules file that can be searched for references
	 *            to external modules
	 */
	public OpenAction(IWorkbenchSite site, String externalModules) {
		super(site);
		fExternalModules = externalModules;
		setText(ActionMessages.OpenAction_label);
		setToolTipText(ActionMessages.OpenAction_tooltip);
		setDescription(ActionMessages.OpenAction_description);
		initPathVars();
		PlatformUI.getWorkbench().getHelpSystem().setHelp(this, "erl.open");

	}

	public OpenAction(ErlangEditor editor, String externalModules) {
		this(editor.getEditorSite(), externalModules);
		fEditor = editor;
		fExternalModules = externalModules;
		initPathVars();
		setText(ActionMessages.OpenAction_declaration_label);
	}

	private void initPathVars() {
		final IPathVariableManager pvm = ResourcesPlugin.getWorkspace()
				.getPathVariableManager();
		final String[] names = pvm.getPathVariableNames();
		pathVars = new ArrayList<Tuple>(names.length);
		for (final String name : names) {
			pathVars.add(new Tuple().add(name).add(
					pvm.getValue(name).toOSString()));
		}
	}

	/*
	 * (non-Javadoc) Method declared on SelectionDispatchAction.
	 */
	@Override
	public void selectionChanged(ITextSelection selection) {
	}

	/*
	 * (non-Javadoc) Method declared on SelectionDispatchAction.
	 */
	@Override
	public void selectionChanged(IStructuredSelection selection) {
		setEnabled(checkEnabled(selection));
	}

	private boolean checkEnabled(IStructuredSelection selection) {
		if (selection.isEmpty()) {
			return false;
		}
		for (final Iterator<?> iter = selection.iterator(); iter.hasNext();) {
			final Object element = iter.next();
			if (element instanceof ISourceReference) {
				continue;
			}
			if (element instanceof IFile) {
				continue;
			}
			if (element instanceof IStorage) {
				continue;
			}
			return false;
		}
		return true;
	}

	/*
	 * (non-Javadoc) Method declared on SelectionDispatchAction.
	 */
	@Override
	public void run(ITextSelection selection) {
		ErlLogger.debug("*> goto " + selection);

		// if (!ActionUtil.isProcessable(getShell(), fEditor))
		// return;
		// try {
		// IErlElement element= SelectionConverter.codeResolve(fEditor,
		// getShell(),
		// getDialogTitle(),
		// ActionMessages.OpenAction_select_element);
		// if (element == null) {
		// IEditorStatusLine statusLine= (IEditorStatusLine)
		// fEditor.getAdapter(IEditorStatusLine.class);
		// if (statusLine != null)
		// statusLine.setMessage(true,
		// ActionMessages.OpenAction_error_messageBadSelection, null);
		// getShell().getDisplay().beep();
		// return;
		// }
		// IErlangElement input= SelectionConverter.getInput(fEditor);
		// int type= element.getElementType();
		// if (type == IErlangElement.Erlang_PROJECT || type ==
		// IErlangElement.PACKAGE_FRAGMENT_ROOT || type ==
		// IErlangElement.PACKAGE_FRAGMENT)
		// element= input;
		// run(new Object[] {element} );
		// } catch (ErlangModelException e) {
		// showError(e);
		// }
	}

	/*
	 * (non-Javadoc) Method declared on SelectionDispatchAction.
	 */
	@Override
	public void run(IStructuredSelection selection) {
		if (!checkEnabled(selection)) {
			return;
		}
		run(selection.toArray());
	}

	/**
	 * Note: this method is for internal use only. Clients should not call this
	 * method.
	 * 
	 * @param elements
	 *            the elements to process
	 */
	public void run(Object[] elements) {
		if (elements == null) {
			return;
		}
		for (Object element : elements) {
			try {
				element = getElementToOpen(element);
				final boolean activateOnOpen = fEditor != null ? true
						: OpenStrategy.activateOnOpen();
				ErlModelUtils.openElementInNewEditor(element, activateOnOpen);
			} catch (final ErlModelException e) {
				ErlangPlugin.log(new Status(IStatus.ERROR,
						ErlangPlugin.PLUGIN_ID,
						IErlangStatusConstants.INTERNAL_ERROR,
						"OpenAction_error_message", e));

				ErrorDialog.openError(getShell(), getDialogTitle(),
						ActionMessages.OpenAction_error_messageProblems, e
								.getStatus());

			} catch (final PartInitException x) {

				String name = null;

				if (element instanceof IErlElement) {
					name = ((IErlElement) element).getName();
				} else if (element instanceof IStorage) {
					name = ((IStorage) element).getName();
				} else if (element instanceof IResource) {
					name = ((IResource) element).getName();
				}

				if (name != null) {
					MessageDialog.openError(getShell(),
							ActionMessages.OpenAction_error_messageProblems, x
									.getMessage());
				}
			}
		}
	}

	//
	// /**
	// * Opens the editor on the given element and subsequently selects it.
	// */
	// private static void open(Object element) throws ErlModelException,
	// PartInitException
	// {
	// open(element, true);
	// }

	/**
	 * Note: this method is for internal use only. Clients should not call this
	 * method.
	 * 
	 * @param object
	 *            the element to open
	 * @return the real element to open
	 * @throws ErlangModelException
	 *             if an error occurs while accessing the Erlang model
	 */
	public Object getElementToOpen(Object object) throws ErlModelException {
		return object;
	}

	private String getDialogTitle() {
		return ActionMessages.OpenAction_error_title;
	}

	@SuppressWarnings("boxing")
	@Override
	public void run() {

		fEditor = (ErlangEditor) getSite().getPage().getActiveEditor();
		final IBackend b = BackendManager.getDefault().getIdeBackend();
		final ISelection sel = getSelection();
		final ITextSelection textSel = (ITextSelection) sel;
		final int offset = textSel.getOffset();
		try {
			final OpenResult res = ErlideOpen.open(b, ErlScanner
					.createScannerModuleName(fEditor.getModule()), offset,
					fExternalModules, pathVars);
			ErlLogger.debug("open " + res);
			final IErlModule module = ErlModelUtils.getModule(fEditor
					.getEditorInput());
			final IProject project = module == null ? null : module
					.getErlProject().getProject();
			if (res.isExternalCall()) {
				ErlModelUtils.openExternalFunction(res, project);
			} else if (res.isInclude()) {
				IResource r = ResourceUtil
						.recursiveFindNamedResourceWithReferences(project, res
								.getName());
				if (r == null) {
					try {
						final String m = ErlModelUtils.findIncludeFile(project,
								res.getName());
						if (m != null) {
							r = EditorUtility.openExternal(m);
						}
					} catch (final Exception e) {
						e.printStackTrace();
					}
				}
				if (r instanceof IFile) {
					final IFile f = (IFile) r;
					EditorUtility.openInEditor(f);
				}
			} else if (res.isLocalCall()) { // local call
				final IWorkbenchPage page = ErlideUIPlugin.getActivePage();
				if (page == null) {
					return;
				}
				final IEditorPart editor = page.getActiveEditor();
				if (!ErlModelUtils.openFunctionInEditor(res.getFun(), res
						.getArity(), editor)) { // not local imports
					if (module == null) {
						return;
					}
					final IErlImport ei = module.findImport(new ErlangFunction(
							res.getFun(), res.getArity()));
					if (ei == null) {
						return;
					}
					final String mod = ei.getImportModule();
					final OtpErlangObject res2 = ErlideOpen
							.getSourceFromModule(b, pathVars, mod,
									fExternalModules);
					if (res2 instanceof OtpErlangString) {
						final String path = ((OtpErlangString) res2)
								.stringValue();
						ErlModelUtils.openExternalFunction(mod, res.getFun(),
								res.getArity(), path, project);
					}
				}
				// } else if (external.equals("variable")) {
				// final OtpErlangTuple mf = (OtpErlangTuple) tres.elementAt(1);
				// final OtpErlangAtom var = (OtpErlangAtom) mf.elementAt(0);
				// final ITextSelection sel = (ITextSelection) fEditor
				// .getSelectionProvider().getSelection();
				// final IErlElement e = fEditor.getElementAt(sel.getOffset(),
				// false);
				// final ISourceReference sref = (ISourceReference) e;
				// final OtpErlangString s = new
				// OtpErlangString(sref.getSource());
				// final OtpErlangObject res2 = b.rpcx("erlide_open",
				// "find_first_var", var, s);
				// if (!(res2 instanceof OtpErlangTuple)) {
				// return;
				// }
				// final OtpErlangTuple mf2 = (OtpErlangTuple) res2;
				// final OtpErlangTuple t = (OtpErlangTuple) mf2.elementAt(1);
				// final int pos = ((OtpErlangLong) t.elementAt(0)).intValue();
				// final int len = ((OtpErlangLong) t.elementAt(1)).intValue();
				// fEditor.setHighlightRange(pos
				// + sref.getSourceRange().getOffset(), len, true);
			} else if (res.isRecord() || res.isMacro()) {
				final IWorkbenchPage page = ErlideUIPlugin.getActivePage();
				if (page == null) {
					return;
				}
				final boolean macro = res.isMacro();
				String definedName = res.getName();
				final IErlModule m = ErlModelUtils.getModule(fEditor
						.getEditorInput());
				if (definedName.length() == 0) {
					return;
				}
				if (definedName.charAt(0) == '?') {
					definedName = definedName.substring(1);
				}
				final IErlElement.Kind type = macro ? IErlElement.Kind.MACRO_DEF
						: IErlElement.Kind.RECORD_DEF;
				ErlModelUtils.openPreprocessorDef(project, page, m,
						definedName, type, new ArrayList<IErlModule>());
			}
		} catch (final Exception e) {
			e.printStackTrace();
		}
	}
}
