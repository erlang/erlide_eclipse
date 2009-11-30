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

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.util.OpenStrategy;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.ErlangStatusConstants;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlImport;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.ISourceRange;
import org.erlide.core.erlang.ISourceReference;
import org.erlide.core.erlang.util.ErlangFunction;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.core.erlang.util.ResourceUtil;
import org.erlide.core.text.ErlangToolkit;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.util.EditorUtility;
import org.erlide.ui.util.ErlModelUtils;

import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideOpen;
import erlang.OpenResult;

/**
 * This action opens a Erlang editor on a Erlang element or file.
 * <p>
 * The action is applicable to selections containing elements of type
 * <code>ICompilationUnit</code>, <code>IMember</code> or <code>IFile</code>.
 * 
 * <p>
 * This class may be instantiated; it is not intended to be subclassed.
 * </p>
 * 
 * @since 2.0
 */
public class OpenAction extends SelectionDispatchAction {

	// private ErlangEditor fEditor;
	// private final String fExternalModules;
	// private final String fExternalIncludes;
	// private final List<Tuple> pathVars;

	/**
	 * Creates a new <code>OpenAction</code>. The action requires that the
	 * selection provided by the site's selection provider is of type <code>
	 * org.eclipse.jface.viewers.IStructuredSelection</code>
	 * .
	 * 
	 * @param site
	 *            the site providing context information for this action
	 * @param externalModules
	 *            the externalModules file that can be searched for references
	 *            to external modules
	 */
	public OpenAction(final IWorkbenchSite site) {
		super(site);
		// fExternalModules = externalModules;
		// fExternalIncludes = externalIncludes;
		setText(ActionMessages.OpenAction_label);
		setToolTipText(ActionMessages.OpenAction_tooltip);
		setDescription(ActionMessages.OpenAction_description);
		// initPathVars();
		// pathVars = getPathVars();
		PlatformUI.getWorkbench().getHelpSystem().setHelp(this, "erl.open");

	}

	// public static List<Tuple> getPathVars() {
	// final IPathVariableManager pvm = ResourcesPlugin.getWorkspace()
	// .getPathVariableManager();
	// final String[] names = pvm.getPathVariableNames();
	// final ArrayList<Tuple> result = new ArrayList<Tuple>(names.length);
	// for (final String name : names) {
	// result.add(new Tuple().add(name).add(
	// pvm.getValue(name).toOSString()));
	// }
	// return result;
	// }
	//
	// public OpenAction(final ErlangEditor editor, final String
	// externalModules,
	// final String externalIncludes) {
	// this(editor.getEditorSite(), externalModules, externalIncludes);
	// fEditor = editor;
	// setText(ActionMessages.OpenAction_declaration_label);
	// }

	/*
	 * (non-Javadoc) Method declared on SelectionDispatchAction.
	 */
	@Override
	public void selectionChanged(final ITextSelection selection) {
	}

	/*
	 * (non-Javadoc) Method declared on SelectionDispatchAction.
	 */
	@Override
	public void selectionChanged(final IStructuredSelection selection) {
		setEnabled(checkEnabled(selection));
	}

	private boolean checkEnabled(final IStructuredSelection selection) {
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
	public void run(final ITextSelection selection) {
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
	public void run(final IStructuredSelection selection) {
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
	public void run(final Object[] elements) {
		if (elements == null) {
			return;
		}
		for (Object element : elements) {
			try {
				element = getElementToOpen(element);
				final boolean activateOnOpen = getSite() != null ? true
						: OpenStrategy.activateOnOpen();
				EditorUtility.openElementInEditor(element, activateOnOpen);
			} catch (final ErlModelException e) {
				ErlangPlugin.log(new Status(IStatus.ERROR,
						ErlangPlugin.PLUGIN_ID,
						ErlangStatusConstants.INTERNAL_ERROR,
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
	public Object getElementToOpen(final Object object)
			throws ErlModelException {
		return object;
	}

	private String getDialogTitle() {
		return ActionMessages.OpenAction_error_title;
	}

	@Override
	public void run() {
		final ErlangEditor editor = (ErlangEditor) getSite().getPage()
				.getActiveEditor();
		editor.reconcileNow();
		final IErlModule module = editor.getModule();
		if (module == null) {
			return;
		}
		final Backend b = ErlangCore.getBackendManager().getIdeBackend();
		final ISelection sel = getSelection();
		final ITextSelection textSel = (ITextSelection) sel;
		final int offset = textSel.getOffset();
		try {
			final IErlProject erlProject = module.getErlProject();
			final IErlModel model = ErlangCore.getModel();
			final OpenResult res = ErlideOpen.open(b, ErlangToolkit
					.createScannerModuleName(editor.getModule()), offset, model
					.getExternal(erlProject, ErlangCore.EXTERNAL_MODULES),
					model.getPathVars());
			ErlLogger.debug("open " + res);
			openOpenResult(editor, module, b, offset, erlProject, res);
		} catch (final Exception e) {
			ErlLogger.warn(e);
		}
	}

	public static void openOpenResult(final ErlangEditor editor,
			final IErlModule module, final Backend b, final int offset,
			final IErlProject erlProject, final OpenResult res)
			throws CoreException, ErlModelException, PartInitException,
			BadLocationException, OtpErlangRangeException {
		final IErlModel model = ErlangCore.getModel();
		final IProject project = erlProject == null ? null : erlProject
				.getProject();
		if (res.isExternalCall()) {
			if (editor != null) {
				final IErlElement e = editor.getElementAt(offset, true);
				if (e.getKind() == IErlElement.Kind.TYPESPEC) {
					if (ErlModelUtils.openExternalType(res.getName(), res
							.getFun(), res.getPath(), project)) {
						return;
					}
				}
			}
			if (!ErlModelUtils.openExternalFunction(res.getName(), res
					.getFunction(), res.getPath(), project)) {
				ErlModelUtils.openExternalFunction(res.getName(),
						new ErlangFunction(res.getFun(),
								IErlModel.UNKNOWN_ARITY), res.getPath(),
						project);
			}
		} else if (res.isInclude()) {
			IContainer parent = module == null ? null : module.getResource()
					.getParent();
			IResource r = ResourceUtil
					.recursiveFindNamedResourceWithReferences(project, res
							.getName(), org.erlide.core.erlang.util.PluginUtils
							.getIncludePathFilter(project, parent));
			if (r == null) {
				try {
					final String includeFile = ErlModelUtils.findIncludeFile(
							project, res.getName(), model.getExternal(
									erlProject, ErlangCore.EXTERNAL_INCLUDES));
					if (includeFile != null) {
						r = EditorUtility.openExternal(includeFile);
					}
				} catch (final Exception e) {
					ErlLogger.warn(e);
				}
			}
			if (r instanceof IFile) {
				final IFile f = (IFile) r;
				EditorUtility.openInEditor(f);
			}
		} else if (res.isLocalCall()) {
			if (editor != null) {
				final IErlElement e = editor.getElementAt(offset, true);
				if (e.getKind() == IErlElement.Kind.TYPESPEC) {
					if (ErlModelUtils.openTypeInEditor(res.getFun(), editor)) {
						return;
					}
				}
			}
			if (!ErlModelUtils.openFunctionInEditor(res.getFunction(), editor)) {
				// not local imports
				OtpErlangObject res2 = null;
				String mod = null;
				if (module != null) {
					final IErlImport ei = module.findImport(res.getFunction());
					if (ei != null) {
						mod = ei.getImportModule();
						try {
							res2 = ErlideOpen.getSourceFromModule(b, model
									.getPathVars(), mod, model.getExternal(
									erlProject, ErlangCore.EXTERNAL_MODULES));
						} catch (final BackendException e1) {
							ErlLogger.warn(e1);
						}
					}
				}
				if (res2 instanceof OtpErlangString && mod != null) {
					final OtpErlangString otpErlangString = (OtpErlangString) res2;
					final String path = otpErlangString.stringValue();
					ErlModelUtils.openExternalFunction(mod, res.getFunction(),
							path, project);
				} else {
					ErlModelUtils.openFunctionInEditor(new ErlangFunction(res
							.getFun(), IErlModel.UNKNOWN_ARITY), editor);
				}
			}
		} else if (res.isVariable()) {
			final IErlElement e = editor.getElementAt(offset, false);
			if (!(e instanceof ISourceReference)) {
				return;
			}
			final ISourceReference sref = (ISourceReference) e;
			final ISourceRange range = sref.getSourceRange();
			final String s = editor.getDocument().get(range.getOffset(),
					range.getLength());
			final OtpErlangTuple res2 = ErlideOpen.findFirstVar(b, res
					.getName(), s);
			if (res2 == null) {
				return;
			}
			final int pos = ((OtpErlangLong) res2.elementAt(0)).intValue() - 1;
			final int len = ((OtpErlangLong) res2.elementAt(1)).intValue();
			final int start = pos + sref.getSourceRange().getOffset();
			editor.selectAndReveal(start, len);
			// editor.setHighlightRange(start, len, true);
		} else if (res.isRecord() || res.isMacro()) {
			final IWorkbenchPage page = ErlideUIPlugin.getActivePage();
			if (page == null) {
				return;
			}
			final boolean macro = res.isMacro();
			final String definedName = res.getName();
			if (definedName.length() == 0) {
				return;
			}
			final IErlElement.Kind type = macro ? IErlElement.Kind.MACRO_DEF
					: IErlElement.Kind.RECORD_DEF;
			final String[] s = new String[] { definedName,
					ErlideUtil.unquote(definedName) };
			for (final String string : s) {
				if (ErlModelUtils.openPreprocessorDef(b, project, page, module,
						string, type, model.getExternal(erlProject,
								ErlangCore.EXTERNAL_INCLUDES),
						new ArrayList<IErlModule>())) {
					break;
				}
			}
		}
	}
}
