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

import java.util.Iterator;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.util.OpenStrategy;
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
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlImport;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlPreprocessorDef;
import org.erlide.core.erlang.ISourceReference;
import org.erlide.core.erlang.TokenWindow;
import org.erlide.core.util.ErlangFunction;
import org.erlide.core.util.ErlangIncludeFile;
import org.erlide.core.util.ResourceUtil;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.util.EditorUtility;
import org.erlide.ui.util.ErlModelUtils;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

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

	/**
	 * Creates a new <code>OpenAction</code>. The action requires that the
	 * selection provided by the site's selection provider is of type <code>
	 * org.eclipse.jface.viewers.IStructuredSelection</code>.
	 * 
	 * @param site
	 *            the site providing context information for this action
	 */
	public OpenAction(IWorkbenchSite site) {
		super(site);

		setText(ActionMessages.OpenAction_label);
		setToolTipText(ActionMessages.OpenAction_tooltip);
		setDescription(ActionMessages.OpenAction_description);
		PlatformUI.getWorkbench().getHelpSystem().setHelp(this, "erl.open");

	}

	public OpenAction(ErlangEditor editor) {
		this(editor.getEditorSite());
		fEditor = editor;
		setText(ActionMessages.OpenAction_declaration_label);
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
		for (final Iterator iter = selection.iterator(); iter.hasNext();) {
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
				open(element, activateOnOpen);
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
					name = ((IErlElement) element).getElementName();
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

	// private void showError(CoreException e)
	// {
	// ExceptionHandler.handle(e, getShell(), getDialogTitle(),
	// ActionMessages.OpenAction_error_message);
	// }

	@Override
	public void run() {
		int window = 5;

		fEditor = (ErlangEditor) getSite().getPage().getActiveEditor();
		final TokenWindow w = fEditor.getTokenWindow(window);
		if (w == null) {
			ErlLogger.debug("open::: null token window " + fEditor);
			return;
		}
		final OtpErlangList list = w.getList();
		window = w.getPos();
		final IBackend b = BackendManager.getDefault().getIdeBackend();
		try {
			final OtpErlangObject res = b.rpcx("erlide_open", "open_info",
					list, window);
			if (!(res instanceof OtpErlangTuple)) {
				return; // not a call, ignore
			}
			final OtpErlangTuple tres = (OtpErlangTuple) res;
			final OtpErlangTuple mf = (OtpErlangTuple) tres.elementAt(1);
			final String external = ((OtpErlangAtom) tres.elementAt(0))
					.atomValue();
			if (external.equals("external")) {
				final String mod = ((OtpErlangAtom) mf.elementAt(0))
						.atomValue();
				final String fun = ((OtpErlangAtom) mf.elementAt(1))
						.atomValue();
				final int arity = ((OtpErlangLong) mf.elementAt(2)).intValue();
				if (mf.elementAt(3) instanceof OtpErlangString) {
					final String path = ((OtpErlangString) mf.elementAt(3))
							.stringValue();
					open(mod, fun, arity, path);
				}
			} else if (external.equals("local")) { // local call
				final String fun = ((OtpErlangAtom) mf.elementAt(0))
						.atomValue();
				final int arity = ((OtpErlangLong) mf.elementAt(1)).intValue();
				final IWorkbenchPage page = ErlideUIPlugin.getActivePage();
				if (page == null) {
					return;
				}
				final IEditorPart editor = page.getActiveEditor();
				if (!open(fun, arity, editor)) { // not local, so check
					// imports
					final IErlModule m = ErlModelUtils.getModule(editor
							.getEditorInput());
					final IErlImport ei = m.findImport(new ErlangFunction(fun,
							arity));
					if (ei == null) {
						return;
					}
					final String mod = ei.getImportModule();
					final OtpErlangAtom a = new OtpErlangAtom(mod);
					final OtpErlangObject res2 = b.rpcx("erlide_open",
							"get_source_from_module", a);
					if (res2 instanceof OtpErlangString) {
						final String path = ((OtpErlangString) res2)
								.stringValue();
						open(mod, fun, arity, path);
					}
				}
			} else if (external.equals("variable")) {
				final OtpErlangAtom var = (OtpErlangAtom) mf.elementAt(0);
				final ITextSelection sel = (ITextSelection) fEditor
						.getSelectionProvider().getSelection();
				final IErlElement e = fEditor.getElementAt(sel.getOffset(),
						false);
				final ISourceReference sref = (ISourceReference) e;
				final OtpErlangString s = new OtpErlangString(sref.getSource());
				final OtpErlangObject res2 = b.rpcx("erlide_open",
						"find_first_var", var, s);
				if (!(res2 instanceof OtpErlangTuple)) {
					return;
				}
				final OtpErlangTuple mf2 = (OtpErlangTuple) res2;
				final OtpErlangTuple t = (OtpErlangTuple) mf2.elementAt(1);
				final int pos = ((OtpErlangLong) t.elementAt(0)).intValue();
				final int len = ((OtpErlangLong) t.elementAt(1)).intValue();
				fEditor.setHighlightRange(pos
						+ sref.getSourceRange().getOffset(), len, true);
			} else if (external.equals("record") || external.equals("macro")) {
				final IWorkbenchPage page = ErlideUIPlugin.getActivePage();
				if (page == null) {
					return;
				}
				final boolean macro = external.equals("macro");
				final OtpErlangAtom defined = (OtpErlangAtom) mf.elementAt(0);
				IErlModule m = ErlModelUtils
						.getModule(fEditor.getEditorInput());
				final String definedName = defined.atomValue();
				final IErlElement.ErlElementType type = macro ? IErlElement.ErlElementType.MACRO_DEF
						: IErlElement.ErlElementType.RECORD_DEF;
				IErlPreprocessorDef pd = m.findPreprocessorDef(definedName,
						type);
				IEditorPart editor = page.getActiveEditor();
				if (pd == null) {
					final ErlangIncludeFile[] includes = m.getIncludedFiles();
					for (ErlangIncludeFile element : includes) {
						final IResource re = ResourceUtil
								.recursiveFindNamedResource(element
										.getFilename());
						if (re != null && re instanceof IFile) {
							m = ErlModelUtils.getModule((IFile) re);
							if (m != null) {
								m.open(null);
								pd = m.findPreprocessorDef(definedName, type);
								if (pd != null) {
									editor = EditorUtility.openInEditor(re,
											true);
									break;
								}
							}
						}
					}
				}
				if (pd == null) {
					return;
				}
				EditorUtility.revealInEditor(editor, pd);
			}
		} catch (final ErlangRpcException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final BackendException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final OtpErlangRangeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final CoreException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final ClassCastException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/**
	 * Open an editor on the given module and select the given erlang function
	 * 
	 * @param mod
	 *            module name (without .erl)
	 * @param fun
	 *            function name
	 * @param arity
	 *            function arity
	 * @param path
	 *            path to module (including .erl)
	 * @throws CoreException
	 */
	private void open(String mod, String fun, int arity, String path)
			throws CoreException {
		final String modFileName = mod + ".erl";
		IResource r = ResourceUtil.recursiveFindNamedResource(modFileName);
		if (r == null) {
			try {
				r = EditorUtility.openExternal(path);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		if (r != null && r instanceof IFile) {
			final IFile f = (IFile) r;
			try {
				final IEditorPart editor = EditorUtility.openInEditor(f);
				open(fun, arity, editor);
			} catch (final PartInitException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (final ErlModelException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	/**
	 * @param path
	 * @return
	 * @throws CoreException
	 */
	/**
	 * Activate editor and select erlang function
	 * 
	 * @param fun
	 * @param arity
	 * @param editor
	 * @throws ErlModelException
	 */
	private boolean open(String fun, int arity, IEditorPart editor)
			throws ErlModelException {
		if (editor == null) {
			return false;
		}
		final IErlModule m = ErlModelUtils.getModule(editor.getEditorInput());
		if (m == null) {
			return false;
		}
		final IErlFunction function = ErlModelUtils.findFunction(m, fun, arity);
		if (function == null) {
			return false;
		}
		EditorUtility.revealInEditor(editor, function);
		return true;
	}

	/**
	 * Opens the editor on the given element and subsequently selects it.
	 */
	private static void open(Object element, boolean activate)
			throws ErlModelException, PartInitException {
		final IEditorPart part = EditorUtility.openInEditor(element, activate);
		if (element instanceof IErlElement) {
			EditorUtility.revealInEditor(part, (IErlElement) element);
		}
	}
}
