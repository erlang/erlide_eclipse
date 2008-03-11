/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.util;

import java.io.File;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.basiccore.ErlLogger;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlImport;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlPreprocessorDef;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IErlScanner;
import org.erlide.core.util.ErlangFunction;
import org.erlide.core.util.ErlangIncludeFile;
import org.erlide.core.util.ResourceUtil;
import org.erlide.runtime.ErlangProjectProperties;
import org.erlide.ui.editors.util.EditorUtility;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideOpen;

public class ErlModelUtils {

	public static IErlModule getModule(ITextEditor editor) {
		if (editor == null) {
			return null;
		}
		return getModule(editor.getEditorInput());
	}

	public static IErlModule getModule(IEditorInput editorInput) {

		if (editorInput instanceof IFileEditorInput) {
			final IFileEditorInput input = (IFileEditorInput) editorInput;
			return getModule(input.getFile());
		}
		return null;
	}

	public static IErlModule getModule(IFile file) {
		final String prj = file.getProject().getName();
		final IErlModel mdl = ErlangCore.getModel();

		try {
			mdl.open(null);
			return mdl.getErlangProject(prj).getModule(file.getName());
		} catch (final ErlModelException e) {
			return null;
		}
	}

	public static IErlProject getErlProject(ITextEditor editor) {
		return getErlProject(editor.getEditorInput());
	}

	public static IErlProject getErlProject(IEditorInput editorInput) {
		if (editorInput instanceof IFileEditorInput) {
			final IFileEditorInput input = (IFileEditorInput) editorInput;
			final IErlModel mdl = ErlangCore.getModel();

			final String prj = input.getFile().getProject().getName();

			try {
				mdl.open(null);
				return mdl.getErlangProject(prj);
			} catch (final ErlModelException e) {
				return null;
			}
		}
		return null;
	}

	public static IErlFunction findFunction(IErlModule module, String function,
			int arity) throws ErlModelException {
		final IErlElement[] children = module.getChildren();
		for (final IErlElement element : children) {
			if (element instanceof IErlFunction) {
				final IErlFunction f = (IErlFunction) element;
				if (arity == -1 || f.getArity() == arity) {
					if (f.getElementName().equals(function)) {
						return f;
					}
				}
			}
		}
		return null;
	}

	public static OtpErlangList getImportsAsList(IErlModule mod) {
		if (mod == null) {
			return new OtpErlangList();
		}

		final IErlImport[] imports = mod.getImports();
		final OtpErlangTuple rImports[] = new OtpErlangTuple[imports.length];
		for (int i = 0; i < imports.length; ++i) {
			final IErlImport imp = imports[i];
			final ErlangFunction[] impFuncs = imp.getFunctions();
			final OtpErlangTuple rImpFuncs[] = new OtpErlangTuple[impFuncs.length];
			for (final ErlangFunction f : impFuncs) {
				rImpFuncs[i] = new OtpErlangTuple(new OtpErlangAtom(f.name),
						new OtpErlangLong(f.arity));
			}
			rImports[i] = new OtpErlangTuple(new OtpErlangAtom(imp
					.getImportModule()), new OtpErlangList(rImpFuncs));
		}
		return new OtpErlangList(rImports);
	}

	public static IErlScanner getScanner(ITextEditor editor) {
		final IErlModule mod = getModule(editor);
		// ErlLogger.debug("getScanner:: " + editor + " = " + mod);
		if (mod != null) {
			return getModule(editor).getScanner();
		}
		return null;
	}

	public static IErlFunction findFunctionFromPos(IErlModule module, int offset) {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * @param b
	 * @param project
	 * @param page
	 * @param m
	 * @param definedName
	 * @param type
	 * @throws CoreException
	 * @throws ErlModelException
	 * @throws PartInitException
	 */
	public static boolean openPreprocessorDef(IProject project,
			final IWorkbenchPage page, IErlModule m, String definedName,
			final IErlElement.ErlElementType type,
			List<IErlModule> modulesDone, IPathVariableManager pvm)
			throws CoreException, ErlModelException, PartInitException {
		if (m == null) {
			return false;
		}
		modulesDone.add(m);
		m.open(null);
		final IErlPreprocessorDef pd = m.findPreprocessorDef(definedName, type);
		if (pd == null) {
			final ErlangIncludeFile[] includes = m.getIncludedFiles();
			for (final ErlangIncludeFile element : includes) {
				IResource re = ResourceUtil
						.recursiveFindNamedResourceWithReferences(project,
								element.getFilenameLastPart());
				if (re == null) {
					try {
						String s = element.getFilename();
						if (element.isSystemInclude()) {
							s = ErlideOpen.getIncludeLib(s);
						} else {
							s = findIncludeFile(project, s, pvm);
						}
						re = EditorUtility.openExternal(s);
					} catch (final Exception e) {
						e.printStackTrace();
					}
				}
				if (re != null && re instanceof IFile) {
					m = getModule((IFile) re);
					if (m != null && !modulesDone.contains(m)) {
						if (openPreprocessorDef(project, page, m, definedName,
								type, modulesDone, pvm)) {
							return true;
						}
					}
				}
			}
		}
		if (pd != null) {
			final IEditorPart editor = EditorUtility.openInEditor(m);
			EditorUtility.revealInEditor(editor, pd);
			return true;
		}
		return false;
	}

	/**
	 * Try to find include file, by searching include paths in the project
	 * (replacing with path variables if needed). If the file is not in the
	 * include paths, the original path is returned
	 * 
	 * @param project
	 *            the project with include dirs
	 * @param filePath
	 *            the path to the include file
	 * @param pvm
	 *            The global IPathVariableManager of the workbench
	 * @return the path to the include file
	 */
	public static String findIncludeFile(IProject project, String filePath,
			IPathVariableManager pvm) {
		final ErlangProjectProperties prefs = new ErlangProjectProperties(
				project);
		for (final String includeDir : prefs.getIncludeDirs()) {
			IPath p = new Path(includeDir).append(filePath);
			p = pvm.resolvePath(p);
			final File f = new File(p.toOSString());
			if (f.exists()) {
				return p.toString();
			}
		}
		return filePath;
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
	public static void openExternalFunction(String mod, String fun, int arity,
			String path, IProject p) throws CoreException {
		final String modFileName = mod + ".erl";
		ErlLogger.debug("open ex mod" + modFileName);
		IResource r = null;
		if (p != null) {
			r = ResourceUtil.recursiveFindNamedResourceWithReferences(p,
					modFileName);
		}
		if (r == null) {
			try {
				r = EditorUtility.openExternal(path);
			} catch (final Exception e) {
				e.printStackTrace();
			}
		}
		if (r != null && r instanceof IFile) {
			final IFile f = (IFile) r;
			try {
				final IEditorPart editor = EditorUtility.openInEditor(f);
				openFunctionInEditor(fun, arity, editor);
			} catch (final PartInitException e) {
				e.printStackTrace();
			} catch (final ErlModelException e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * Activate editor and select erlang function
	 * 
	 * @param fun
	 * @param arity
	 * @param editor
	 * @throws ErlModelException
	 */
	public static boolean openFunctionInEditor(String fun, int arity,
			IEditorPart editor) throws ErlModelException {
		if (editor == null) {
			return false;
		}
		final IErlModule module = getModule(editor.getEditorInput());
		ErlLogger.debug("open fun m" + fun + "/" + arity + " " + module);
		if (module == null) {
			return false;
		}
		module.open(null);
		final IErlFunction function = findFunction(module, fun, arity);
		if (function == null) {
			return false;
		}
		EditorUtility.revealInEditor(editor, function);
		return true;
	}

	/**
	 * Opens the editor on the given element and subsequently selects it.
	 */
	public static void openElementInNewEditor(Object element, boolean activate)
			throws ErlModelException, PartInitException {
		final IEditorPart part = EditorUtility.openInEditor(element, activate);
		if (element instanceof IErlElement) {
			EditorUtility.revealInEditor(part, (IErlElement) element);
		}
	}

}
