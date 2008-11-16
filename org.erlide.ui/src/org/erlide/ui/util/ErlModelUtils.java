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
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.texteditor.ITextEditor;
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
import org.erlide.core.util.ErlangIncludeFile;
import org.erlide.core.util.ResourceUtil;
import org.erlide.runtime.ErlangProjectProperties;
import org.erlide.runtime.backend.IdeBackend;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.util.EditorUtility;

import erlang.ErlideOpen;
import erlang.OpenResult;

public class ErlModelUtils {

	public static IErlModule getModule(final ITextEditor editor) {
		if (editor == null) {
			return null;
		}
		return getModule(editor.getEditorInput());
	}

	public static IErlModule getModule(final IEditorInput editorInput) {

		if (editorInput instanceof IFileEditorInput) {
			final IFileEditorInput input = (IFileEditorInput) editorInput;
			return getModule(input.getFile());
		}
		return null;
	}

	public static IErlModule getModule(final IFile file) {
		final IErlModel model = ErlangCore.getModel();
		try {
			model.open(null);
			return model.getModule(file);
		} catch (final ErlModelException e) {
			return null;
		}
	}

	public static IErlProject getErlProject(final ITextEditor editor) {
		return getErlProject(editor.getEditorInput());
	}

	public static IErlProject getErlProject(final IEditorInput editorInput) {
		if (editorInput instanceof IFileEditorInput) {
			final IFileEditorInput input = (IFileEditorInput) editorInput;
			final IErlModel model = ErlangCore.getModel();
			final String prj = input.getFile().getProject().getName();
			try {
				model.open(null);
				return model.getErlangProject(prj);
			} catch (final ErlModelException e) {
				return null;
			}
		}
		return null;
	}

	public static IErlFunction findFunction(final IErlModule module,
			final String function, final int arity) throws ErlModelException {
		final List<? extends IErlElement> children = module.getChildren();
		for (final IErlElement element : children) {
			if (element instanceof IErlFunction) {
				final IErlFunction f = (IErlFunction) element;
				if (arity == IErlModel.UNKNOWN_ARITY || f.getArity() == arity) {
					if (f.getName().equals(function)) {
						return f;
					}
				}
			}
		}
		return null;
	}

	public static List<IErlPreprocessorDef> getPreprocessorDefs(
			final IdeBackend b, final IProject project,
			final IErlModule module, final IErlElement.Kind kind) {
		final List<IErlPreprocessorDef> res = new ArrayList<IErlPreprocessorDef>();
		final List<IErlModule> modulesFound = new ArrayList<IErlModule>(1);
		List<IErlModule> modulesWithIncludes = modulesFound;
		try {
			modulesWithIncludes = getModulesWithIncludes(b, project, module,
					modulesFound);
		} catch (final CoreException e) {
			e.printStackTrace();
		}
		for (final IErlModule m : modulesWithIncludes) {
			res.addAll(m.getPreprocessorDefs(kind));
		}
		return res;
	}

	public static List<IErlImport> getImportsAsList(final IErlModule mod) {
		if (mod == null) {
			return new ArrayList<IErlImport>(0);
		}
		return mod.getImports();
	}

	public static IErlScanner getScanner(final ITextEditor editor) {
		final IErlModule mod = getModule(editor);
		// ErlLogger.debug("getScanner:: " + editor + " = " + mod);
		if (mod != null) {
			return getModule(editor).getScanner();
		}
		return null;
	}

	public static IErlFunction findFunctionFromPos(final IErlModule module,
			final int offset) {
		// TODO Auto-generated method stub
		return null;
	}

	public static IErlPreprocessorDef findPreprocessorDef(final IdeBackend b,
			final IProject project, final IErlModule module,
			final String definedName, final IErlElement.Kind type) {
		try {
			return findPreprocessorDef(b, project, module, definedName, type,
					new ArrayList<IErlModule>());
		} catch (final CoreException e) {
			return null;
		}
	}

	/**
	 * @param project
	 * @param m
	 * @param definedName
	 * @param type
	 * @param modulesDone
	 * @return
	 * @throws CoreException
	 */
	private static IErlPreprocessorDef findPreprocessorDef(final IdeBackend b,
			final IProject project, IErlModule m, final String definedName,
			final IErlElement.Kind type, final List<IErlModule> modulesDone)
			throws CoreException {
		if (m == null) {
			return null;
		}
		modulesDone.add(m);
		m.open(null);
		final IErlPreprocessorDef pd = m.findPreprocessorDef(definedName, type);
		if (pd != null) {
			return pd;
		}
		final List<ErlangIncludeFile> includes = m.getIncludedFiles();
		for (final ErlangIncludeFile element : includes) {
			IResource re = ResourceUtil
					.recursiveFindNamedResourceWithReferences(project, element
							.getFilenameLastPart());
			if (re == null) {
				try {
					String s = element.getFilename();
					if (element.isSystemInclude()) {
						s = ErlideOpen.getIncludeLib(b, s);
					} else {
						s = findIncludeFile(project, s);
					}
					re = EditorUtility.openExternal(s);
				} catch (final Exception e) {
					e.printStackTrace();
				}
			}
			if (re != null && re instanceof IFile) {
				m = getModule((IFile) re);
				if (m != null && !modulesDone.contains(m)) {
					final IErlPreprocessorDef pd2 = findPreprocessorDef(b,
							project, m, definedName, type, modulesDone);
					if (pd2 != null) {
						return pd2;
					}
				}
			}
		}
		return null;
	}

	/**
	 * @param b
	 * @param project
	 * @param m
	 * @param modulesFound
	 * @return
	 * @throws CoreException
	 */
	private static List<IErlModule> getModulesWithIncludes(final IdeBackend b,
			final IProject project, final IErlModule m,
			final List<IErlModule> modulesFound) throws CoreException {
		if (m == null) {
			return null;
		}
		modulesFound.add(m);
		m.open(null);
		final List<ErlangIncludeFile> includes = m.getIncludedFiles();
		for (final ErlangIncludeFile element : includes) {
			IResource re = ResourceUtil
					.recursiveFindNamedResourceWithReferences(project, element
							.getFilenameLastPart());
			if (re == null) {
				try {
					String s = element.getFilename();
					if (element.isSystemInclude()) {
						s = ErlideOpen.getIncludeLib(b, s);
					} else {
						s = findIncludeFile(project, s);
					}
					re = EditorUtility.openExternal(s);
				} catch (final Exception e) {
					e.printStackTrace();
				}
			}
			if (re != null && re instanceof IFile) {
				final IErlModule included = getModule((IFile) re);
				if (included != null && !modulesFound.contains(included)) {
					getModulesWithIncludes(b, project, included, modulesFound);
				}
			}
		}
		return modulesFound;
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
	public static boolean openPreprocessorDef(final IdeBackend b,
			final IProject project, final IWorkbenchPage page, IErlModule m,
			final String definedName, final IErlElement.Kind type,
			final List<IErlModule> modulesDone) throws CoreException,
			ErlModelException, PartInitException {
		if (m == null) {
			return false;
		}
		modulesDone.add(m);
		m.open(null);
		final IErlPreprocessorDef pd = m.findPreprocessorDef(definedName, type);
		if (pd == null) {
			final List<ErlangIncludeFile> includes = m.getIncludedFiles();
			for (final ErlangIncludeFile element : includes) {
				IResource re = ResourceUtil
						.recursiveFindNamedResourceWithReferences(project,
								element.getFilenameLastPart());
				if (re == null) {
					try {
						String s = element.getFilename();
						if (element.isSystemInclude()) {
							s = ErlideOpen.getIncludeLib(b, s);
						} else {
							s = findIncludeFile(project, s);
						}
						re = EditorUtility.openExternal(s);
					} catch (final Exception e) {
						e.printStackTrace();
					}
				}
				if (re != null && re instanceof IFile) {
					m = getModule((IFile) re);
					if (m != null && !modulesDone.contains(m)) {
						if (openPreprocessorDef(b, project, page, m,
								definedName, type, modulesDone)) {
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
	 * @return the path to the include file
	 */
	public static String findIncludeFile(final IProject project,
			final String filePath) {
		final IPathVariableManager pvm = ResourcesPlugin.getWorkspace()
				.getPathVariableManager();
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
	public static void openExternalFunction(final OpenResult res,
			final IProject project) throws CoreException {
		openExternalFunction(res.getName(), res.getFun(), res.getArity(), res
				.getPath(), project);
	}

	public static void openExternalFunction(final String mod, final String fun,
			final int arity, final String path, final IProject project)
			throws CoreException {
		final String modFileName = mod + ".erl";
		IResource r = null;
		if (project != null) {
			r = ResourceUtil.recursiveFindNamedResourceWithReferences(project,
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
	public static boolean openFunctionInEditor(final String fun,
			final int arity, final IEditorPart editor) throws ErlModelException {
		if (editor == null) {
			return false;
		}
		final IErlModule module = getModule(editor.getEditorInput());
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
	public static void openElementInNewEditor(final Object element,
			final boolean activate) throws ErlModelException, PartInitException {
		final IEditorPart part = EditorUtility.openInEditor(element, activate);
		if (element instanceof IErlElement) {
			EditorUtility.revealInEditor(part, (IErlElement) element);
		}
	}

	public static void disposeScanner(final ErlangEditor editor) {
		final IErlModule mod = getModule(editor);
		if (mod != null) {
			getModule(editor).disposeScanner();
		}
	}

	public static void disposeParser(final ErlangEditor editor) {
		final IErlModule mod = getModule(editor);
		if (mod != null) {
			getModule(editor).disposeParser();
		}
	}

	public static void reenableScanner(final ErlangEditor editor) {
		final IErlModule mod = getModule(editor);
		if (mod != null) {
			getModule(editor).reenableScanner();
		}
	}
}
