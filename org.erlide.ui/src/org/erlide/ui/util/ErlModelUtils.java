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
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.IURIEditorInput;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditor;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.core.ErlangProjectProperties;
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
import org.erlide.core.erlang.IErlTypespec;
import org.erlide.core.util.ErlangFunction;
import org.erlide.core.util.ErlangIncludeFile;
import org.erlide.core.util.ResourceUtil;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.Backend;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.util.EditorUtility;

import erlang.ErlideOpen;

public class ErlModelUtils {

	public static IErlModule getModule(final IEditorPart editor) {
		if (editor == null || !(editor instanceof AbstractDecoratedTextEditor)) {
			return null;
		}
		final AbstractDecoratedTextEditor adte = (AbstractDecoratedTextEditor) editor;
		return getModule(editor.getEditorInput(), adte.getDocumentProvider());
	}

	public static IErlModule getModule(final IFile file) {
		final IErlModel model = ErlangCore.getModel();
		try {
			model.open(null);
			return model.getModule(file);
		} catch (final ErlModelException e) {
		}
		return null;
	}

	public static IErlModule getModule(final String moduleName) {
		final IErlModel model = ErlangCore.getModel();
		try {
			model.open(null);
			return model.getModule(moduleName);
		} catch (final ErlModelException e) {
		}
		return null;
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
			final ErlangFunction erlangFunction) throws ErlModelException {
		final List<? extends IErlElement> children = module.getChildren();
		for (final IErlElement element : children) {
			if (element instanceof IErlFunction) {
				final IErlFunction f = (IErlFunction) element;
				if (f.getFunction().equals(erlangFunction)) {
					return f;
				}
			}
		}
		return null;
	}

	public static IErlTypespec findTypespec(final IErlModule module,
			final String name) throws ErlModelException {
		final List<? extends IErlElement> children = module.getChildren();
		for (final IErlElement element : children) {
			if (element instanceof IErlTypespec) {
				final IErlTypespec t = (IErlTypespec) element;
				if (t.getName().equals(name)) {
					return t;
				}
			}
		}
		return null;
	}

	public static List<IErlPreprocessorDef> getPreprocessorDefs(
			final Backend b, final IProject project, final IErlModule module,
			final IErlElement.Kind kind, final String externalIncludes) {
		final List<IErlPreprocessorDef> res = new ArrayList<IErlPreprocessorDef>();
		final List<IErlModule> modulesFound = new ArrayList<IErlModule>(1);
		List<IErlModule> modulesWithIncludes = modulesFound;
		try {
			modulesWithIncludes = getModulesWithIncludes(b, project, module,
					externalIncludes, modulesFound);
		} catch (final CoreException e) {
			ErlLogger.warn(e);
		}
		for (final IErlModule m : modulesWithIncludes) {
			res.addAll(m.getPreprocessorDefs(kind));
		}
		return res;
	}

	public static Collection<IErlImport> getImportsAsList(final IErlModule mod) {
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

	public static IErlPreprocessorDef findPreprocessorDef(final Backend b,
			final IProject project, final IErlModule module,
			final String definedName, final IErlElement.Kind type,
			final String externalIncludes) {
		try {
			return findPreprocessorDef(b, project, module, definedName, type,
					externalIncludes, new ArrayList<IErlModule>());
		} catch (final CoreException e) {
			return null;
		}
	}

	/**
	 * @param project
	 * @param m
	 * @param definedName
	 * @param type
	 * @param externalIncludes
	 *            TODO
	 * @param modulesDone
	 * @return
	 * @throws CoreException
	 */
	private static IErlPreprocessorDef findPreprocessorDef(final Backend b,
			final IProject project, IErlModule m, final String definedName,
			final IErlElement.Kind type, final String externalIncludes,
			final List<IErlModule> modulesDone) throws CoreException {
		if (m == null) {
			return null;
		}
		modulesDone.add(m);
		m.open(null);
		final IErlPreprocessorDef pd = m.findPreprocessorDef(definedName, type);
		if (pd != null) {
			return pd;
		}
		final Collection<ErlangIncludeFile> includes = m.getIncludedFiles();
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
						s = findIncludeFile(project, s, externalIncludes);
					}
					re = EditorUtility.openExternal(s);
				} catch (final Exception e) {
					ErlLogger.warn(e);
				}
			}
			if (re != null && re instanceof IFile) {
				m = getModule((IFile) re);
				if (m != null && !modulesDone.contains(m)) {
					final IErlPreprocessorDef pd2 = findPreprocessorDef(b,
							project, m, definedName, type, externalIncludes,
							modulesDone);
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
	private static List<IErlModule> getModulesWithIncludes(final Backend b,
			final IProject project, final IErlModule m,
			final String externalIncludes, final List<IErlModule> modulesFound)
			throws CoreException {
		if (m == null) {
			return null;
		}
		modulesFound.add(m);
		m.open(null);
		final Collection<ErlangIncludeFile> includes = m.getIncludedFiles();
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
						s = findIncludeFile(project, s, externalIncludes);
					}
					re = EditorUtility.openExternal(s);
				} catch (final Exception e) {
					ErlLogger.warn(e);
				}
			}
			if (re != null && re instanceof IFile) {
				final IErlModule included = getModule((IFile) re);
				if (included != null && !modulesFound.contains(included)) {
					getModulesWithIncludes(b, project, included,
							externalIncludes, modulesFound);
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
	public static boolean openPreprocessorDef(final Backend b,
			final IProject project, final IWorkbenchPage page, IErlModule m,
			final String definedName, final IErlElement.Kind type,
			final String externalIncludes, final List<IErlModule> modulesDone)
			throws CoreException, ErlModelException, PartInitException {
		if (m == null) {
			return false;
		}
		modulesDone.add(m);
		m.open(null);
		final IErlPreprocessorDef pd = m.findPreprocessorDef(definedName, type);
		if (pd == null) {
			final Collection<ErlangIncludeFile> includes = m.getIncludedFiles();
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
							s = findIncludeFile(project, s, externalIncludes);
						}
						re = EditorUtility.openExternal(s);
					} catch (final Exception e) {
						ErlLogger.warn(e);
					}
				}
				if (re != null && re instanceof IFile) {
					m = getModule((IFile) re);
					if (m != null && !modulesDone.contains(m)) {
						if (openPreprocessorDef(b, project, page, m,
								definedName, type, externalIncludes,
								modulesDone)) {
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
	 * @param externalIncludes
	 *            TODO
	 * @return the path to the include file
	 */
	public static String findIncludeFile(final IProject project,
			final String filePath, final String externalIncludes) {
		if (project == null) {
			return filePath;
		}
		final IPathVariableManager pvm = ResourcesPlugin.getWorkspace()
				.getPathVariableManager();
		final ErlangProjectProperties prefs = ErlangCore
				.getProjectProperties(project);
		for (final String includeDir : prefs.getIncludeDirs()) {
			IPath p = new Path(includeDir).append(filePath);
			p = pvm.resolvePath(p);
			final File f = new File(p.toOSString());
			if (f.exists()) {
				return p.toString();
			}
		}
		final String s = ErlideOpen.getExternalInclude(ErlangCore
				.getBackendManager().getIdeBackend(), filePath,
				externalIncludes, ErlangCore.getModel()
						.getPathVars());
		if (s != null) {
			return s;
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
	public static void openExternalFunction(final String mod,
			final ErlangFunction function, final String path,
			final IProject project) throws CoreException {
		final IResource r = openExternalModule(mod, path, project);
		if (r != null && r instanceof IFile) {
			final IFile f = (IFile) r;
			try {
				final IEditorPart editor = EditorUtility.openInEditor(f);
				openFunctionInEditor(function, editor);
			} catch (final PartInitException e) {
				ErlLogger.warn(e);
			} catch (final ErlModelException e) {
				ErlLogger.warn(e);
			}
		}
	}

	public static IResource openExternalModule(final String mod,
			final String path, final IProject project) throws CoreException {
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
				ErlLogger.warn(e);
			}
		}
		return r;
	}

	public static IErlModule getExternalModule(final String mod,
			final String externalModules) throws CoreException {
		final String path = ErlideOpen.getExternalModule(ErlangCore
				.getBackendManager().getIdeBackend(), mod, externalModules,
				ErlangCore.getModel().getPathVars());
		if (path != null) {
			final IProject p = ResourceUtil.getExternalFilesProject();
			if (p != null) {
				final IFile f = EditorUtility.openExternal(path);
				return getModule(f);
			}
		}
		return null;
	}

	/**
	 * Activate editor and select erlang function
	 * 
	 * @param fun
	 * @param arity
	 * @param editor
	 * @throws ErlModelException
	 */
	public static boolean openFunctionInEditor(
			final ErlangFunction erlangFunction, final IEditorPart editor)
			throws ErlModelException {
		final IErlModule module = getModule(editor);
		if (module == null) {
			return false;
		}
		module.open(null);
		final IErlFunction function = findFunction(module, erlangFunction);
		if (function == null) {
			return false;
		}
		EditorUtility.revealInEditor(editor, function);
		return true;
	}

	public static boolean openTypeInEditor(final String typeName,
			final IEditorPart editor) throws ErlModelException {
		final IErlModule module = getModule(editor);
		if (module == null) {
			return false;
		}
		module.open(null);
		final IErlTypespec typespec = findTypespec(module, typeName);
		if (typespec == null) {
			return false;
		}
		EditorUtility.revealInEditor(editor, typespec);
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
			mod.disposeScanner();
		}
	}

	public static void disposeParser(final ErlangEditor editor) {
		final IErlModule mod = getModule(editor);
		if (mod != null) {
			mod.disposeParser();
		}
	}

	public static void disposeModule(final ErlangEditor editor) {
		final IErlModule mod = getModule(editor);
		if (mod != null) {
			mod.dispose();
		}
	}

	public static void reenableScanner(final ErlangEditor editor) {
		final IErlModule mod = getModule(editor);
		if (mod != null) {
			mod.reenableScanner();
		}
	}

	public static List<IErlModule> getModulesWithReferencedProjects(
			final IErlProject project) {
		final IErlModel model = ErlangCore.getModel();
		final List<IErlModule> result = new ArrayList<IErlModule>();
		if (project == null) {
			return result;
		}
		try {
			project.open(null);
			result.addAll(project.getModules());
			for (final IProject p : project.getProject()
					.getReferencedProjects()) {
				final IErlProject ep = model.findProject(p);
				if (ep != null) {
					ep.open(null);
					result.addAll(ep.getModules());
				}
			}
		} catch (final ErlModelException e) {
			ErlLogger.error(e);
		} catch (final CoreException e) {
			ErlLogger.error(e);
		}
		return result;
	}

	public static List<String> getExternalModules(final Backend b,
			final String prefix, final String externalModules) {
		return ErlideOpen.getExternalModules(b, prefix, externalModules,
				ErlangCore.getModel().getPathVars());
	}

	// FIXME: move this to a separate class, that somehow listens to something
	// so that the map is not filled with old disposed stuff

	public static IErlModule getModule(final IEditorInput editorInput,
			final IDocumentProvider documentProvider) {
		if (editorInput == null) {
			return null;
		}
		if (editorInput instanceof IFileEditorInput) {
			final IFileEditorInput input = (IFileEditorInput) editorInput;
			return getModule(input.getFile());
		}
		if (editorInput instanceof IStorageEditorInput) {
			final IStorageEditorInput sei = (IStorageEditorInput) editorInput;
			final IDocument doc = documentProvider.getDocument(editorInput);
			try {
				final IPath p = sei.getStorage().getFullPath();
				final String path = p == null ? "" : p.toString();
				return ErlangCore.getModelManager().getModuleFromFile(
						editorInput.getName(), doc.get(), path, editorInput);
			} catch (final CoreException e) {
				ErlLogger.warn(e);
			}
		}
		if (editorInput instanceof IURIEditorInput) {
			final IURIEditorInput ue = (IURIEditorInput) editorInput;
			final String path = ue.getURI().getPath();
			final IDocument doc = documentProvider.getDocument(editorInput);
			return ErlangCore.getModelManager().getModuleFromFile(
					editorInput.getName(), doc.get(), path, editorInput);
		}
		return null;
	}

	public static IErlElement getEditorInputErlElement(final IEditorInput input) {
		final IWorkbench workbench = ErlideUIPlugin.getDefault().getWorkbench();
		for (final IWorkbenchWindow workbenchWindow : workbench
				.getWorkbenchWindows()) {
			final IWorkbenchPage page = workbenchWindow.getActivePage();
			if (page != null) {
				final IEditorPart part = page.getActiveEditor();
				if (part != null) {
					if (part instanceof AbstractDecoratedTextEditor) {
						final AbstractDecoratedTextEditor adte = (AbstractDecoratedTextEditor) part;
						final IErlModule module = getModule(input, adte
								.getDocumentProvider());
						if (module != null) {
							return module;
						}
					}
				}
			}
		}
		return null;
	}

}
