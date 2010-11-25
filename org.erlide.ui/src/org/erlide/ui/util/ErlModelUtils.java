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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.IURIEditorInput;
import org.eclipse.ui.PartInitException;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlElement.Kind;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlImport;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlPreprocessorDef;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IErlTypespec;
import org.erlide.core.erlang.ISourceRange;
import org.erlide.core.erlang.SourceRange;
import org.erlide.core.erlang.util.ContainerFilter;
import org.erlide.core.erlang.util.ErlangFunction;
import org.erlide.core.erlang.util.ErlangIncludeFile;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.core.erlang.util.ModelUtils;
import org.erlide.core.erlang.util.PluginUtils;
import org.erlide.core.erlang.util.ResourceUtil;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.util.EditorUtility;
import org.erlide.ui.editors.util.ErlangExternalEditorInput;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideOpen;

public class ErlModelUtils {

    private static final ArrayList<OtpErlangObject> NO_IMPORTS = new ArrayList<OtpErlangObject>(
            0);

    public static List<IErlPreprocessorDef> getPreprocessorDefs(
            final Backend b, final IProject project, final IErlModule module,
            final IErlElement.Kind kind, final String externalIncludes)
            throws CoreException, BackendException {
        final List<IErlPreprocessorDef> res = new ArrayList<IErlPreprocessorDef>();
        final List<IErlModule> modulesFound = new ArrayList<IErlModule>(1);
        List<IErlModule> modulesWithIncludes = modulesFound;
        modulesWithIncludes = getModulesWithIncludes(b, project, module,
                externalIncludes, modulesFound);
        for (final IErlModule m : modulesWithIncludes) {
            res.addAll(m.getPreprocessorDefs(kind));
        }
        return res;
    }

    public static List<OtpErlangObject> getImportsAsList(final IErlModule mod) {
        if (mod == null) {
            return NO_IMPORTS;
        }
        final Collection<IErlImport> imports = mod.getImports();
        if (imports.isEmpty()) {
            return NO_IMPORTS;
        }
        final List<OtpErlangObject> result = new ArrayList<OtpErlangObject>(
                imports.size());
        for (final IErlImport i : imports) {
            final List<ErlangFunction> functions = i.getFunctions();
            final OtpErlangObject funsT[] = new OtpErlangObject[functions
                    .size()];
            int j = 0;
            for (final ErlangFunction f : functions) {
                funsT[j] = f.getNameArityTuple();
                j++;
            }
            final OtpErlangTuple modFunsT = new OtpErlangTuple(
                    new OtpErlangObject[] {
                            new OtpErlangAtom(i.getImportModule()),
                            new OtpErlangList(funsT) });
            result.add(modFunsT);
        }
        return result;
    }

    public static IErlPreprocessorDef findPreprocessorDef(
            final Backend backend, final IProject project,
            final IErlModule module, final String definedName,
            final IErlElement.Kind kind, final String externalIncludes)
            throws CoreException, BackendException {
        String unquoted = ErlideUtil.unquote(definedName);
        final Set<String> names = new HashSet<String>(3);
        if (kind == Kind.RECORD_DEF) {
            while (names.add(unquoted)) {
                unquoted = resolveMacroValue(unquoted, module);
            }
        } else {
            names.add(unquoted);
        }
        names.add(definedName);
        for (final String name : names) {
            final IErlPreprocessorDef pd = internalFindPreprocessorDef(backend,
                    project, module, name, kind, externalIncludes,
                    new ArrayList<IErlModule>());
            if (pd != null) {
                return pd;
            }
        }
        return null;
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
     * @throws BackendException
     */
    private static IErlPreprocessorDef internalFindPreprocessorDef(
            final Backend b, final IProject project, IErlModule module,
            final String name, final IErlElement.Kind kind,
            final String externalIncludes, final List<IErlModule> modulesDone)
            throws CoreException, BackendException {
        if (module == null) {
            return null;
        }
        modulesDone.add(module);
        module.open(null);
        final IErlPreprocessorDef pd = module.findPreprocessorDef(name, kind);
        if (pd != null) {
            return pd;
        }
        final Collection<ErlangIncludeFile> includes = module
                .getIncludedFiles();
        for (final ErlangIncludeFile element : includes) {
            final IResource resource = module.getResource();
            IResource re = null;
            if (resource != null) {
                re = ResourceUtil.recursiveFindNamedResourceWithReferences(
                        project,
                        element.getFilenameLastPart(),
                        PluginUtils.getIncludePathFilter(project,
                                resource.getParent()));
            }
            if (re instanceof IFile) {
                module = ModelUtils.getModule((IFile) re);
            } else {
                module = getExternalInclude(b, project, externalIncludes,
                        element);
            }
            if (module != null && !modulesDone.contains(module)) {
                final IErlPreprocessorDef pd2 = internalFindPreprocessorDef(b,
                        project, module, name, kind, externalIncludes,
                        modulesDone);
                if (pd2 != null) {
                    return pd2;
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
     * @throws BackendException
     */
    private static List<IErlModule> getModulesWithIncludes(final Backend b,
            final IProject project, final IErlModule m,
            final String externalIncludes, final List<IErlModule> modulesFound)
            throws CoreException, BackendException {
        if (m == null) {
            return null;
        }
        modulesFound.add(m);
        m.open(null);
        final Collection<ErlangIncludeFile> includes = m.getIncludedFiles();
        for (final ErlangIncludeFile element : includes) {
            final IResource re = ResourceUtil
                    .recursiveFindNamedResourceWithReferences(project, element
                            .getFilenameLastPart(), PluginUtils
                            .getIncludePathFilter(project, m.getResource()
                                    .getParent()));
            final IErlModule included;
            if (re instanceof IFile) {
                included = ModelUtils.getModule((IFile) re);
            } else {
                included = getExternalInclude(b, project, externalIncludes,
                        element);
            }
            if (included != null && !modulesFound.contains(included)) {
                getModulesWithIncludes(b, project, included, externalIncludes,
                        modulesFound);
            }
        }
        return modulesFound;
    }

    public static boolean openPreprocessorDef(final Backend backend,
            final IProject project, final IErlModule module, final String name,
            final IErlElement.Kind kind, final String externalIncludes)
            throws PartInitException, ErlModelException, CoreException,
            BackendException {
        return internalOpenPreprocessorDef(backend, project, module, name,
                kind, externalIncludes, new ArrayList<IErlModule>());
    }

    /**
     * @param backend
     * @param project
     * @param module
     * @param definedName
     * @param type
     * @param findPreprocessorDef
     *            TODO
     * @throws CoreException
     * @throws ErlModelException
     * @throws PartInitException
     * @throws BackendException
     */
    private static boolean internalOpenPreprocessorDef(final Backend backend,
            final IProject project, final IErlModule module,
            final String definedName, final IErlElement.Kind type,
            final String externalIncludes, final List<IErlModule> modulesDone)
            throws CoreException, ErlModelException, PartInitException,
            BackendException {
        if (module == null) {
            return false;
        }
        modulesDone.add(module);
        module.open(null);
        final IErlPreprocessorDef pd = findPreprocessorDef(backend, project,
                module, definedName, type, externalIncludes);
        if (pd == null) {
            final Collection<ErlangIncludeFile> includes = module
                    .getIncludedFiles();
            for (final ErlangIncludeFile element : includes) {
                final String filenameLastPart = element.getFilenameLastPart();
                final IResource resource = module.getResource();
                final IContainer parent = resource.getParent();
                final ContainerFilter includePathFilter = PluginUtils
                        .getIncludePathFilter(project, parent);
                final IResource re = ResourceUtil
                        .recursiveFindNamedResourceWithReferences(project,
                                filenameLastPart, includePathFilter);
                final IErlModule m2;
                if (re instanceof IFile) {
                    m2 = ModelUtils.getModule((IFile) re);
                } else {
                    m2 = getExternalInclude(backend, project, externalIncludes,
                            element);
                }
                if (m2 != null && !modulesDone.contains(m2)) {
                    if (internalOpenPreprocessorDef(backend, project, m2,
                            definedName, type, externalIncludes, modulesDone)) {
                        return true;
                    }
                }
            }
        }
        if (pd != null) {
            final IEditorPart editor = EditorUtility.openInEditor(pd
                    .getModule());
            EditorUtility.revealInEditor(editor, pd);
            return true;
        }
        return false;
    }

    private static IErlModule getExternalInclude(final Backend backend,
            final IProject project, final String externalIncludes,
            final ErlangIncludeFile element) throws BackendException,
            CoreException {
        String s = element.getFilename();
        if (element.isSystemInclude()) {
            s = ErlideOpen.getIncludeLib(backend, s);
        } else {
            s = ModelUtils.findIncludeFile(project, s, externalIncludes);
        }
        final IErlModule module = ModelUtils.openExternal(project, s);
        return module;
    }

    public static String resolveMacroValue(final String definedName,
            final IErlModule m) {
        if ("?MODULE".equals(definedName)) {
            return m.getModuleName();
        }
        final IErlPreprocessorDef def = m.findPreprocessorDef(
                withoutInterrogationMark(definedName), Kind.MACRO_DEF);
        if (def != null) {
            final String extra = def.getExtra();
            final int p = extra.indexOf(',');
            if (p != -1) {
                final String s = extra.substring(p + 1).trim();
                if (s.length() > 0) {
                    return s;
                }
            }
        }
        return definedName;
    }

    private static String withoutInterrogationMark(final String definedName) {
        if (definedName.startsWith("?")) {
            return definedName.substring(1);
        }
        return definedName;
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
     * @param checkAllProjects
     *            if true, check all projects in workspace, otherwise only
     *            consider projects referred from project
     * @throws CoreException
     */
    public static boolean openExternalFunction(final String mod,
            final ErlangFunction function, final String path,
            final IErlModule module, final IProject project,
            final boolean checkAllProjects) throws CoreException {
        final IErlModule module2 = findExternalModule(mod, path, project,
                checkAllProjects);
        if (module2 != null) {
            final IEditorPart editor = EditorUtility.openInEditor(module2);
            return openFunctionInEditor(function, editor);
        }
        return false;
    }

    public static IErlElement findExternalFunction(String moduleName,
            final ErlangFunction erlangFunction, final String modulePath,
            final IProject project, final boolean checkAllProjects,
            final IErlModule module) {
        try {
            moduleName = resolveMacroValue(moduleName, module);
            final IErlModule module2 = findExternalModule(moduleName,
                    modulePath, project, checkAllProjects);
            if (module2 != null) {
                module2.open(null);
                return module2.findFunction(erlangFunction);
            }
        } catch (final ErlModelException e) {
        } catch (final CoreException e) {
            e.printStackTrace();
        }
        return null;
    }

    public static void openElement(final IErlElement element)
            throws PartInitException, ErlModelException {
        final IEditorPart editor = EditorUtility.openInEditor(element);
        EditorUtility.revealInEditor(editor, element);
    }

    public static void openSourceRange(final IErlModule module,
            final ISourceRange sourceRange) throws PartInitException,
            ErlModelException {
        final IEditorPart editor = EditorUtility.openInEditor(module);
        EditorUtility.revealInEditor(editor, sourceRange);
    }

    public static IErlElement findExternalType(final IErlModule module,
            String moduleName, final String typeName, final String modulePath,
            final IProject project, final boolean checkAllProjects) {
        try {
            moduleName = resolveMacroValue(moduleName, module);
            final IErlModule module2 = findExternalModule(moduleName,
                    modulePath, project, checkAllProjects);
            if (module2 != null) {
                module2.open(null);
                return module2.findTypespec(typeName);
            }
        } catch (final ErlModelException e) {
        } catch (final CoreException e) {
            e.printStackTrace();
        }
        return null;
    }

    public static IErlModule findExternalModule(final String moduleName,
            final String modulePath, final IProject project,
            final boolean checkAllProjects) throws CoreException {
        final String modFileName = moduleName + ".erl";
        IResource r = null;
        if (project != null) {
            r = ResourceUtil.recursiveFindNamedResourceWithReferences(project,
                    modFileName, PluginUtils.getSourcePathFilter(project));

            if (r == null) {
                final IErlModule module = ModelUtils.openExternal(project,
                        modulePath);
                if (module != null) {
                    return module;
                    // if (r != null &&
                    // !PluginUtils.isOnSourcePath(r.getParent())) {
                    // r = null;
                    // }
                }
            }
        }
        if (r == null) {
            ErlLogger.debug(
                    "findExternalModule not found yet, checkAllProjects %b",
                    checkAllProjects);
        }
        if (r == null && checkAllProjects) {
            final IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace()
                    .getRoot();
            final IProject[] projects = workspaceRoot.getProjects();
            for (final IProject p : projects) {
                if (ErlideUtil.hasErlangNature(p)) {
                    ErlLogger.debug("searching project %s", p.getName());
                    r = ResourceUtil.recursiveFindNamedResource(p, modFileName,
                            PluginUtils.getSourcePathFilter(p));
                    if (r != null) {
                        ErlLogger.debug("found %s", r);
                        break;
                    }
                }
            }
        }
        if (r instanceof IFile) {
            return ErlangCore.getModel().findModule((IFile) r);
        }
        return null;
    }

    public static IErlModule getExternalModule(final String mod,
            final String externalModules) throws CoreException {
        final String path = ErlideOpen.getExternalModule(ErlangCore
                .getBackendManager().getIdeBackend(), mod, externalModules,
                ErlangCore.getModel().getPathVars());
        if (path != null) {
            ModelUtils.openExternal(null, path);
        }
        return null;
    }

    /**
     * Activate editor and select erlang function
     * 
     * @param fun
     * @param arity
     * @param editor
     * @throws CoreException
     */
    public static boolean openFunctionInEditor(
            final ErlangFunction erlangFunction, final IEditorPart editor)
            throws CoreException {
        final ErlangEditor erlangEditor = (ErlangEditor) editor;
        final IErlModule module = erlangEditor.getModule();
        if (module == null) {
            return false;
        }
        module.open(null);
        final IErlFunction function = ModelUtils.findFunction(module,
                erlangFunction);
        if (function == null) {
            return false;
        }
        EditorUtility.revealInEditor(editor, function);
        return true;
    }

    public static boolean openTypeInEditor(final String typeName,
            final IEditorPart editor) throws CoreException {
        final ErlangEditor erlangEditor = (ErlangEditor) editor;
        final IErlModule module = erlangEditor.getModule();
        if (module == null) {
            return false;
        }
        module.open(null);
        final IErlTypespec typespec = ModelUtils.findTypespec(module, typeName);
        if (typespec == null) {
            return false;
        }
        EditorUtility.revealInEditor(editor, typespec);
        return true;
    }

    public static List<IErlModule> getModulesWithReferencedProjects(
            final IErlProject project) throws CoreException {
        final IErlModel model = ErlangCore.getModel();
        final List<IErlModule> result = new ArrayList<IErlModule>();
        if (project == null) {
            return result;
        }
        project.open(null);
        result.addAll(project.getModules());
        for (final IProject p : project.getProject().getReferencedProjects()) {
            final IErlProject ep = model.findProject(p);
            if (ep != null) {
                ep.open(null);
                result.addAll(ep.getModules());
            }
        }
        return result;
    }

    public static IErlModule getModule(final IEditorInput editorInput) {
        if (editorInput instanceof IFileEditorInput) {
            final IFileEditorInput input = (IFileEditorInput) editorInput;
            return ModelUtils.getModule(input.getFile());
        }
        if (editorInput instanceof ErlangExternalEditorInput) {
            final ErlangExternalEditorInput erlangExternalEditorInput = (ErlangExternalEditorInput) editorInput;
            return erlangExternalEditorInput.getModule();
        }
        String path = null;
        if (editorInput instanceof IStorageEditorInput) {
            final IStorageEditorInput sei = (IStorageEditorInput) editorInput;
            try {
                final IPath p = sei.getStorage().getFullPath();
                path = p.toPortableString();
            } catch (final CoreException e) {
            }
        }
        if (editorInput instanceof IURIEditorInput) {
            final IURIEditorInput ue = (IURIEditorInput) editorInput;
            path = ue.getURI().getPath();
        }
        if (path != null) {
            return ModelUtils.findExternalModuleFromPath(path);
        }
        return null;
    }

    public static String[] getPredefinedMacroNames() {
        return new String[] { "MODULE", "LINE", "FILE" };
    }

    public static void openMFA(final String module, final String function,
            final int arity) throws CoreException {
        ErlModelUtils.openExternalFunction(module, new ErlangFunction(function,
                arity), null, ErlangCore.getModel().findModule(module), null,
                true);
    }

    public static void openMF(final String module, final String function)
            throws CoreException {
        openMFA(module, function, ErlangFunction.ANY_ARITY);
    }

    public static void openModule(final String moduleName) throws CoreException {
        final IErlModule module = findExternalModule(moduleName, null, null,
                true);
        if (module != null) {
            EditorUtility.openInEditor(module);
        }
    }

    public static ISourceRange findVariable(final Backend backend,
            final ISourceRange range, final String variableName,
            final String elementText) throws ErlModelException,
            BadLocationException, OtpErlangRangeException {
        final OtpErlangTuple res2 = ErlideOpen.findFirstVar(backend,
                variableName, elementText);
        if (res2 != null) {
            final int relativePos = ((OtpErlangLong) res2.elementAt(0))
                    .intValue() - 1;
            final int length = ((OtpErlangLong) res2.elementAt(1)).intValue();
            final int start = relativePos + range.getOffset();
            return new SourceRange(start, length);
        }
        return range;
    }

    public static boolean isTypeDefOrRecordDef(final IErlElement element) {
        return element != null
                && (element.getKind() == IErlElement.Kind.TYPESPEC || element
                        .getKind() == IErlElement.Kind.RECORD_DEF);
    }

}
