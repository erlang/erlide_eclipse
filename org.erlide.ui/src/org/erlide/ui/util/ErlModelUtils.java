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

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.IURIEditorInput;
import org.eclipse.ui.PartInitException;
import org.erlide.core.ErlangCore;
import org.erlide.core.backend.BackendException;
import org.erlide.core.model.erlang.IErlElement;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.IErlProject;
import org.erlide.core.model.erlang.IErlProject.Scope;
import org.erlide.core.model.erlang.IErlTypespec;
import org.erlide.core.model.erlang.ISourceRange;
import org.erlide.core.model.erlang.util.ErlangFunction;
import org.erlide.core.model.erlang.util.ModelUtils;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.util.EditorUtility;
import org.erlide.ui.editors.util.ErlangExternalEditorInput;

public class ErlModelUtils {

    /**
     * @param moduleName
     * @param function
     * @param modulePath
     * @param module
     * @param project
     * @param scope
     * @return
     * @throws CoreException
     */
    public static boolean openExternalFunction(final String moduleName,
            final ErlangFunction function, final String modulePath,
            final IErlModule module, final IErlProject project,
            final Scope scope) throws CoreException {
        final IErlModule module2 = ModelUtils.findModule(project, moduleName,
                modulePath, scope);
        if (module2 != null) {
            final IEditorPart editor = EditorUtility.openInEditor(module2);
            return openFunctionInEditor(function, editor);
        }
        return false;
    }

    public static void openElement(final IErlElement element)
            throws PartInitException {
        final IEditorPart editor = EditorUtility.openInEditor(element);
        EditorUtility.revealInEditor(editor, element);
    }

    public static void openSourceRange(final IErlModule module,
            final ISourceRange sourceRange) throws PartInitException {
        final IEditorPart editor = EditorUtility.openInEditor(module);
        EditorUtility.revealInEditor(editor, sourceRange);
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
        final IErlFunction function = module.findFunction(erlangFunction);
        if (function == null) {
            return false;
        }
        EditorUtility.revealInEditor(editor, function);
        return true;
    }

    public static boolean openTypeInEditor(final String typeName,
            final IEditorPart editor) throws CoreException, BackendException {
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

    public static IErlModule getModule(final IEditorInput editorInput)
            throws CoreException {
        if (editorInput instanceof IFileEditorInput) {
            final IFileEditorInput input = (IFileEditorInput) editorInput;
            final IFile file = input.getFile();
            IErlModule module = ErlangCore.getModel().findModule(file);
            if (module != null) {
                return module;
            }
            final String path = file.getLocation().toPortableString();
            module = ErlangCore.getModelManager().getModuleFromFile(
                    ErlangCore.getModel(), file.getName(), null, path, path);
            module.setResource(file);
            return module;
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
            final IErlModule module = ModelUtils.findModule(null, null, path,
                    Scope.ALL_PROJECTS);
            if (module != null) {
                return module;
            }
        }
        return null;
    }

    public static void openMFA(final String module, final String function,
            final int arity) throws CoreException {
        ErlModelUtils.openExternalFunction(module, new ErlangFunction(function,
                arity), null, ErlangCore.getModel().findModule(module), null,
                Scope.ALL_PROJECTS);
    }

    public static void openMF(final String module, final String function)
            throws CoreException {
        openMFA(module, function, ErlangFunction.ANY_ARITY);
    }

    public static void openModule(final String moduleName) throws CoreException {
        final IErlModule module = ModelUtils.findModule(null, moduleName, null,
                Scope.ALL_PROJECTS);
        if (module != null) {
            EditorUtility.openInEditor(module);
        }
    }

}
