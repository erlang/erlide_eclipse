/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others. All rights reserved. This program and
 * the accompanying materials are made available under the terms of the Eclipse Public
 * License v1.0 which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors: Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.util;

import java.nio.charset.Charset;

import org.eclipse.core.resources.IEncodedStorage;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IStorage;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.IURIEditorInput;
import org.eclipse.ui.PartInitException;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.IErlElement;
import org.erlide.engine.model.erlang.ErlangFunction;
import org.erlide.engine.model.erlang.IErlFunction;
import org.erlide.engine.model.erlang.ISourceRange;
import org.erlide.engine.model.root.IErlElementLocator;
import org.erlide.engine.model.root.IErlModel;
import org.erlide.engine.model.root.IErlModule;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.ui.editors.erl.AbstractErlangEditor;
import org.erlide.ui.editors.util.EditorUtility;
import org.erlide.ui.editors.util.ErlangExternalEditorInput;

import com.google.common.base.Charsets;

public class ErlModelUtils {

    private ErlModelUtils() {
    }

    public static boolean openExternalFunction(final String moduleName,
            final ErlangFunction function, final String modulePath,
            final IErlModule module, final IErlProject project,
            final IErlElementLocator.Scope scope) throws CoreException {
        final IErlElementLocator model = ErlangEngine.getInstance().getModel();
        final IErlModule module2 = ErlangEngine.getInstance().getModelFindService()
                .findModule(model, project, moduleName, modulePath, scope);
        if (module2 != null) {
            final IEditorPart editor = EditorUtility.openInEditor(module2);
            return ErlModelUtils.openFunctionInEditor(function, editor);
        }
        return false;
    }

    public static void openElement(final IErlElement element) throws PartInitException {
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
     */
    public static boolean openFunctionInEditor(final ErlangFunction erlangFunction,
            final IEditorPart editor) throws CoreException {
        final AbstractErlangEditor erlangEditor = (AbstractErlangEditor) editor;
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

    public static IErlModule getModule(final IEditorInput editorInput)
            throws CoreException {
        if (editorInput instanceof IFileEditorInput) {
            final IFileEditorInput input = (IFileEditorInput) editorInput;
            final IFile file = input.getFile();
            final IErlModel model = ErlangEngine.getInstance().getModel();
            IErlModule module = model.findModule(file);
            if (module != null) {
                return module;
            }
            final IPath path = file.getLocation();
            Charset encoding;
            try {
                encoding = Charset.forName(file.getCharset());
            } catch (final Exception e) {
                encoding = Charsets.UTF_8;
            }
            module = model.getModuleFromFile(model, file.getName(), path, encoding);
            module.setResource(file);
            return module;
        }
        if (editorInput instanceof ErlangExternalEditorInput) {
            final ErlangExternalEditorInput erlangExternalEditorInput = (ErlangExternalEditorInput) editorInput;
            return erlangExternalEditorInput.getModule();
        }
        final String path = ErlModelUtils.getPathForInput(editorInput);
        if (path == null) {
            return null;
        }
        final IErlElementLocator model = ErlangEngine.getInstance().getModel();
        final IErlModule module = ErlangEngine.getInstance().getModelFindService()
                .findModule(model, null, null, path,
                        IErlElementLocator.Scope.ALL_PROJECTS);
        if (module != null) {
            return module;
        }
        final Charset encoding = ErlModelUtils.getEncodingForInput(editorInput);
        final IPath p = new Path(path);
        return ErlangEngine.getInstance().getModel().getModuleFromFile(null,
                p.lastSegment(), p, encoding);

    }

    private static String getPathForInput(final IEditorInput editorInput) {
        if (editorInput instanceof IStorageEditorInput) {
            final IStorageEditorInput sei = (IStorageEditorInput) editorInput;
            try {
                final IStorage storage = sei.getStorage();
                final IPath p = storage.getFullPath();
                return p.toPortableString();
            } catch (final CoreException e) {
            }
        }
        if (editorInput instanceof IURIEditorInput) {
            final IURIEditorInput ue = (IURIEditorInput) editorInput;
            return ue.getURI().getPath();
        }
        return null;
    }

    private static Charset getEncodingForInput(final IEditorInput editorInput) {
        if (editorInput instanceof IStorageEditorInput) {
            final IStorageEditorInput sei = (IStorageEditorInput) editorInput;
            try {
                final IStorage storage = sei.getStorage();
                if (storage instanceof IEncodedStorage) {
                    final IEncodedStorage encodedStorage = (IEncodedStorage) storage;
                    try {
                        return Charset.forName(encodedStorage.getCharset());
                    } catch (final Exception e) {
                        return Charsets.UTF_8;
                    }
                }
            } catch (final CoreException e) {
            }
        }
        try {
            return Charset.forName(ResourcesPlugin.getEncoding());
        } catch (final Exception e) {
            return Charsets.UTF_8;
        }
    }

    public static void openMFA(final String module, final String function,
            final int arity) throws CoreException {
        ErlModelUtils.openExternalFunction(module, new ErlangFunction(function, arity),
                null, ErlangEngine.getInstance().getModel().findModule(module), null,
                IErlElementLocator.Scope.ALL_PROJECTS);
    }

    public static void openMF(final String module, final String function)
            throws CoreException {
        ErlModelUtils.openMFA(module, function, ErlangFunction.ANY_ARITY);
    }

    public static void openModule(final String moduleName) throws CoreException {
        final IErlElementLocator model = ErlangEngine.getInstance().getModel();
        final IErlModule module = ErlangEngine.getInstance().getModelFindService()
                .findModule(model, null, moduleName, null,
                        IErlElementLocator.Scope.ALL_PROJECTS);
        if (module != null) {
            EditorUtility.openInEditor(module);
        }
    }

}
