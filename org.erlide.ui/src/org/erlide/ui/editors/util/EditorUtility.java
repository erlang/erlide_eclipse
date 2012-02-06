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

package org.erlide.ui.editors.util;

import java.io.File;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileInfo;
import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.text.IRegion;
import org.eclipse.swt.SWT;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.ide.FileStoreEditorInput;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.ISourceRange;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlExternal;
import org.erlide.core.model.root.IParent;
import org.erlide.jinterface.ErlLogger;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.internal.ErlideUIPlugin;

import com.google.common.collect.Lists;

/**
 * A number of routines for working with ErlElements in editors.
 * 
 * Use 'isOpenInEditor' to test if an element is already open in a editor Use
 * 'openInEditor' to force opening an element in a editor With 'getWorkingCopy'
 * you get the working copy (element in the editor) of an element
 */
public class EditorUtility {

    public static boolean isEditorInput(final Object element,
            final IEditorPart editor) {
        if (editor != null) {
            return editor.getEditorInput().equals(getEditorInput(element));
        }
        return false;
    }

    /**
     * Tests if a CU is currently shown in an editor
     * 
     * @return the IEditorPart if shown, null if element is not open in an
     *         editor
     */
    public static IEditorPart isOpenInEditor(final Object inputElement) {
        final Collection<IEditorPart> allErlangEditors = EditorUtility
                .getAllErlangEditors();
        for (final IEditorPart editorPart : allErlangEditors) {
            if (inputElement instanceof IErlElement) {
                final IErlElement element = (IErlElement) inputElement;
                final IErlModule module = element.getModule();
                final ErlangEditor editor = (ErlangEditor) editorPart;
                if (module.equals(editor.getModule())) {
                    return editorPart;
                }
            }
        }
        final IEditorInput input = getEditorInput(inputElement);
        if (input != null) {
            for (final IEditorPart editorPart : allErlangEditors) {
                if (editorPart.getEditorInput().equals(input)) {
                    return editorPart;
                }
            }
        }
        return null;
    }

    /**
     * Opens an Erlang editor for an element such as <code>IErlElement</code>,
     * <code>IFile</code>, or <code>IStorage</code>. The editor is activated by
     * default.
     * 
     * @return the IEditorPart or null if wrong element type or opening failed
     */
    public static IEditorPart openInEditor(final Object inputElement)
            throws PartInitException {
        return openInEditor(inputElement, true);
    }

    /**
     * Opens an Erlang editor for an element (IErlElement, IFile, IStorage...)
     * 
     * @return the IEditorPart or null if wrong element type or opening failed
     */
    public static IEditorPart openInEditor(final Object inputElement,
            final boolean activate) throws PartInitException {
        final IEditorInput input = getEditorInput(inputElement);
        if (input == null) {
            return null;
        }
        final IEditorPart editorPart = openInEditor(input,
                getEditorID(input, inputElement), activate);

        if (editorPart != null && inputElement instanceof IErlElement) {
            revealInEditor(editorPart, (IErlElement) inputElement);
            return editorPart;
        }

        if (inputElement instanceof IFile) {
            return openInEditor((IFile) inputElement, activate);
        }

        return null;
    }

    /**
     * Selects a Erlang Element in an editor
     */
    public static boolean revealInEditor(final IEditorPart part,
            final IErlElement element) {
        if (element != null && part instanceof ErlangEditor) {
            ((ErlangEditor) part).setSelection(element);
            return true;
        }
        return false;
    }

    /**
     * Selects and reveals the given region in the given editor part.
     */
    public static void revealInEditor(final IEditorPart part,
            final IRegion region) {
        if (part != null && region != null) {
            revealInEditor(part, region.getOffset(), region.getLength());
        }
    }

    public static void revealInEditor(final IEditorPart part,
            final ISourceRange sourceRange) {
        if (part != null && sourceRange != null) {
            final int offset = sourceRange.getOffset();
            final int length = sourceRange.getLength();
            revealInEditor(part, offset, length);
        }
    }

    /**
     * Selects and reveals the given offset and length in the given editor part.
     */
    public static void revealInEditor(final IEditorPart editor,
            final int offset, final int length) {
        if (editor instanceof ITextEditor) {
            ((ITextEditor) editor).selectAndReveal(offset, length);
            return;
        }
        ErlLogger
                .warn("EditorUtility.revealInEditor should only be called on an ErlangEditor; it was an %s",
                        editor.getClass().getName());
    }

    private static IEditorPart openInEditor(final IFile file,
            final boolean activate) throws PartInitException {
        if (file != null) {
            final IWorkbenchPage p = ErlideUIPlugin.getActivePage();
            if (p != null) {
                final IEditorPart editorPart = IDE
                        .openEditor(p, file, activate);
                return editorPart;
            }
        }
        return null;
    }

    private static IEditorPart openInEditor(final IEditorInput input,
            final String editorID, final boolean activate)
            throws PartInitException {
        if (input != null) {
            final IWorkbenchPage p = ErlideUIPlugin.getActivePage();
            if (p != null) {
                final IEditorPart editorPart = p.openEditor(input, editorID,
                        activate);
                return editorPart;
            }
        }
        return null;
    }

    public static String getEditorID(final IEditorInput input,
            final Object inputObject) {
        IEditorDescriptor editorDescriptor;
        try {
            if (input instanceof IFileEditorInput) {
                editorDescriptor = IDE
                        .getEditorDescriptor(((IFileEditorInput) input)
                                .getFile());
            } else {
                editorDescriptor = IDE.getEditorDescriptor(input.getName());
            }
        } catch (final PartInitException e) {
            return null;
        }

        if (editorDescriptor != null) {
            return editorDescriptor.getId();
        }

        return null;
    }

    private static IEditorInput getEditorInput(IErlElement element) {
        final IResource resource = element.getResource();
        if (resource instanceof IFile) {
            IFile file = (IFile) resource;
            file = resolveFile(file);
            return new FileEditorInput(file);
        }
        String filePath = element.getFilePath();
        while (filePath == null) {
            final IParent parent = element.getParent();
            if (parent instanceof IErlElement) {
                element = (IErlElement) parent;
                filePath = element.getFilePath();
            } else {
                break;
            }
        }
        if (filePath != null) {
            final IPath path = new Path(filePath);
            IFileStore fileStore = EFS.getLocalFileSystem().getStore(
                    path.removeLastSegments(1));
            fileStore = fileStore.getChild(path.lastSegment());
            final IFileInfo fetchInfo = fileStore.fetchInfo();
            if (!fetchInfo.isDirectory() && fetchInfo.exists()) {
                if (element instanceof IErlModule
                        && element.getParent() instanceof IErlExternal) {
                    return new ErlangExternalEditorInput(fileStore,
                            (IErlModule) element);
                }
                return new FileStoreEditorInput(fileStore);
            }
        }
        return null;
    }

    private static IFile resolveFile(final IFile file) {
        IFile result = file;
        if (file.getResourceAttributes().isSymbolicLink()) {
            try {
                final File f = new File(file.getLocation().toString());
                final IFileInfo info = EFS.getFileSystem(EFS.SCHEME_FILE)
                        .fromLocalFile(f).fetchInfo();
                final String target = info
                        .getStringAttribute(EFS.ATTRIBUTE_LINK_TARGET);
                if (target != null) {
                    // FIXME this is wrong in the general case
                    // find the file in the externals!
                    result = (IFile) file.getParent().findMember(target);
                    if (result == null) {
                        result = file;
                    }
                }
            } catch (final Exception e) {
                ErlLogger.warn(e);
            }
        }
        return result;
    }

    public static IEditorInput getEditorInput(final Object input) {
        if (input instanceof IErlElement) {
            return getEditorInput((IErlElement) input);
        }

        if (input instanceof IFile) {
            return new FileEditorInput((IFile) input);
        }

        // if (input instanceof IStorage)
        // return new JarEntryEditorInput((IStorage)input);

        return null;
    }

    /**
     * Opens the editor on the given element and subsequently selects it.
     */
    public static void openElementInEditor(final Object element,
            final boolean activate) throws PartInitException {
        final IEditorPart part = EditorUtility.openInEditor(element, activate);
        if (element instanceof IErlElement) {
            EditorUtility.revealInEditor(part, (IErlElement) element);
        }
    }

    /**
     * If the current active editor edits a erlang element return it, else
     * return null
     */
    public static IErlElement getActiveEditorErlangInput() {
        final IWorkbenchPage page = ErlideUIPlugin.getActivePage();
        if (page != null) {
            final IEditorPart part = page.getActiveEditor();
            if (part != null) {
                final IEditorInput editorInput = part.getEditorInput();
                if (editorInput != null) {
                    return (IErlElement) editorInput
                            .getAdapter(IErlElement.class);
                    // return JavaUI.getEditorInputJavaElement(editorInput);
                }
            }
        }
        return null;
    }

    /**
     * Maps the localized modifier name to a code in the same manner as
     * #findModifier.
     * 
     * @param modifierName
     *            the modifier name
     * @return the SWT modifier bit, or <code>0</code> if no match was found
     * @since 2.1.1
     */
    public static int findLocalizedModifier(final String modifierName) {
        if (modifierName == null) {
            return 0;
        }

        if (modifierName.equalsIgnoreCase(Action.findModifierString(SWT.CTRL))) {
            return SWT.CTRL;
        }
        if (modifierName.equalsIgnoreCase(Action.findModifierString(SWT.SHIFT))) {
            return SWT.SHIFT;
        }
        if (modifierName.equalsIgnoreCase(Action.findModifierString(SWT.ALT))) {
            return SWT.ALT;
        }
        if (modifierName.equalsIgnoreCase(Action
                .findModifierString(SWT.COMMAND))) {
            return SWT.COMMAND;
        }

        return 0;
    }

    public static Collection<IEditorPart> getAllErlangEditors() {
        final List<IEditorPart> result = Lists.newArrayList();
        final IWorkbench workbench = ErlideUIPlugin.getDefault().getWorkbench();
        for (final IWorkbenchWindow i : workbench.getWorkbenchWindows()) {
            for (final IWorkbenchPage j : i.getPages()) {
                for (final IEditorReference editorReference : j
                        .getEditorReferences()) {
                    final IEditorPart editorPart = editorReference
                            .getEditor(false);
                    if (editorPart instanceof ErlangEditor) {
                        result.add(editorPart);
                    }
                }
            }
        }
        return result;
    }

}
