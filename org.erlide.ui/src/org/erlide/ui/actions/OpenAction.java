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

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchSite;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.erlang.ISourceReference;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.search.OpenResult;
import org.erlide.engine.services.search.OpenService;
import org.erlide.ui.editors.erl.AbstractErlangEditor;
import org.erlide.ui.util.ErlModelUtils;
import org.erlide.util.ErlLogger;

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

    private final OpenUtils helper = new OpenUtils();

    /**
     * Creates a new <code>OpenAction</code>. The action requires that the
     * selection provided by the site's selection provider is of type <code>
     * org.eclipse.jface.viewers.IStructuredSelection</code> .
     *
     * @param site
     *            the site providing context information for this action
     * @param externalModules
     *            the externalModules file that can be searched for references
     *            to external modules
     */
    public OpenAction(final IWorkbenchSite site) {
        super(site);
        setText(ActionMessages.OpenAction_label);
        setToolTipText(ActionMessages.OpenAction_tooltip);
        setDescription(ActionMessages.OpenAction_description);
        PlatformUI.getWorkbench().getHelpSystem().setHelp(this, "erl.open");
    }

    public OpenAction(final AbstractErlangEditor erlangEditor) {
        super(erlangEditor.getSite());
        setText(ActionMessages.OpenAction_open_declaration_label);
        setToolTipText(ActionMessages.OpenAction_tooltip);
        setDescription(ActionMessages.OpenAction_description);
        PlatformUI.getWorkbench().getHelpSystem().setHelp(this, "erl.open");
    }

    @Override
    public void selectionChanged(final ITextSelection selection) {
    }

    @Override
    public void selectionChanged(final IStructuredSelection selection) {
        setEnabled(checkEnabled(selection));
    }

    private boolean checkEnabled(final IStructuredSelection selection) {
        if (selection.isEmpty()) {
            return false;
        }
        for (final Object element : selection.toArray()) {
            if (element instanceof ISourceReference) {
                continue;
            }
            if (element instanceof IFile) {
                continue;
            }
            if (element instanceof IStorage) {
                // We don't handle IStorage, do we?
                continue;
            }
            if (element instanceof IErlModule) {
                continue;
            }
            return false;
        }
        return true;
    }

    @Override
    public void run(final ITextSelection selection) {
        try {
            final IEditorPart activeEditor = getSite().getPage().getActiveEditor();
            final int offset = selection.getOffset();
            ITextEditor textEditor = null;
            OpenResult openResult = null;
            IErlElement element = null;
            IErlProject project = null;
            IErlModule module = null;
            final IErlModel model = ErlangEngine.getInstance().getModel();
            if (activeEditor instanceof AbstractErlangEditor) {
                final AbstractErlangEditor editor = (AbstractErlangEditor) activeEditor;
                textEditor = editor;
                editor.reconcileNow();
                final String scannerName = editor.getScannerName();
                module = editor.getModule();
                project = editor.getProject();
                openResult = ErlangEngine
                        .getInstance()
                        .getService(OpenService.class)
                        .open(scannerName,
                                offset,
                                ErlangEngine.getInstance().getModelUtilService()
                                        .getImportsAsList(module),
                                project.getProperties().getExternalModules(),
                                model.getPathVars());
                ErlLogger.debug("open " + openResult);
                element = editor.getElementAt(offset, true);
            } else if (activeEditor instanceof ITextEditor) {
                textEditor = (ITextEditor) activeEditor;
                final String text = textEditor.getDocumentProvider()
                        .getDocument(textEditor.getEditorInput()).get();
                openResult = ErlangEngine.getInstance().getService(OpenService.class)
                        .openText(text, offset);
                final IFile file = (IFile) textEditor.getEditorInput().getAdapter(
                        IFile.class);
                if (file != null) {
                    final IProject p = file.getProject();
                    if (p != null) {
                        project = model.findProject(p);
                    }
                }
            }
            if (openResult != null) {
                helper.openOpenResult(textEditor, module, offset, project, openResult,
                        element);
            }
        } catch (final Exception e) {
            ErlLogger.error(e);
        }
    }

    @Override
    public void run(final IStructuredSelection selection) {
        if (!checkEnabled(selection)) {
            return;
        }
        for (final Object i : selection.toArray()) {
            if (i instanceof IErlElement) {
                try {
                    ErlModelUtils.openElement((IErlElement) i);
                } catch (final CoreException e) {
                    ErlLogger.error(e);
                }
            }
        }
    }

}
