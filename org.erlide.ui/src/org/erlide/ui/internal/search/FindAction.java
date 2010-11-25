/*******************************************************************************
 * Copyright (c) 2000, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.internal.search;

import java.util.Collection;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchSite;
import org.eclipse.ui.IWorkingSet;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.progress.IProgressService;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlAttribute;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunctionClause;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlPreprocessorDef;
import org.erlide.core.text.ErlangToolkit;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.ui.actions.SelectionDispatchAction;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.internal.ExceptionHandler;
import org.erlide.ui.util.ErlModelUtils;

import erlang.ErlangSearchPattern;
import erlang.ErlangSearchPattern.LimitTo;
import erlang.ErlideOpen;
import erlang.OpenResult;

/**
 * Abstract class for Java search actions.
 * <p>
 * Note: This class is for internal use only. Clients should not use this class.
 * </p>
 * 
 * @since 2.0
 */
public abstract class FindAction extends SelectionDispatchAction {

    // A dummy which can't be selected in the UI
    // private static final IErlElement RETURN_WITHOUT_BEEP = JavaCore
    // .create(JavaPlugin.getWorkspace().getRoot());

    private ErlangEditor fEditor;

    FindAction(final IWorkbenchSite site) {
        super(site);
        init();
    }

    FindAction(final ErlangEditor editor) {
        this(editor.getEditorSite());
        fEditor = editor;
        setEnabled(true); // FIXME kolla selection, sno grejer fr�n open
        // kanske...
    }

    /**
     * Called once by the constructors to initialize label, tooltip, image and
     * help support of the action. To be overridden by implementors of this
     * action.
     */
    abstract void init();

    private boolean canOperateOn(final IStructuredSelection sel) {
        return sel != null && !sel.isEmpty()
                && canOperateOn(getErlElement(sel, true));
    }

    boolean canOperateOn(final IErlElement element) {
        if (element instanceof IErlFunctionClause) {
            return true;
        } else if (element instanceof IErlPreprocessorDef) {
            return true;
        } else if (element instanceof IErlAttribute) {
            final IErlAttribute a = (IErlAttribute) element;
            return a.getName().startsWith("include");
        }
        return false;
    }

    // private IErlElement getTypeIfPossible(IErlElement o, final boolean
    // silent) {
    // switch (o.getElementType()) {
    // case IErlElement.COMPILATION_UNIT:
    // if (silent) {
    // return o;
    // } else {
    // return findType((ICompilationUnit) o, silent);
    // }
    // case IErlElement.CLASS_FILE:
    // return ((IClassFile) o).getType();
    // default:
    // return o;
    // }
    // }

    IErlElement getErlElement(final IStructuredSelection selection,
            final boolean silent) {
        if (selection.size() == 1) {
            final Object firstElement = selection.getFirstElement();
            if (firstElement instanceof IErlElement) {
                return (IErlElement) firstElement;
            } else if (firstElement instanceof IAdaptable) {
                return (IErlElement) ((IAdaptable) firstElement)
                        .getAdapter(IErlElement.class);
            }
        }
        return null;
    }

    private void showOperationUnavailableDialog() {
        MessageDialog.openInformation(getShell(), "Operation unavailable",
                getOperationUnavailableMessage());
    }

    String getOperationUnavailableMessage() {
        return "Operation unavailable";
    }

    // private IErlElement findType(ICompilationUnit cu, final boolean silent)
    // {
    // IType[] types = null;
    // try {
    // types = cu.getAllTypes();
    // } catch (JavaModelException ex) {
    // if (JavaModelUtil.isExceptionToBeLogged(ex)) {
    // ExceptionHandler.log(ex,
    // SearchMessages.JavaElementAction_error_open_message);
    // }
    // if (silent) {
    // return RETURN_WITHOUT_BEEP;
    // } else {
    // return null;
    // }
    // }
    // if (types.length == 1 || silent && types.length > 0) {
    // return types[0];
    // }
    // if (silent) {
    // return RETURN_WITHOUT_BEEP;
    // }
    // if (types.length == 0) {
    // return null;
    // }
    // final String title =
    // SearchMessages.JavaElementAction_typeSelectionDialog_title;
    // final String message =
    // SearchMessages.JavaElementAction_typeSelectionDialog_message;
    // final int flags = JavaElementLabelProvider.SHOW_DEFAULT;
    //
    // final ElementListSelectionDialog dialog = new ElementListSelectionDialog(
    // getShell(), new JavaElementLabelProvider(flags));
    // dialog.setTitle(title);
    // dialog.setMessage(message);
    // dialog.setElements(types);
    //
    // if (dialog.open() == Window.OK) {
    // return (IType) dialog.getFirstResult();
    // } else {
    // return RETURN_WITHOUT_BEEP;
    // }
    // }

    /*
     * Method declared on SelectionChangedAction.
     */
    @Override
    public void run(final IStructuredSelection selection) {
        final IErlElement element = getErlElement(selection, false);
        if (element == null || !element.exists()) {
            showOperationUnavailableDialog();
            return;
        }
        // else if (element == RETURN_WITHOUT_BEEP) {
        // return;
        // }

        run(element);
    }

    /*
     * Method declared on SelectionChangedAction.
     */
    @Override
    public void run(final ITextSelection selection) {
        performNewSearch(selection, getScope());
    }

    protected void performNewSearch(final ITextSelection selection,
            final Collection<IResource> scope) {
        performNewSearch(selection, getScope(), getExternalScope());
    }

    protected void performNewSearch(final ITextSelection selection,
            final Collection<IResource> scope,
            final Collection<IErlModule> externalScope) {
        // if (!ActionUtil.isProcessable(fEditor)) {
        // return;
        // }
        final IErlModule module = fEditor.getModule();
        if (module == null) {
            return;
        }
        final Backend b = ErlangCore.getBackendManager().getIdeBackend();
        final ISelection sel = getSelection();
        final ITextSelection textSel = (ITextSelection) sel;
        final int offset = textSel.getOffset();
        final String scannerModuleName = ErlangToolkit
                .createScannerModuleName(module);
        OpenResult res;
        try {
            res = ErlideOpen.open(b, scannerModuleName, offset, ErlModelUtils
                    .getImportsAsList(module), "", ErlangCore.getModel()
                    .getPathVars());
            ErlLogger.debug("find " + res);
            final ErlangSearchPattern ref = SearchUtil
                    .getSearchPatternFromOpenResultAndLimitTo(module, offset,
                            res, getLimitTo(), true);
            if (ref != null) {
                performNewSearch(ref, scope, externalScope);
            }
        } catch (final BackendException e) {
            final String title = "SearchMessages.Search_Error_search_title";
            final String message = "SearchMessages.Search_Error_codeResolve";
            ExceptionHandler.handle(e, getShell(), title, message);
        }
    }

    abstract LimitTo getLimitTo();

    abstract protected Collection<IResource> getScope();

    abstract protected Collection<IErlModule> getExternalScope();

    abstract protected String getScopeDescription();

    /*
     * Method declared on SelectionChangedAction.
     */
    @Override
    public void selectionChanged(final IStructuredSelection selection) {
        setEnabled(canOperateOn(selection));
    }

    /*
     * Method declared on SelectionChangedAction.
     */
    @Override
    public void selectionChanged(final ITextSelection selection) {
        setEnabled(true); // FIXME japps
    }

    /**
     * Executes this action for the given java element.
     * 
     * @param element
     *            The erlang element to be found.
     */
    public void run(final IErlElement element) {
        performNewSearch(element, getScope(), getExternalScope());
    }

    protected void performNewSearch(final IErlElement element,
            final Collection<IResource> scope,
            final Collection<IErlModule> externalScope) {
        final ErlangSearchPattern pattern = ErlangSearchPattern
                .getSearchPatternFromErlElementAndLimitTo(element, getLimitTo());
        SearchUtil.runQuery(pattern, scope, externalScope,
                getScopeDescription(), getShell());
    }

    private void performNewSearch(final ErlangSearchPattern ref,
            final Collection<IResource> scope,
            final Collection<IErlModule> externalScope) {
        final ErlSearchQuery query = new ErlSearchQuery(ref, scope,
                externalScope, getScopeDescription());
        if (query.canRunInBackground()) {
            /*
             * This indirection with Object as parameter is needed to prevent
             * the loading of the Search plug-in: the VM verifies the method
             * call and hence loads the types used in the method signature,
             * eventually triggering the loading of a plug-in (in this case
             * ISearchQuery results in Search plug-in being loaded).
             */
            SearchUtil.runQueryInBackground(query);
        } else {
            final IProgressService progressService = PlatformUI.getWorkbench()
                    .getProgressService();
            /*
             * This indirection with Object as parameter is needed to prevent
             * the loading of the Search plug-in: the VM verifies the method
             * call and hence loads the types used in the method signature,
             * eventually triggering the loading of a plug-in (in this case it
             * would be ISearchQuery).
             */
            final IStatus status = SearchUtil.runQueryInForeground(
                    progressService, query);
            if (status.matches(IStatus.ERROR | IStatus.INFO | IStatus.WARNING)) {
                ErrorDialog.openError(getShell(),
                        "SearchMessages.Search_Error_search_title",
                        "SearchMessages.Search_Error_search_message", status);
            }
        }
    }

    /**
     * @return the fEditor
     */
    public ErlangEditor getEditor() {
        return fEditor;
    }

    protected Collection<IResource> getProjectScope() {
        final TextEditor editor = getEditor();
        if (editor != null) {
            final IEditorInput editorInput = editor.getEditorInput();
            if (editorInput instanceof IFileEditorInput) {
                final IFileEditorInput input = (IFileEditorInput) editorInput;
                final IFile file = input.getFile();
                final IProject project = file.getProject();
                return SearchUtil.getProjectScope(project);
            }
        } else {
            final IWorkbenchSite site = getSite();
            final ISelection selection = site.getSelectionProvider()
                    .getSelection();
            if (selection instanceof IStructuredSelection) {
                final IStructuredSelection ss = (IStructuredSelection) selection;
                final Object element = ss.getFirstElement();
                if (element instanceof IErlElement) {
                    final IErlElement e = (IErlElement) element;
                    return SearchUtil.getProjectScope(e.getResource()
                            .getProject());
                } else if (element instanceof IResource) {
                    final IResource r = (IResource) element;
                    return SearchUtil.getProjectScope(r.getProject());
                }
            }
        }
        return null;
    }

    protected Collection<IResource> getWorkingSetsScope(
            final IWorkingSet[] workingSets) throws InterruptedException {
        IWorkingSet[] ws = workingSets;
        if (ws == null) {
            ws = SearchUtil.queryWorkingSets();
        }
        if (ws != null) {
            SearchUtil.updateLRUWorkingSets(ws);
            return SearchUtil.getWorkingSetsScope(ws);
        } else {
            return SearchUtil.getWorkspaceScope();
        }
    }

    protected Collection<IErlModule> getWorkingSetsExternalScope(
            final IWorkingSet[] workingSets) throws InterruptedException {
        IWorkingSet[] ws = workingSets;
        if (ws == null) {
            ws = SearchUtil.queryWorkingSets();
        }
        if (ws != null) {
            SearchUtil.updateLRUWorkingSets(ws);
            return SearchUtil.getWorkingSetsExternalScope(ws);
        } else {
            return SearchUtil.getWorkspaceExternalScope();
        }
    }
}
