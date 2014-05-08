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

import java.lang.reflect.InvocationTargetException;
import java.util.Collection;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchSite;
import org.eclipse.ui.editors.text.TextEditor;
import org.erlide.core.services.search.SearchCoreUtil;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.erlang.IErlAttribute;
import org.erlide.engine.model.erlang.IErlFunctionClause;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.erlang.IErlPreprocessorDef;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.services.search.ErlSearchScope;
import org.erlide.engine.services.search.ErlangSearchPattern;
import org.erlide.engine.services.search.LimitTo;
import org.erlide.engine.services.search.OpenResult;
import org.erlide.engine.services.search.OpenService;
import org.erlide.engine.services.search.SearchPatternFactory;
import org.erlide.ui.actions.SelectionDispatchAction;
import org.erlide.ui.editors.erl.AbstractErlangEditor;
import org.erlide.ui.internal.ExceptionHandler;
import org.erlide.util.ErlLogger;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

/**
 * Abstract class for Erlang search actions.
 * <p>
 * Note: This class is for internal use only. Clients should not use this class.
 * </p>
 *
 * @since 2.0
 */
public abstract class FindAction extends SelectionDispatchAction {

    // A dummy which can't be selected in the UI
    // private static final IErlElement RETURN_WITHOUT_BEEP = ErlangCore
    // .create(ErlangPlugin.getWorkspace().getRoot());

    private AbstractErlangEditor fEditor;

    FindAction(final IWorkbenchSite site) {
        super(site);
        init();
    }

    FindAction(final AbstractErlangEditor editor) {
        this(editor.getEditorSite());
        fEditor = editor;
        setEnabled(true); // FIXME check selection, steal stuff from 'open'
    }

    /**
     * Called once by the constructors to initialize label, tooltip, image and
     * help support of the action. To be overridden by implementors of this
     * action.
     */
    abstract void init();

    private boolean canOperateOn(final IStructuredSelection sel) {
        return sel != null && !sel.isEmpty() && canOperateOn(getErlElement(sel, true));
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

    IErlElement getErlElement(final IStructuredSelection selection, final boolean silent) {
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
        try {
            final IErlModule module = fEditor.getModule();
            if (module == null) {
                return;
            }
            final ISelection sel = getSelection();
            final ITextSelection textSel = (ITextSelection) sel;
            final int offset = textSel.getOffset();
            final OpenResult res = ErlangEngine
                    .getInstance()
                    .getService(OpenService.class)
                    .open(module.getScannerName(),
                            offset,
                            ErlangEngine.getInstance().getModelUtilService()
                                    .getImportsAsList(module), "",
                            ErlangEngine.getInstance().getModel().getPathVars());
            ErlLogger.debug("find " + res);
            final ErlangSearchPattern ref = SearchUtil
                    .getSearchPatternFromOpenResultAndLimitTo(module, offset, res,
                            getLimitTo(), true);
            if (ref != null) {
                SearchUtil.runQuery(ref, getScope(), getScopeDescription(), getShell());
            }
        } catch (final Exception e) {
            handleException(e);
        }
    }

    abstract LimitTo getLimitTo();

    abstract protected ErlSearchScope getScope() throws ErlModelException, CoreException;

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
        setEnabled(true);
    }

    /**
     * Executes this action for the given java element.
     *
     * @param element
     *            The erlang element to be found.
     * @throws CoreException
     */
    public void run(final IErlElement element) {
        try {
            final ErlangSearchPattern pattern = new SearchPatternFactory(ErlangEngine
                    .getInstance().getModelUtilService())
                    .getSearchPatternFromErlElementAndLimitTo(element, getLimitTo());
            SearchUtil.runQuery(pattern, getScope(), getScopeDescription(), getShell());
        } catch (final CoreException e) {
            handleException(e);
        }
    }

    protected void handleException(final Exception e) {
        ExceptionHandler.handle(new InvocationTargetException(e), getShell(), "Search",
                "Problems occurred while searching. "
                        + "The affected files will be skipped.");
    }

    /**
     * @return the fEditor
     */
    public AbstractErlangEditor getEditor() {
        return fEditor;
    }

    protected Collection<IProject> getProjects() {

        final TextEditor editor = getEditor();
        if (editor != null) {
            final IEditorInput editorInput = editor.getEditorInput();
            if (editorInput instanceof IFileEditorInput) {
                final IFileEditorInput input = (IFileEditorInput) editorInput;
                final IFile file = input.getFile();
                final IProject project = file.getProject();
                return Lists.newArrayList(project);
            }
        } else {
            final IWorkbenchSite site = getSite();
            final ISelection selection = site.getSelectionProvider().getSelection();
            if (selection instanceof IStructuredSelection) {
                final IStructuredSelection ss = (IStructuredSelection) selection;
                final Collection<IProject> result = Sets.newHashSet();
                for (final Object element : ss.toList()) {
                    if (element instanceof IErlElement) {
                        final IErlElement e = (IErlElement) element;
                        result.add(e.getResource().getProject());
                    } else if (element instanceof IResource) {
                        final IResource r = (IResource) element;
                        result.add(r.getProject());
                    }
                }
                return result;
            }
        }
        return null;
    }

    protected ErlSearchScope getProjectScope() throws CoreException {
        return SearchCoreUtil.getProjectsScope(getProjects(), false, false);
    }

}
