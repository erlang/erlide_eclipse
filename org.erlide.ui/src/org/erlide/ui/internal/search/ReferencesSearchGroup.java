/*******************************************************************************
 * Copyright (c) 2000, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.internal.search;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.search.ui.IContextMenuConstants;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IWorkbenchSite;
import org.eclipse.ui.IWorkingSet;
import org.eclipse.ui.actions.ActionContext;
import org.eclipse.ui.actions.ActionGroup;
import org.eclipse.ui.texteditor.ITextEditorActionConstants;
import org.erlide.ui.ErlideUIConstants;
import org.erlide.ui.actions.SelectionDispatchAction;
import org.erlide.ui.editors.erl.ErlangEditor;

/**
 * Action group that adds the search for references actions to a context menu
 * and the global menu bar.
 * 
 * <p>
 * This class may be instantiated; it is not intended to be subclassed.
 * </p>
 * 
 * @since 2.0
 */
public class ReferencesSearchGroup extends ActionGroup {

    private static final String MENU_TEXT = "References";

    private final IWorkbenchSite fSite;
    private ErlangEditor fEditor;
    private IActionBars fActionBars;

    private final String fGroupId;

    private FindReferencesAction fFindReferencesAction;
    private FindReferencesInProjectAction fFindReferencesInProjectAction;

    private FindAction fFindReferencesInWorkingSetAction;

    /**
     * Creates a new <code>ReferencesSearchGroup</code>. The group requires that
     * the selection provided by the site's selection provider is of type <code>
     * org.eclipse.jface.viewers.IStructuredSelection</code> .
     * 
     * @param site
     *            the view part that owns this action group
     */
    public ReferencesSearchGroup(final IWorkbenchSite site) {
        fSite = site;
        fGroupId = IContextMenuConstants.GROUP_SEARCH;

        fFindReferencesAction = new FindReferencesAction(site);
        fFindReferencesAction
                .setActionDefinitionId("org.erlide.ui.search.references.in.workspace");

        fFindReferencesInProjectAction = new FindReferencesInProjectAction(site);
        fFindReferencesInProjectAction
                .setActionDefinitionId("org.erlide.ui.search.references.in.project");

        fFindReferencesInWorkingSetAction = new FindReferencesInWorkingSetAction(
                site);
        fFindReferencesInWorkingSetAction
                .setActionDefinitionId("org.erlide.ui.search.references.in.workset");

        // register the actions as selection listeners
        final ISelectionProvider provider = fSite.getSelectionProvider();
        final ISelection selection = provider.getSelection();
        registerAction(fFindReferencesAction, provider, selection);
        registerAction(fFindReferencesInProjectAction, provider, selection);
        registerAction(fFindReferencesInWorkingSetAction, provider, selection);
    }

    /**
     * Note: This constructor is for internal use only. Clients should not call
     * this constructor.
     * 
     * @param editor
     *            the Erlang editor
     */
    public ReferencesSearchGroup(final ErlangEditor editor) {
        Assert.isNotNull(editor);
        fEditor = editor;
        fSite = fEditor.getSite();
        fGroupId = ITextEditorActionConstants.GROUP_FIND;

        fFindReferencesAction = new FindReferencesAction(editor);
        fFindReferencesAction
                .setActionDefinitionId("org.erlide.ui.search.references.in.workspace");
        fEditor.setAction("SearchReferencesInWorkspace", fFindReferencesAction); //$NON-NLS-1$

        fFindReferencesInProjectAction = new FindReferencesInProjectAction(
                fEditor);
        fFindReferencesInProjectAction
                .setActionDefinitionId("org.erlide.ui.search.references.in.project");
        fEditor.setAction(
                "SearchReferencesInProject", fFindReferencesInProjectAction); //$NON-NLS-1$

        fFindReferencesInWorkingSetAction = new FindReferencesInWorkingSetAction(
                fEditor);
        fFindReferencesInWorkingSetAction
                .setActionDefinitionId("org.erlide.ui.search.references.in.workset");
        fEditor.setAction(
                "SearchReferencesInWorkingSet", fFindReferencesInWorkingSetAction); //$NON-NLS-1$
    }

    private void registerAction(final SelectionDispatchAction action,
            final ISelectionProvider provider, final ISelection selection) {
        action.update(selection);
        provider.addSelectionChangedListener(action);
    }

    /**
     * Note: this method is for internal use only. Clients should not call this
     * method.
     * 
     * @return the menu label
     */
    protected String getName() {
        return MENU_TEXT;
    }

    @Override
    public void fillActionBars(final IActionBars actionBars) {
        Assert.isNotNull(actionBars);
        super.fillActionBars(actionBars);
        fActionBars = actionBars;
        updateGlobalActionHandlers();
    }

    private void addAction(final IAction action, final IMenuManager manager) {
        if (action.isEnabled()) {
            manager.add(action);
        }
    }

    private void addWorkingSetAction(final IWorkingSet[] workingSets,
            final IMenuManager manager) {
        FindAction action;
        if (fEditor != null) {
            action = new WorkingSetFindAction(fEditor,
                    new FindReferencesInWorkingSetAction(fEditor, workingSets),
                    SearchUtil.toString(workingSets));
        } else {
            action = new WorkingSetFindAction(fSite,
                    new FindReferencesInWorkingSetAction(fSite, workingSets),
                    SearchUtil.toString(workingSets));
        }
        final ActionContext context = getContext();
        if (context != null) {
            action.update(context.getSelection());
        }
        addAction(action, manager);
    }

    @Override
    public void fillContextMenu(final IMenuManager manager) {
        final MenuManager erlangSearchMM = new MenuManager(getName(),
                IContextMenuConstants.GROUP_SEARCH);
        addAction(fFindReferencesAction, erlangSearchMM);
        addAction(fFindReferencesInProjectAction, erlangSearchMM);

        erlangSearchMM.add(new Separator());

        for (final IWorkingSet[] i : SearchUtil.getLRUWorkingSets().getSorted()) {
            addWorkingSetAction(i, erlangSearchMM);
        }
        addAction(fFindReferencesInWorkingSetAction, erlangSearchMM);

        if (!erlangSearchMM.isEmpty()) {
            manager.appendToGroup(fGroupId, erlangSearchMM);
        }
    }

    /*
     * Overrides method declared in ActionGroup
     */
    @Override
    public void dispose() {
        final ISelectionProvider provider = fSite.getSelectionProvider();
        if (provider != null) {
            disposeAction(fFindReferencesAction, provider);
            disposeAction(fFindReferencesInProjectAction, provider);
            disposeAction(fFindReferencesInWorkingSetAction, provider);
        }
        fFindReferencesAction = null;
        fFindReferencesInProjectAction = null;
        fFindReferencesInWorkingSetAction = null;
        updateGlobalActionHandlers();
        super.dispose();
    }

    private void updateGlobalActionHandlers() {
        if (fActionBars != null) {
            fActionBars.setGlobalActionHandler(
                    ErlideUIConstants.FIND_REFERENCES_IN_WORKSPACE,
                    fFindReferencesAction);
            fActionBars.setGlobalActionHandler(
                    ErlideUIConstants.FIND_REFERENCES_IN_PROJECT,
                    fFindReferencesInProjectAction);
            fActionBars.setGlobalActionHandler(
                    ErlideUIConstants.FIND_REFERENCES_IN_WORKING_SET,
                    fFindReferencesInWorkingSetAction);
        }
    }

    private void disposeAction(final ISelectionChangedListener action,
            final ISelectionProvider provider) {
        if (action != null) {
            provider.removeSelectionChangedListener(action);
        }
    }
}
