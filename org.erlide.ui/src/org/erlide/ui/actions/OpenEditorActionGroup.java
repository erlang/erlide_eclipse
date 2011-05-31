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
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchSite;
import org.eclipse.ui.actions.ActionGroup;
import org.eclipse.ui.actions.OpenWithMenu;
import org.erlide.ui.editors.erl.IErlangEditorActionDefinitionIds;
import org.erlide.ui.util.IContextMenuConstants;

/**
 * Action group that adds the actions opening a new editor to the context menu
 * and the action bar's navigate menu.
 * 
 * <p>
 * This class may be instantiated; it is not intended to be subclassed.
 * </p>
 * 
 * @since 2.0
 */
public class OpenEditorActionGroup extends ActionGroup {

    private final IWorkbenchSite fSite;

    private boolean fIsEditorOwner;

    private final OpenAction fOpen;

    /**
     * Creates a new <code>OpenActionGroup</code>. The group requires that the
     * selection provided by the part's selection provider is of type <code>
     * org.eclipse.jface.viewers.IStructuredSelection</code> .
     * 
     * @param part
     *            the view part that owns this action group
     */
    public OpenEditorActionGroup(final IViewPart part) {
        fSite = part.getSite();
        fOpen = new OpenAction(fSite);
        fOpen.setActionDefinitionId(IErlangEditorActionDefinitionIds.OPEN_EDITOR);
        initialize(fSite.getSelectionProvider());
    }

    /**
     * Returns the open action managed by this action group.
     * 
     * @return the open action. Returns <code>null</code> if the group doesn't
     *         provide any open action
     */
    public IAction getOpenAction() {
        return fOpen;
    }

    private void initialize(final ISelectionProvider provider) {
        final ISelection selection = provider.getSelection();
        fOpen.update(selection);
        if (!fIsEditorOwner) {
            provider.addSelectionChangedListener(fOpen);
        }
    }

    @Override
    public void fillActionBars(final IActionBars actionBar) {
        super.fillActionBars(actionBar);
        setGlobalActionHandlers(actionBar);
    }

    @Override
    public void fillContextMenu(final IMenuManager menu) {
        super.fillContextMenu(menu);
        appendToGroup(menu, fOpen);
        if (!fIsEditorOwner) {
            addOpenWithMenu(menu);
        }
    }

    /*
     * @see ActionGroup#dispose()
     */
    @Override
    public void dispose() {
        final ISelectionProvider provider = fSite.getSelectionProvider();
        provider.removeSelectionChangedListener(fOpen);
        super.dispose();
    }

    private void setGlobalActionHandlers(final IActionBars actionBars) {
        actionBars.setGlobalActionHandler(
                IErlangEditorActionDefinitionIds.OPEN_EDITOR, fOpen);
    }

    private void appendToGroup(final IMenuManager menu, final IAction action) {
        if (action.isEnabled()) {
            menu.appendToGroup(IContextMenuConstants.GROUP_OPEN, action);
        }
    }

    private void addOpenWithMenu(final IMenuManager menu) {
        final ISelection selection = getContext().getSelection();
        if (selection.isEmpty() || !(selection instanceof IStructuredSelection)) {
            return;
        }
        final IStructuredSelection ss = (IStructuredSelection) selection;
        if (ss.size() != 1) {
            return;
        }

        final Object o = ss.getFirstElement();
        if (!(o instanceof IAdaptable)) {
            return;
        }

        final IAdaptable element = (IAdaptable) o;
        final Object resource = element.getAdapter(IResource.class);
        if (!(resource instanceof IFile)) {
            return;
        }

        // Create a menu.
        final IMenuManager submenu = new MenuManager(
                ActionMessages.OpenWithMenu_label);
        submenu.add(new OpenWithMenu(fSite.getPage(), (IFile) resource));

        // Add the submenu.
        menu.appendToGroup(IContextMenuConstants.GROUP_OPEN, submenu);
    }
}
