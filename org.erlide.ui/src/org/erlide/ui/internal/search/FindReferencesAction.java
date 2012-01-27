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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.IWorkbenchSite;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.services.search.ErlSearchScope;
import org.erlide.core.services.search.ErlangSearchPattern.LimitTo;
import org.erlide.ui.editors.erl.ErlangEditor;

/**
 * Finds references of the selected element in the workspace. The action is
 * applicable to selections representing a Erlang element.
 * 
 * <p>
 * This class may be instantiated; it is not intended to be subclassed.
 * </p>
 * 
 * @since 2.0
 */
public class FindReferencesAction extends FindAction {

    /**
     * Creates a new <code>FindReferencesAction</code>. The action requires that
     * the selection provided by the site's selection provider is of type
     * <code>org.eclipse.jface.viewers.IStructuredSelection</code>.
     * 
     * @param site
     *            the site providing context information for this action
     */
    public FindReferencesAction(final IWorkbenchSite site) {
        super(site);
    }

    /**
     * Note: This constructor is for internal use only. Clients should not call
     * this constructor.
     * 
     * @param editor
     *            the Erlang editor
     */
    public FindReferencesAction(final ErlangEditor editor) {
        super(editor);
    }

    @Override
    void init() {
        setText("Workspace");
        setToolTipText("Find references in workspace");
        // FIXME setImageDescriptor(JavaPluginImages.DESC_OBJS_SEARCH_REF);
        // FIXME PlatformUI.getWorkbench().getHelpSystem().setHelp(this,
        // IJavaHelpContextIds.FIND_REFERENCES_IN_WORKSPACE_ACTION);
    }

    @Override
    LimitTo getLimitTo() {
        return LimitTo.REFERENCES;
    }

    @Override
    protected ErlSearchScope getScope() throws ErlModelException, CoreException {
        return SearchUtil.getWorkspaceScope(false, false);
    }

    @Override
    protected String getScopeDescription() {
        return SearchUtil.getWorkspaceScopeDescription();
    }
}
