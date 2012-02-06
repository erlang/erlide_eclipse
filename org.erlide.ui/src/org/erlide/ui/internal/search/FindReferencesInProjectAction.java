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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.IWorkbenchSite;
import org.erlide.core.services.search.ErlSearchScope;
import org.erlide.ui.editors.erl.ErlangEditor;

/**
 * Finds references to the selected element in the enclosing project of the
 * selected element. The action is applicable to selections representing a
 * Erlang element.
 * 
 * <p>
 * This class may be instantiated; it is not intended to be subclassed.
 * </p>
 * 
 * @since 3.0
 */
public class FindReferencesInProjectAction extends FindReferencesAction {

    /**
     * Creates a new <code>FindReferencesInProjectAction</code>. The action
     * requires that the selection provided by the site's selection provider is
     * of type <code>IStructuredSelection</code>.
     * 
     * @param site
     *            the site providing context information for this action
     */
    public FindReferencesInProjectAction(final IWorkbenchSite site) {
        super(site);
    }

    /**
     * Note: This constructor is for internal use only. Clients should not call
     * this constructor.
     * 
     * @param editor
     *            the Erlang editor
     */
    public FindReferencesInProjectAction(final ErlangEditor editor) {
        super(editor);
    }

    @Override
    void init() {
        setText("Project");
        setToolTipText("Find references in selected projects");
        // FIXME setImageDescriptor(JavaPluginImages.DESC_OBJS_SEARCH_REF);
        // FIXME PlatformUI.getWorkbench().getHelpSystem().setHelp(this,
        // IJavaHelpContextIds.FIND_REFERENCES_IN_PROJECT_ACTION);
    }

    @Override
    protected ErlSearchScope getScope() throws CoreException {
        return getProjectScope();
    }

    @Override
    protected String getScopeDescription() {
        return SearchUtil.getProjectScopeDescription(getProjects());
    }

}
