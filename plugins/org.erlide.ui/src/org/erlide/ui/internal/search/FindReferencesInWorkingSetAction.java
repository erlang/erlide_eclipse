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
import org.eclipse.ui.IWorkingSet;
import org.erlide.core.services.search.SearchCoreUtil;
import org.erlide.engine.services.search.ErlSearchScope;
import org.erlide.ui.editors.erl.AbstractErlangEditor;

/**
 * Finds references of the selected element in working sets. The action is
 * applicable to selections representing a Erlang element.
 *
 * <p>
 * This class may be instantiated; it is not intended to be subclassed.
 * </p>
 *
 * @since 2.0
 */
public class FindReferencesInWorkingSetAction extends FindReferencesAction {

    private final IWorkingSet[] fWorkingSets;

    /**
     * Creates a new <code>FindReferencesInWorkingSetAction</code>. The action
     * requires that the selection provided by the site's selection provider is
     * of type <code>org.eclipse.jface.viewers.IStructuredSelection</code>. The
     * user will be prompted to select the working sets.
     *
     * @param site
     *            the site providing context information for this action
     */
    public FindReferencesInWorkingSetAction(final IWorkbenchSite site) {
        this(site, null);
    }

    /**
     * Creates a new <code>FindReferencesInWorkingSetAction</code>. The action
     * requires that the selection provided by the site's selection provider is
     * of type <code>org.eclipse.jface.viewers.IStructuredSelection</code>.
     *
     * @param site
     *            the site providing context information for this action
     * @param workingSets
     *            the working sets to be used in the search
     */
    public FindReferencesInWorkingSetAction(final IWorkbenchSite site,
            final IWorkingSet[] workingSets) {
        super(site);
        fWorkingSets = workingSets;
    }

    /**
     * Note: This constructor is for internal use only. Clients should not call
     * this constructor.
     *
     * @param editor
     *            the Erlang editor
     */
    public FindReferencesInWorkingSetAction(final AbstractErlangEditor editor) {
        this(editor, null);
    }

    /**
     * Note: This constructor is for internal use only. Clients should not call
     * this constructor.
     *
     * @param editor
     *            the Erlang editor
     * @param workingSets
     *            the working sets to be used in the search
     */
    public FindReferencesInWorkingSetAction(final AbstractErlangEditor editor,
            final IWorkingSet[] workingSets) {
        super(editor);
        fWorkingSets = workingSets;
    }

    @Override
    void init() {
        setText("Working set");
        setToolTipText("Find references in working set");
    }

    @Override
    protected ErlSearchScope getScope() throws CoreException {
        if (fWorkingSets != null) {
            return SearchUtil.getWorkingSetsScope(fWorkingSets, false, false);
        }
        return SearchCoreUtil.getWorkspaceScope(false, false);
    }
}
