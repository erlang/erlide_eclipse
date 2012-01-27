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
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.IWorkbenchSite;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.services.search.ErlSearchScope;
import org.erlide.core.services.search.ErlangSearchPattern.LimitTo;
import org.erlide.ui.editors.erl.ErlangEditor;

/**
 * Wraps a <code>ErlangElementSearchActions</code> to find its results in the
 * specified working set.
 * <p>
 * The action is applicable to selections and Search view entries representing a
 * Erlang element.
 * 
 * <p>
 * Note: This class is for internal use only. Clients should not use this class.
 * </p>
 * 
 * @since 2.0
 */
public class WorkingSetFindAction extends FindAction {

    private FindAction fAction;

    /**
     * Note: This constructor is for internal use only. Clients should not call
     * this constructor.
     */
    public WorkingSetFindAction(final IWorkbenchSite site,
            final FindAction action, final String workingSetName) {
        super(site);
        init(action, workingSetName);
    }

    /**
     * Note: This constructor is for internal use only. Clients should not call
     * this constructor.
     */
    public WorkingSetFindAction(final ErlangEditor editor,
            final FindAction action, final String workingSetName) {
        super(editor);
        init(action, workingSetName);
    }

    @Override
    void init() {
        // ignore: do our own init in 'init(FindAction, String)'
    }

    private void init(final FindAction action, final String workingSetName) {
        Assert.isNotNull(action);
        fAction = action;
        setText(workingSetName);
        setImageDescriptor(action.getImageDescriptor());
        setToolTipText(action.getToolTipText());
        // FIXME PlatformUI.getWorkbench().getHelpSystem().setHelp(this,
        // IErlangHelpContextIds.WORKING_SET_FIND_ACTION);
    }

    @Override
    public void run(final IErlElement element) {
        fAction.run(element);
    }

    @Override
    boolean canOperateOn(final IErlElement element) {
        return fAction.canOperateOn(element);
    }

    @Override
    LimitTo getLimitTo() {
        return fAction.getLimitTo();
    }

    @Override
    String getOperationUnavailableMessage() {
        return fAction.getOperationUnavailableMessage();
    }

    @Override
    protected ErlSearchScope getScope() throws ErlModelException, CoreException {
        return fAction.getScope();
    }

    @Override
    protected String getScopeDescription() {
        return "working set '" + getText() + "'";
    }
}
