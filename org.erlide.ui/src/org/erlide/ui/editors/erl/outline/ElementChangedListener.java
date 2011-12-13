/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.editors.erl.outline;

import org.eclipse.swt.widgets.Display;
import org.erlide.core.model.root.IErlElementDelta;
import org.erlide.core.model.util.ElementChangedEvent;
import org.erlide.core.model.util.IElementChangedListener;

class ElementChangedListener implements IElementChangedListener {

    /**
	 * 
	 */
    private final ErlangOutlinePage page;

    /**
     * @param page
     */
    ElementChangedListener(final ErlangOutlinePage page) {
        this.page = page;
    }

    @Override
    public void elementChanged(final ElementChangedEvent e) {

        if (page.getControl() == null) {
            return;
        }

        final Display d = page.getControl().getDisplay();
        if (d != null) {
            d.asyncExec(new Runnable() {

                @Override
                public void run() {
                    // IErlModule cu = (IErlModule) fInput;
                    // IErlElement base = cu;
                    // base = getMainType(cu);
                    // if (base == null)
                    // {
                    // if (fOutlineViewer != null)
                    // fOutlineViewer.refresh(true);
                    // return;
                    // }
                    // IErlElementDelta delta = findElement(base,
                    // e.getDelta());
                    // if (delta != null && fOutlineViewer != null)
                    // {
                    // fOutlineViewer.reconcile(delta);
                    // }

                }
            });
        }
    }

    protected boolean isPossibleStructuralChange(final IErlElementDelta cuDelta) {
        if (cuDelta.getKind() != IErlElementDelta.CHANGED) {
            return true; // add or remove
        }
        final int flags = cuDelta.getFlags();
        if ((flags & IErlElementDelta.F_CHILDREN) != 0) {
            return true;
        }
        return (flags & (IErlElementDelta.F_CONTENT | IErlElementDelta.F_FINE_GRAINED)) == IErlElementDelta.F_CONTENT;
    }

}
