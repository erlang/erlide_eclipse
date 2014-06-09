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

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IParent;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.util.ErlLogger;

public class ErlangContentProvider implements ITreeContentProvider {

    private final Object[] NO_CHILDREN = new Object[] {};

    private ElementChangedListener fListener;

    @Override
    public Object[] getChildren(final Object parent) {
        if (parent instanceof IParent) {
            final IParent p = (IParent) parent;
            try {
                return p.getChildren().toArray();
            } catch (final ErlModelException x) {
                ErlLogger.warn(x);
            }
        }
        return NO_CHILDREN;
    }

    @Override
    public Object[] getElements(final Object parent) {
        return getChildren(parent);
    }

    @Override
    public Object getParent(final Object child) {
        if (child instanceof IErlElement) {
            final IErlElement e = (IErlElement) child;
            return e.getParent();
        }
        return null;
    }

    @Override
    public boolean hasChildren(final Object parent) {
        if (parent instanceof IParent) {
            final IParent p = (IParent) parent;
            return p.hasChildren();
        }
        return false;
    }

    public boolean isDeleted(final Object o) {
        return false;
    }

    @Override
    public void dispose() {
        if (fListener != null) {
            ErlangEngine.getInstance().getModel().removeElementChangedListener(fListener);
            fListener = null;
        }
    }

    /*
     * @see IContentProvider#inputChanged(Viewer, Object, Object)
     */
    @Override
    public void inputChanged(final Viewer viewer, final Object oldInput,
            final Object newInput) {
        final boolean isModule = newInput instanceof IErlModule;

        // ErlLogger.debug("content set input:: " + newInput);
        if (isModule && fListener == null) {
            fListener = new ElementChangedListener(null);

            ErlangEngine.getInstance().getModel().addElementChangedListener(fListener);
        } else if (!isModule && fListener != null) {
            ErlangEngine.getInstance().getModel().removeElementChangedListener(fListener);
            fListener = null;
        }
    }
}
