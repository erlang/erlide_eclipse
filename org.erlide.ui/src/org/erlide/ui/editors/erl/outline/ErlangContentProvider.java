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
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IParent;
import org.erlide.jinterface.ErlLogger;

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
                if (!x.isDoesNotExist()) {
                    ErlLogger.debug("element missing: " + x.getMessage());
                }
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
            ErlModelManager.getErlangModel().removeElementChangedListener(
                    fListener);
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
            // TODO fixme
            fListener = new ElementChangedListener(null);

            ErlModelManager.getErlangModel().addElementChangedListener(
                    fListener);
        } else if (!isModule && fListener != null) {
            ErlModelManager.getErlangModel().removeElementChangedListener(
                    fListener);
            fListener = null;
        }
    }
}
