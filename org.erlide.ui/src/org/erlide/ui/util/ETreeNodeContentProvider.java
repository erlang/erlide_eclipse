package org.erlide.ui.util;

/*******************************************************************************
 * Copyright (c) 2005, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

import com.google.common.collect.Iterables;

/**
 * <p>
 * A content provider that expects every element to be a <code>TreeNode</code>.
 * Most methods delegate to <code>TreeNode</code>. <code>dispose()</code> and
 * <code>inputChanged(Viewer, Object, Object)</code> do nothing by default.
 * </p>
 * <p>
 * This class and all of its methods may be overridden or extended.
 * </p>
 *
 */
public class ETreeNodeContentProvider implements ITreeContentProvider {
    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.IContentProvider#dispose()
     */
    @Override
    public void dispose() {
        // Do nothing
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.
     * Object)
     */
    @Override
    public Object[] getChildren(final Object parentElement) {
        final ETreeNode node = (ETreeNode) parentElement;
        return Iterables.toArray(node.getChildren(), Object.class);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java
     * .lang.Object)
     */
    @Override
    public Object[] getElements(final Object inputElement) {
        if (inputElement instanceof Iterable<?>) {
            return Iterables.toArray((Iterable<?>) inputElement, Object.class);
        }
        return new Object[0];
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object
     * )
     */
    @Override
    public Object getParent(final Object element) {
        final ETreeNode node = (ETreeNode) element;
        return node.getParent();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.
     * Object)
     */
    @Override
    public boolean hasChildren(final Object element) {
        final ETreeNode node = (ETreeNode) element;
        return node.hasChildren();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface
     * .viewers.Viewer, java.lang.Object, java.lang.Object)
     */
    @Override
    public void inputChanged(final Viewer viewer, final Object oldInput,
            final Object newInput) {
        // Do nothing
    }
}
