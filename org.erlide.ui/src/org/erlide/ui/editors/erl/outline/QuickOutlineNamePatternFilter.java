/*******************************************************************************
 * Copyright (c) 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

package org.erlide.ui.editors.erl.outline;

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.erlide.ui.util.StringMatcher;

/**
 * QuickOutlineNamePatternFilter
 * 
 */
public class QuickOutlineNamePatternFilter extends ViewerFilter {

    private StringMatcher fStringMatcher;

    /**
     *
     */
    public QuickOutlineNamePatternFilter() {
        fStringMatcher = null;
    }

    @Override
    public boolean select(final Viewer viewer, final Object parentElement,
            final Object element) {
        // Element passes the filter if the string matcher is undefined or the
        // viewer is not a tree viewer
        if (fStringMatcher == null || !(viewer instanceof TreeViewer)) {
            return true;
        }
        final TreeViewer treeViewer = (TreeViewer) viewer;
        // Match the pattern against the label of the given element
        final String matchName = ((ILabelProvider) treeViewer
                .getLabelProvider()).getText(element);
        // Element passes the filter if it matches the pattern
        if (matchName != null && fStringMatcher.match(matchName)) {
            return true;
        }
        // Determine whether the element has children that pass the filter
        return hasUnfilteredChild(treeViewer, element);
    }

    /**
     * @param viewer
     * @param element
     * @return
     */
    private boolean hasUnfilteredChild(final TreeViewer viewer,
            final Object element) {
        // No point calling hasChildren() because the operation is the same cost
        // as getting the children
        // If the element has a child that passes the filter, then we want to
        // keep the parent around - even if it does not pass the filter itself
        final Object[] children = ((ITreeContentProvider) viewer
                .getContentProvider()).getChildren(element);
        for (int i = 0; i < children.length; i++) {
            if (select(viewer, element, children[i])) {
                return true;
            }
        }
        // Element does not pass the filter
        return false;
    }

    /**
     * @param stringMatcher
     */
    public void setStringMatcher(final StringMatcher stringMatcher) {
        fStringMatcher = stringMatcher;
    }

}
