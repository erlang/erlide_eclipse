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
package org.erlide.ui.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.erlide.ui.ErlideImage;
import org.erlide.ui.navigator.ErlElementSorter;

public class SortAction extends Action {

    private boolean fSorted;
    private int fSortedHow;

    private final StructuredViewer fViewer;

    private final ViewerComparator fComparator;
    private final ViewerComparator fComparator2;

    private final ViewerComparator fDefaultComparator;

    private final IPreferenceStore fStore;

    /**
     * @param viewer
     * @param tooltipText
     * @param sorter
     * @param defaultSorter
     * @param listener
     * @param useMiniImage
     */
    public SortAction(final StructuredViewer viewer, final String tooltipText,
            final ViewerComparator comparator,
            final ViewerComparator comparator2,
            final IPropertyChangeListener listener, final boolean useMiniImage,
            final IPreferenceStore store) {

        super(tooltipText, IAction.AS_CHECK_BOX);
        // Set the tooltip
        setToolTipText(tooltipText);
        // Set the default comparator
        fDefaultComparator = null;
        // Set the viewer
        fViewer = viewer;
        // Set prefs store
        fStore = store;
        // Set the comparators
        fComparator = comparator;
        fComparator2 = comparator2;

        // Determine if the viewer is already sorted
        fSorted = fStore != null && fStore.getBoolean("erlide.sortedOutline");
        fSortedHow = fStore != null ? fStore.getInt("erlide.sortedOutlineHow")
                : ErlElementSorter.SORT_ON_NAME;
        setComparator();
        // Set the status of this action depending on whether it is sorted or
        // not
        setChecked(fSorted);
        // Set the image
        setImage();
        // If a listener was specified, use it
        if (listener != null) {
            addListenerObject(listener);
        }
    }

    /**
	 * 
	 */
    private void setComparator() {
        if (fSorted) {
            if (fSortedHow == ErlElementSorter.SORT_ON_EXPORT) {
                fViewer.setComparator(fComparator2);
            } else {
                fViewer.setComparator(fComparator);
            }
        } else {
            fViewer.setComparator(fDefaultComparator);
        }
    }

    @Override
    public void run() {
        // Toggle sorting in three steps
        if (fSorted) {
            if (fSortedHow == ErlElementSorter.SORT_ON_NAME) {
                fSortedHow = ErlElementSorter.SORT_ON_EXPORT;
            } else {
                fSorted = false;
            }
        } else {
            // Sorting is off
            // Turn it on (name)
            fSorted = true;
            fSortedHow = ErlElementSorter.SORT_ON_NAME;
        }
        setComparator();
        if (fStore != null) {
            fStore.setValue("erlide.sortedOutline", fSorted);
            final int how = fViewer.getComparator() == fComparator2 ? ErlElementSorter.SORT_ON_EXPORT
                    : ErlElementSorter.SORT_ON_NAME;
            fStore.setValue("erlide.sortedOutlineHow", how);
        }
        setChecked(fSorted);
        setImage();
    }

    private void setImage() {
        final ImageDescriptor desc = fSorted
                && fSortedHow == ErlElementSorter.SORT_ON_EXPORT ? ErlideImage.EXPORTED_SORT
                .getDescriptor() : ErlideImage.ALPHAB_SORT.getDescriptor();
        setImageDescriptor(desc);
    }

}
