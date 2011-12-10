/*******************************************************************************
 * Copyright (c) 2000, 2008 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.internal.search;

import java.text.Collator;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.eclipse.ui.IWorkingSet;
import org.eclipse.ui.PlatformUI;

public class LRUWorkingSetsList {

    public static class WorkingSetsComparator implements
            Comparator<IWorkingSet[]> {
        private static final Collator collator = Collator.getInstance();

        @Override
        public int compare(final IWorkingSet[] o1, final IWorkingSet[] o2) {
            final String s1 = o1.length > 0 ? o1[0].getLabel() : null;
            final String s2 = o2.length > 0 ? o2[0].getLabel() : null;
            return collator.compare(s1, s2);
        }

    }

    private final ArrayList<IWorkingSet[]> fLRUList;
    private final int fSize;
    private final WorkingSetsComparator fComparator = new WorkingSetsComparator();

    public LRUWorkingSetsList(final int size) {
        fSize = size;
        fLRUList = new ArrayList<IWorkingSet[]>(size);
    }

    public void add(final IWorkingSet[] workingSets) {
        removeDeletedWorkingSets();
        final IWorkingSet[] existingWorkingSets = find(fLRUList, workingSets);
        if (existingWorkingSets != null) {
            fLRUList.remove(existingWorkingSets);
        } else if (fLRUList.size() == fSize) {
            fLRUList.remove(fSize - 1);
        }
        fLRUList.add(0, workingSets);

    }

    public Collection<IWorkingSet[]> getSorted() {
        final ArrayList<IWorkingSet[]> sortedList = new ArrayList<IWorkingSet[]>(
                fLRUList);
        Collections.sort(sortedList, fComparator);
        return sortedList;
    }

    public Collection<IWorkingSet[]> get() {
        return new ArrayList<IWorkingSet[]>(fLRUList);
    }

    private void removeDeletedWorkingSets() {
        final Iterator<IWorkingSet[]> iter = new ArrayList<IWorkingSet[]>(
                fLRUList).iterator();
        while (iter.hasNext()) {
            final IWorkingSet[] workingSets = iter.next();
            for (int i = 0; i < workingSets.length; i++) {
                if (PlatformUI.getWorkbench().getWorkingSetManager()
                        .getWorkingSet(workingSets[i].getName()) == null) {
                    fLRUList.remove(workingSets);
                    break;
                }
            }
        }
    }

    private IWorkingSet[] find(final ArrayList<IWorkingSet[]> list,
            final IWorkingSet[] workingSets) {
        final Set<IWorkingSet> workingSetList = new HashSet<IWorkingSet>(
                Arrays.asList(workingSets));
        final Iterator<IWorkingSet[]> iter = list.iterator();
        while (iter.hasNext()) {
            final IWorkingSet[] lruWorkingSets = iter.next();
            final Set<IWorkingSet> lruWorkingSetList = new HashSet<IWorkingSet>(
                    Arrays.asList(lruWorkingSets));
            if (lruWorkingSetList.equals(workingSetList)) {
                return lruWorkingSets;
            }
        }
        return null;
    }
}
