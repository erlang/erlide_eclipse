package org.erlide.ui.navigator;

import java.text.Collator;

import org.eclipse.jface.viewers.Viewer;

public class ErlElementPositionSorter extends ErlElementSorter {

    @Override
    public int compare(final Viewer viewer, final Object e1, final Object e2) {
        return comparePositions(viewer, e1, e2);
    }

    public ErlElementPositionSorter() {
        super();
    }

    public ErlElementPositionSorter(final Collator collator) {
        super(collator);
    }

    // @Override
    // public int category(final Object element) {
    // if (element instanceof IErlElement) {
    // final IErlElement e = (IErlElement) element;
    // if (e.getKind() == IErlElement.Kind.FUNCTION) {
    // return 1000;
    // }
    // return e.getKind().ordinal();
    // }
    // return super.category(element);
    // }

}
