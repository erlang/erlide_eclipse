package org.erlide.ui.navigator;

import java.text.Collator;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.ISourceReference;
import org.erlide.core.model.root.IErlElement;

public class ErlElementSorter extends ViewerSorter {

    public static final int SORT_ON_NAME = 0;
    public static final int SORT_ON_EXPORT = 1;

    private int how = SORT_ON_NAME;

    @Override
    public int compare(final Viewer viewer, final Object o1, final Object o2) {
        if (o1 instanceof IErlElement && o2 instanceof IErlElement) {
            final IErlElement e1 = (IErlElement) o1;
            final IErlElement e2 = (IErlElement) o2;
            if (e1.getKind() == IErlElement.Kind.CLAUSE
                    && e2.getKind() == IErlElement.Kind.CLAUSE) {
                return comparePositions(viewer, o1, o2);
            }
        }
        return super.compare(viewer, o1, o2);
    }

    public ErlElementSorter() {
        super();
    }

    public ErlElementSorter(final int how) {
        super();
        this.how = how;
    }

    public ErlElementSorter(final Collator collator) {
        super(collator);
    }

    @Override
    public int category(final Object element) {
        if (how == SORT_ON_NAME) {
            if (element instanceof IErlElement) {
                final IErlElement e = (IErlElement) element;
                if (e.getKind() == IErlElement.Kind.FUNCTION) {
                    return 1000;
                }
                return e.getKind().ordinal();
            }
        } else if (how == SORT_ON_EXPORT) {
            if (element instanceof IErlElement) {
                final IErlElement e = (IErlElement) element;
                if (e.getKind() == IErlElement.Kind.FUNCTION) {
                    if (e instanceof IErlFunction) {
                        final IErlFunction f = (IErlFunction) e;
                        if (f.isExported()) {
                            return 500;
                        }
                    }
                    return 1000;
                }
                return e.getKind().ordinal();
            }
        }
        return super.category(element);
    }

    /**
     * @param e1
     * @param e2
     */
    protected int comparePositions(final Viewer viewer, final Object e1,
            final Object e2) {
        if (e1 instanceof ISourceReference) {
            final ISourceReference s1 = (ISourceReference) e1;
            if (e2 instanceof ISourceReference) {
                final ISourceReference s2 = (ISourceReference) e2;
                final int p1 = s1.getSourceRange().getOffset();
                final int p2 = s2.getSourceRange().getOffset();
                if (p1 < p2) {
                    return -1;
                } else if (p1 > p2) {
                    return 1;
                } else {
                    return 0;
                }
            }
        }
        return super.compare(viewer, e1, e2);
    }

}
