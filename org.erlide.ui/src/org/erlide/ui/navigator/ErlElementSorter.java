package org.erlide.ui.navigator;

import java.text.Collator;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.erlide.engine.model.erlang.IErlFunction;
import org.erlide.engine.model.erlang.ISourceReference;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;

public class ErlElementSorter extends ViewerSorter {

    public static final int SORT_ON_NAME = 0;
    public static final int SORT_ON_EXPORT = 1;

    private int how = SORT_ON_NAME;

    @Override
    public int compare(final Viewer viewer, final Object o1, final Object o2) {
        if (o1 instanceof IErlElement && o2 instanceof IErlElement) {
            final IErlElement e1 = (IErlElement) o1;
            final IErlElement e2 = (IErlElement) o2;
            if (e1.getKind() == ErlElementKind.CLAUSE
                    && e2.getKind() == ErlElementKind.CLAUSE) {
                return comparePositions(viewer, (ISourceReference) o1,
                        (ISourceReference) o2);
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
        if (!(element instanceof IErlElement)) {
            return super.category(element);
        }
        final IErlElement e = (IErlElement) element;
        switch (how) {
        case SORT_ON_NAME:
            if (e.getKind() == ErlElementKind.FUNCTION) {
                return 1000;
            }
            return e.getKind().ordinal();
        case SORT_ON_EXPORT:
            if (e.getKind() == ErlElementKind.FUNCTION) {
                if (e instanceof IErlFunction) {
                    final IErlFunction f = (IErlFunction) e;
                    if (f.isExported()) {
                        return 500;
                    }
                }
                return 1000;
            }
            return e.getKind().ordinal();
        default:
            return super.category(element);
        }
    }

    protected int comparePositions(final Viewer viewer, final ISourceReference s1,
            final ISourceReference s2) {
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
