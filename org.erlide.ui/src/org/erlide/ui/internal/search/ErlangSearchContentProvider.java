package org.erlide.ui.internal.search;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;

public abstract class ErlangSearchContentProvider implements
        IStructuredContentProvider, IErlSearchContentProvider {
    protected final Object[] EMPTY_ARR = new Object[0];

    private ErlangSearchResult fResult;
    private final ErlangSearchResultPage fPage;

    ErlangSearchContentProvider(final ErlangSearchResultPage page) {
        fPage = page;
    }

    public void inputChanged(final Viewer viewer, final Object oldInput,
            final Object newInput) {
        initialize((ErlangSearchResult) newInput);
    }

    protected void initialize(final ErlangSearchResult result) {
        fResult = result;
    }

    public abstract void elementsChanged(Object[] updatedElements);

    public abstract void clear();

    public void dispose() {
        // nothing to do
    }

    ErlangSearchResultPage getPage() {
        return fPage;
    }

    ErlangSearchResult getSearchResult() {
        return fResult;
    }
}
