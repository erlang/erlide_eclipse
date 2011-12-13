/**
 * 
 */
package org.erlide.ui.internal.search;

import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;

public class ErlangSearchTableContentProvider extends
        ErlangSearchContentProvider {

    private ErlangSearchResult fResult;

    ErlangSearchTableContentProvider(final ErlangSearchResultPage page) {
        super(page);
    }

    @Override
    public void dispose() {
        // nothing to do
    }

    @Override
    public void inputChanged(final Viewer viewer, final Object oldInput,
            final Object newInput) {
        if (newInput instanceof ErlangSearchResult) {
            fResult = (ErlangSearchResult) newInput;
        }
    }

    @Override
    public void elementsChanged(final Object[] updatedElements) {
        final TableViewer viewer = getViewer();
        final int elementLimit = getElementLimit();
        final boolean tableLimited = elementLimit != -1;
        for (int i = 0; i < updatedElements.length; i++) {
            if (fResult.getMatchCount(updatedElements[i]) > 0) {
                if (viewer.testFindItem(updatedElements[i]) != null) {
                    viewer.update(updatedElements[i], null);
                } else {
                    if (!tableLimited
                            || viewer.getTable().getItemCount() < elementLimit) {
                        viewer.add(updatedElements[i]);
                    }
                }
            } else {
                viewer.remove(updatedElements[i]);
            }
        }
    }

    @Override
    public Object[] getElements(final Object inputElement) {
        if (inputElement instanceof ErlangSearchResult) {
            final ErlangSearchResult esr = (ErlangSearchResult) inputElement;
            final int elementLimit = getElementLimit();
            final Object[] elements = esr.getElements();
            if (elementLimit != -1 && elements.length > elementLimit) {
                final Object[] shownElements = new Object[elementLimit];
                System.arraycopy(elements, 0, shownElements, 0, elementLimit);
                return shownElements;
            }
            return elements;
        }
        return EMPTY_ARR;
    }

    @Override
    public void clear() {
        getViewer().refresh();
    }

    private int getElementLimit() {
        return getPage().getElementLimit().intValue();
    }

    private TableViewer getViewer() {
        return (TableViewer) getPage().getViewer();
    }
}
