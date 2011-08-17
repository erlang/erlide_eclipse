package org.erlide.ui.editors.erl.outline.filters;

import org.eclipse.jface.viewers.Viewer;
import org.erlide.core.model.erlang.IErlFunction;

public class LocalFunctionsFilter extends ErlangViewerFilter {

    @Override
    public boolean select(final Viewer viewer, final Object parentElement,
            final Object element) {
        if (element instanceof IErlFunction) {
            final IErlFunction f = (IErlFunction) element;
            if (!f.isExported()) {
                return false;
            }
        }
        return true;
    }

}
