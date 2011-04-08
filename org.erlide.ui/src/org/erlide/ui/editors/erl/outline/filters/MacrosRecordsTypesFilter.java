package org.erlide.ui.editors.erl.outline.filters;

import org.eclipse.jface.viewers.Viewer;
import org.erlide.core.model.erlang.IErlPreprocessorDef;
import org.erlide.core.model.erlang.IErlTypespec;

public class MacrosRecordsTypesFilter extends ErlangViewerFilter {

    @Override
    public boolean select(final Viewer viewer, final Object parentElement,
            final Object element) {
        if (element instanceof IErlPreprocessorDef
                || element instanceof IErlTypespec) {
            return false;
        }
        return true;
    }

}
