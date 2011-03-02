package org.erlide.ui.editors.erl.outline.filters;

import org.eclipse.jface.viewers.Viewer;
import org.erlide.core.model.erlang.IErlAttribute;
import org.erlide.core.model.erlang.IErlImportExport;

public class AttributesFilter extends ErlangViewerFilter {

    @Override
    public boolean select(final Viewer viewer, final Object parentElement,
            final Object element) {
        if (element instanceof IErlAttribute
                || element instanceof IErlImportExport) {
            return false;
        }
        return true;
    }

}
