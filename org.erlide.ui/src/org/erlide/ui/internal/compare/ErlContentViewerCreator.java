package org.erlide.ui.internal.compare;

import org.eclipse.compare.CompareConfiguration;
import org.eclipse.compare.IViewerCreator;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.widgets.Composite;

public class ErlContentViewerCreator implements IViewerCreator {

    @Override
    public Viewer createViewer(final Composite parent,
            final CompareConfiguration config) {
        return new ErlContentViewer(parent);
    }

}
