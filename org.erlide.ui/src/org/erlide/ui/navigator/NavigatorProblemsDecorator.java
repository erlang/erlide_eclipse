package org.erlide.ui.navigator;

import org.eclipse.core.resources.IFolder;
import org.erlide.ui.util.ProblemsLabelDecorator;

public class NavigatorProblemsDecorator extends ProblemsLabelDecorator {

    private boolean fIsFlatLayout;

    public NavigatorProblemsDecorator() {
        this(false);
    }

    public NavigatorProblemsDecorator(final boolean isFlatLayout) {
        fIsFlatLayout = isFlatLayout;
    }

    protected int computeFolderAdornmentFlags(final IFolder folder) {
        // if (!fIsFlatLayout && !(folder instanceof IProject)) {
        if (!fIsFlatLayout) {
            return super.computeAdornmentFlags(folder);
        }
        return super.computeAdornmentFlags(folder);
    }

    @Override
    protected int computeAdornmentFlags(final Object element) {
        if (element instanceof IFolder) {
            return computeFolderAdornmentFlags((IFolder) element);
        }
        return super.computeAdornmentFlags(element);
    }

    public void setIsFlatLayout(final boolean state) {
        fIsFlatLayout = state;
    }

}
