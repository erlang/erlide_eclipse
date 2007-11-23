package org.erlide.ui.navigator;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
//import org.eclipse.jdt.ui.ProblemsLabelDecorator;
import org.erlide.ui.internal.ProblemsLabelDecorator;

public class NavigatorProblemsDecorator extends ProblemsLabelDecorator{
	
	private boolean fIsFlatLayout;

	public NavigatorProblemsDecorator() {
		this(false);
	}

	public NavigatorProblemsDecorator(boolean isFlatLayout) {
		fIsFlatLayout = isFlatLayout;
	}

	protected int computeFolderAdornmentFlags(IFolder folder) {
		if (!fIsFlatLayout && !(folder instanceof IProject)) {
			return super.computeAdornmentFlags(folder);
		}
		return super.computeAdornmentFlags(folder);
	}

	protected int computeAdornmentFlags(Object element) {
		if (element instanceof IFolder) {
			return computeFolderAdornmentFlags((IFolder) element);
		}
		return super.computeAdornmentFlags(element);
	}

	public void setIsFlatLayout(boolean state) {
		fIsFlatLayout = state;
	}


}
