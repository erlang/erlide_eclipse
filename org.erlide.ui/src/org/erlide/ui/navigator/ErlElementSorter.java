package org.erlide.ui.navigator;

import java.text.Collator;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;

public class ErlElementSorter extends ViewerSorter {

	public static final int SORT_ON_NAME = 0;
	public static final int SORT_ON_EXPORT = 1;

	private int how;

	@Override
	public int compare(final Viewer viewer, final Object e1, final Object e2) {
		// TODO Auto-generated method stub
		return super.compare(viewer, e1, e2);
	}

	public ErlElementSorter(final int how) {
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

}
