package org.erlide.ui.navigator;

import java.text.Collator;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.erlide.core.erlang.IErlElement;

public class ErlElementSorter extends ViewerSorter {

	@Override
	public int compare(Viewer viewer, Object e1, Object e2) {
		// TODO Auto-generated method stub
		return super.compare(viewer, e1, e2);
	}

	public ErlElementSorter() {
		// TODO Auto-generated constructor stub
	}

	public ErlElementSorter(Collator collator) {
		super(collator);
		// TODO Auto-generated constructor stub
	}

	@Override
	public int category(Object element) {
		if (element instanceof IErlElement) {
			final IErlElement e = (IErlElement) element;
			if (e.getKind() == IErlElement.Kind.FUNCTION) {
				return 1000;
			}
			return e.getKind().ordinal();
		}
		return super.category(element);
	}

}
