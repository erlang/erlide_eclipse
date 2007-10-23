package org.erlide.ui.navigator;

import java.text.Collator;

import org.eclipse.jface.viewers.ViewerSorter;
import org.erlide.core.erlang.IErlElement;

public class ErlElementSorter extends ViewerSorter {

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
			IErlElement e = (IErlElement) element;
			if (e.getElementType() == IErlElement.ErlElementType.FUNCTION)
				return 1000;
			else
				return e.getElementType().ordinal();
		} else
			return super.category(element);
	}

}
