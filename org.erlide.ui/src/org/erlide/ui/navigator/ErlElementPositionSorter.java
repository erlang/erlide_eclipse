package org.erlide.ui.navigator;

import java.text.Collator;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ISourceReference;
import org.erlide.runtime.ErlLogger;

public class ErlElementPositionSorter extends ViewerSorter {

	@Override
	public int compare(final Viewer viewer, final Object e1, final Object e2) {
		if (e1 instanceof ISourceReference) {
			final ISourceReference s1 = (ISourceReference) e1;
			if (e2 instanceof ISourceReference) {
				final ISourceReference s2 = (ISourceReference) e2;
				try {
					final int p1 = s1.getSourceRange().getOffset();
					final int p2 = s2.getSourceRange().getOffset();
					if (p1 < p2) {
						return -1;
					} else if (p1 > p2) {
						return 1;
					} else {
						return 0;
					}
				} catch (final ErlModelException e) {
					ErlLogger.warn(e);
				}
			}
		}
		return super.compare(viewer, e1, e2);
	}

	public ErlElementPositionSorter() {
	}

	public ErlElementPositionSorter(final Collator collator) {
		super(collator);
	}

	// @Override
	// public int category(final Object element) {
	// if (element instanceof IErlElement) {
	// final IErlElement e = (IErlElement) element;
	// if (e.getKind() == IErlElement.Kind.FUNCTION) {
	// return 1000;
	// }
	// return e.getKind().ordinal();
	// }
	// return super.category(element);
	// }

}
