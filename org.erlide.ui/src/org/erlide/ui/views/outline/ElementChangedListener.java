/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.views.outline;

import org.eclipse.swt.widgets.Display;
import org.erlide.core.erlang.IErlElementDelta;
import org.erlide.core.erlang.util.ElementChangedEvent;
import org.erlide.core.erlang.util.IElementChangedListener;

class ElementChangedListener implements IElementChangedListener {

	/**
	 * 
	 */
	private final ErlangOutlinePage page;

	/**
	 * @param page
	 */
	ElementChangedListener(ErlangOutlinePage page) {
		this.page = page;
	}

	public void elementChanged(final ElementChangedEvent e) {

		if (this.page.getControl() == null) {
			return;
		}

		final Display d = this.page.getControl().getDisplay();
		if (d != null) {
			d.asyncExec(new Runnable() {

				public void run() {
					// IErlModule cu = (IErlModule) fInput;
					// IErlElement base = cu;
					// base = getMainType(cu);
					// if (base == null)
					// {
					// if (fOutlineViewer != null)
					// fOutlineViewer.refresh(true);
					// return;
					// }
					// IErlElementDelta delta = findElement(base,
					// e.getDelta());
					// if (delta != null && fOutlineViewer != null)
					// {
					// fOutlineViewer.reconcile(delta);
					// }

				}
			});
		}
	}

	/* FIXME: isPossibleStructuralChange -- needed, or rudiment? */
	@SuppressWarnings("unused")
	private boolean isPossibleStructuralChange(IErlElementDelta cuDelta) {
		if (cuDelta.getKind() != IErlElementDelta.CHANGED) {
			return true; // add or remove
		}
		final int flags = cuDelta.getFlags();
		if ((flags & IErlElementDelta.F_CHILDREN) != 0) {
			return true;
		}
		return (flags & (IErlElementDelta.F_CONTENT | IErlElementDelta.F_FINE_GRAINED)) == IErlElementDelta.F_CONTENT;
	}

}
