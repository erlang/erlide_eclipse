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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IParent;
import org.erlide.ui.views.outline.ErlangOutlinePage.NoModuleElement;

public class ErlangContentProvider implements ITreeContentProvider {

	/* FIXME: NO_MODULE -- remove or use */
	@SuppressWarnings("unused")
	private final Object[] NO_MODULE = new Object[] { new NoModuleElement() };

	private final Object[] NO_CHILDREN = new Object[] {};

	private ElementChangedListener fListener;

	private boolean fDetailed;

	public ErlangContentProvider(boolean detailed) {
		fDetailed = detailed;
	}

	public Object[] getChildren(Object parent) {
		if (parent instanceof IParent) {
			final IParent c = (IParent) parent;
			try {
				IErlElement[] res = c.getChildren();
				if (fDetailed)
					return res;
				else
					return filter(res);
			} catch (final ErlModelException x) {
				if (!x.isDoesNotExist()) {
					System.out.println("element missing: " + x.getMessage());
				}
			}
		}
		return NO_CHILDREN;
	}

	synchronized private Object[] filter(IErlElement[] res) {
		List<IErlElement> list = Arrays.asList(res);
		List<IErlElement> list2 = new ArrayList<IErlElement>(5);
		for (IErlElement element : list) {
			if (element instanceof IErlFunction)
				list2.add(element);
		}
		return list2.toArray();
	}

	public Object[] getElements(Object parent) {
		if (parent instanceof IParent) {
			try {
				IErlElement[] res = ((IParent) parent).getChildren();
				if (fDetailed)
					return res;
				else
					return filter(res);
			} catch (final ErlModelException e) {
				e.printStackTrace();
				return NO_CHILDREN;
			}
		}
		return getChildren(parent);
	}

	public Object getParent(Object child) {
		if (child instanceof IErlElement) {
			final IErlElement e = (IErlElement) child;
			return e.getParent();
		}
		return null;
	}

	public boolean hasChildren(Object parent) {
		if (parent instanceof IParent) {
			final IParent c = (IParent) parent;
			try {
				final IErlElement[] children = c.getChildren();
				return (children != null && children.length > 0);
			} catch (final ErlModelException x) {
				if (!x.isDoesNotExist()) {
					System.out.println("element missing: " + x.getMessage());
				}
			}
		}
		return false;
	}

	public boolean isDeleted(Object o) {
		return false;
	}

	public void dispose() {
		if (fListener != null) {
			ErlangCore.getModelManager()
					.removeElementChangedListener(fListener);
			fListener = null;
		}
	}

	/*
	 * @see IContentProvider#inputChanged(Viewer, Object, Object)
	 */
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		if (oldInput == newInput) {
			return;
		}
		boolean isModule = (newInput instanceof IErlModule);

		// System.out.println("> cprov set input:::" + newInput);
		if (newInput != null) {
			// System.out.println("!! " + newInput.getClass().getName() + "
			// " +
			// fListener);
		}
		if (isModule && fListener == null) {
			// TODO fixme
			fListener = new ElementChangedListener(null);

			ErlangCore.getModelManager().addElementChangedListener(fListener);
		} else if (!isModule && fListener != null) {
			ErlangCore.getModelManager()
					.removeElementChangedListener(fListener);
			fListener = null;
		}
	}
}
