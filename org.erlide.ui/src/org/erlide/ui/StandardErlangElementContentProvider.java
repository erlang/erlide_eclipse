/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IParent;
import org.erlide.core.erlang.ISourceReference;

/**
 * A base content provider for Erlang elements. It provides access to the Erlang
 * element hierarchy without listening to changes in the Erlang model. If
 * updating the presentation on Erlang model change is required than clients
 * have to subclass, listen to Erlang model changes and have to update the UI
 * using corresponding methods provided by the JFace viewers or their own UI
 * presentation.
 * <p>
 * The following Erlang element hierarchy is surfaced by this content provider:
 * <p>
 * 
 * <pre>
 *        model (
 * <code>
 * IErlangModel
 * </code>
 *    )
 *          project (
 * <code>
 * IErlangProject
 * </code>
 *    )
 *              module (
 * <code>
 * IErlModule
 * </code>
 *    )
 *              binary beam file (
 * <code>
 * IBeamFile
 * </code>
 *    )
 * </pre>
 * 
 * </p>
 */
public class StandardErlangElementContentProvider implements
		ITreeContentProvider {

	protected static final Object[] NO_CHILDREN = new Object[0];

	protected boolean fProvideMembers;

	/**
	 * Creates a new content provider. The content provider does not provide
	 * members of compilation units or class files.
	 */
	public StandardErlangElementContentProvider() {
		this(false);
	}

	/**
	 * Creates a new <code>StandardErlangElementContentProvider</code>.
	 * 
	 * @param provideMembers
	 *            if <code>true</code> members below compilation units and
	 *            class files are provided.
	 */
	public StandardErlangElementContentProvider(boolean provideMembers) {
		fProvideMembers = provideMembers;
	}

	/**
	 * Returns whether members are provided when asking for a compilation units
	 * or class file for its children.
	 * 
	 * @return <code>true</code> if the content provider provides members;
	 *         otherwise <code>false</code> is returned
	 */
	public boolean getProvideMembers() {
		return fProvideMembers;
	}

	/**
	 * Sets whether the content provider is supposed to return members when
	 * asking a compilation unit or class file for its children.
	 * 
	 * @param b
	 *            if <code>true</code> then members are provided. If
	 *            <code>false</code> compilation units and class files are the
	 *            leaves provided by this content provider.
	 */
	public void setProvideMembers(boolean b) {
		// hello
		fProvideMembers = b;
	}

	/*
	 * (non-Javadoc) Method declared on IStructuredContentProvider.
	 */
	public Object[] getElements(Object parent) {
		return getChildren(parent);
	}

	/*
	 * (non-Javadoc) Method declared on IContentProvider.
	 */
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
	}

	/*
	 * (non-Javadoc) Method declared on IContentProvider.
	 */
	public void dispose() {
	}

	/*
	 * (non-Javadoc) Method declared on ITreeContentProvider.
	 */
	public Object[] getChildren(Object element) {
		if (!exists(element)) {
			return NO_CHILDREN;
		}

		try {
			if (element instanceof IErlModel) {
				return getErlangProjects((IErlModel) element);
			}

			if (element instanceof IErlProject) {
				return getPackages((IErlProject) element);
			}

			if (element instanceof IFolder) {
				return getResources((IFolder) element);
			}

			if (getProvideMembers() && element instanceof ISourceReference
					&& element instanceof IParent) {
				return ((IParent) element).getChildren();
			}
		} catch (final ErlModelException e) {
			return NO_CHILDREN;
		}
		return NO_CHILDREN;
	}

	private Object[] getPackages(IErlProject project) {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see ITreeContentProvider
	 */
	public boolean hasChildren(Object element) {
		if (getProvideMembers()) {
			// assume CUs and class files are never empty
			if (element instanceof IErlModule) {
				return true;
			}
		} else {
			// don't allow to drill down into a compilation unit or class file
			if (element instanceof IErlModule || element instanceof IFile) {
				return false;
			}
		}

		if (element instanceof IErlProject) {
			final IErlProject jp = (IErlProject) element;
			if (!jp.getProject().isOpen()) {
				return false;
			}
		}

		if (element instanceof IParent) {
			// when we have Erlang children return true, else we fetch all the
			// children
			if (((IParent) element).hasChildren()) {
				return true;
			}
		}
		final Object[] children = getChildren(element);
		return (children != null) && children.length > 0;
	}

	/*
	 * (non-Javadoc) Method declared on ITreeContentProvider.
	 */
	public Object getParent(Object element) {
		if (!exists(element)) {
			return null;
		}
		return internalGetParent(element);
	}

	/**
	 * Note: This method is for internal use only. Clients should not call this
	 * method.
	 */
	protected Object[] getErlangProjects(IErlModel jm) throws ErlModelException {
		return jm.getErlangProjects();
	}

	private Object[] getResources(IFolder folder) {
		/*
		 * try { IResource[] members = folder.members(); IErlProject erlProject =
		 * ErlangPlugin.create(folder.getProject()); if (erlProject == null ||
		 * !erlProject.exists()) return members; boolean isFolderOnClasspath =
		 * erlProject.isOnClasspath(folder); List nonErlangResources = new
		 * ArrayList(); // Can be on classpath but as a member of non-erlang
		 * resource folder for (int i = 0; i < members.length; i++) { IResource
		 * member = members[i]; // A resource can also be an erlang element //
		 * in the case of exclusion and inclusion filters. // We therefore
		 * exclude Erlang elements from the list // of non-Erlang resources. if
		 * (isFolderOnClasspath) { if
		 * (erlProject.findApplication(member.getFullPath()) == null) {
		 * nonErlangResources.add(member); } } else if
		 * (!erlProject.isOnClasspath(member)) { nonErlangResources.add(member); } }
		 * return nonErlangResources.toArray(); } catch (CoreException e) {
		 * return NO_CHILDREN; }
		 */
		return NO_CHILDREN;
	}

	/**
	 * Note: This method is for internal use only. Clients should not call this
	 * method.
	 */
	protected boolean isApplicationEmpty(IErlElement element)
			throws ErlModelException {
		/*
		 * if (element instanceof IErlApplication) { IErlApplication app =
		 * (IErlApplication) element; if (app.exists() && !(app.hasChildren() ||
		 * app.getNonErlangResources().length > 0) && app.hasSubpackages())
		 * return true; }
		 */return false;
	}

	/**
	 * Note: This method is for internal use only. Clients should not call this
	 * method.
	 */
	protected boolean exists(Object element) {
		if (element == null) {
			return false;
		}
		if (element instanceof IResource) {
			return ((IResource) element).exists();
		}
		if (element instanceof IErlElement) {
			return ((IErlElement) element).exists();
		}
		return true;
	}

	/**
	 * Note: This method is for internal use only. Clients should not call this
	 * method.
	 */
	protected Object internalGetParent(Object element) {
		if (element instanceof IResource) {
			final IResource parent = ((IResource) element).getParent();
			final IErlElement eParent = ErlangCore.getModelManager().create(parent);
			if (eParent != null && eParent.exists()) {
				return eParent;
			}
			return parent;
		} else if (element instanceof IErlElement) {
			final IErlElement parent = ((IErlElement) element).getParent();
			// for package fragments that are contained in a project package
			// fragment
			// we have to skip the package fragment root as the parent.
			return parent;
		}
		return null;
	}

	/**
	 * Note: This method is for internal use only. Clients should not call this
	 * method.
	 */
	protected static Object[] concatenate(Object[] a1, Object[] a2) {
		final int a1Len = a1.length;
		final int a2Len = a2.length;
		final Object[] res = new Object[a1Len + a2Len];
		System.arraycopy(a1, 0, res, 0, a1Len);
		System.arraycopy(a2, 0, res, a1Len, a2Len);
		return res;
	}

}