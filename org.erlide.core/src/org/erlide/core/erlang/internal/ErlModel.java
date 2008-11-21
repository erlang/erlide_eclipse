/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.core.erlang.internal;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModelChangeListener;
import org.erlide.core.erlang.IErlModelManager;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IOpenable;
import org.erlide.core.erlang.IParent;
import org.erlide.core.util.ErlideUtil;

/**
 * Implementation of
 * <code>IErlModel<code>. The Erlang Model maintains a cache of
 * active <code>IErlProject</code>s in a workspace. A Erlang Model is specific
 * to a workspace. To retrieve a workspace's model, use the
 * <code>#getErlangModel(IWorkspace)</code> method.
 * 
 * @see IErlModel
 */
public class ErlModel extends Openable implements IErlModel {
	/**
	 * A array with all the non-erlang projects contained by this model
	 */
	private IProject[] nonErlangProjects;

	private final ArrayList<IErlModelChangeListener> fListeners = new ArrayList<IErlModelChangeListener>(
			5);

	/**
	 * Constructs a new Erlang Model on the given workspace. Note that only one
	 * instance of ErlModel handle should ever be created. One should only
	 * indirect through ErlModelManager#getErlangModel() to get access to it.
	 * 
	 * @exception Error
	 *                if called more than once
	 */
	protected ErlModel() {
		super(null, ""); //$NON-NLS-1$
	}

	@Override
	protected boolean buildStructure(final IProgressMonitor pm,
			final IResource underlyingResource)
	// throws ErlModelException
	{
		logBuildStructure(underlyingResource);
		final IErlModelManager modelManager = ErlangCore.getModelManager();
		// determine my children
		final IProject[] projects = ResourcesPlugin.getWorkspace().getRoot()
				.getProjects();
		for (final IProject project : projects) {
			if (ErlideUtil.hasErlangNature(project)) {
				addChild(modelManager.create(project, this));
			}
		}

		return true;
	}

	// /*
	// * @see IErlModel
	// */
	// public boolean contains(final IResource resource) {
	// switch (resource.getType()) {
	// case IResource.ROOT:
	// case IResource.PROJECT:
	// return true;
	// }
	// // file or folder
	// List<IErlProject> projects;
	// try {
	// projects = getErlangProjects();
	// } catch (final ErlModelException e) {
	// return false;
	// }
	// for (final IErlProject element : projects) {
	// final ErlProject project = (ErlProject) element;
	// if (!project.contains(resource)) {
	// return false;
	// }
	// }
	// return true;
	// }

	/**
	 * @see IErlModel
	 */
	public void copy(final IErlElement[] elements,
			final IErlElement[] containers, final IErlElement[] siblings,
			final String[] renamings, final boolean force,
			final IProgressMonitor monitor) throws ErlModelException {
		// if (elements != null && elements.length > 0 && elements[0] != null
		// && elements[0].getElementType() < IErlElement.TYPE)
		// {
		// runOperation(new CopyResourceElementsOperation(elements, containers,
		// force),
		// elements,
		// siblings, renamings, monitor);
		// } else
		// {
		// runOperation(new CopyElementsOperation(elements, containers, force),
		// elements,
		// siblings, renamings, monitor);
		// }
	}

	/**
	 * @see IErlModel
	 */
	public void delete(final IErlElement[] elements, final boolean force,
			final IProgressMonitor monitor) throws ErlModelException {
		// if (elements != null && elements.length > 0 && elements[0] != null
		// && elements[0].getElementType() < IErlElement.TYPE)
		// {
		// new DeleteResourceElementsOperation(elements, force)
		// .runOperation(monitor);
		// }
		// else
		// {
		// new DeleteElementsOperation(elements, force).runOperation(monitor);
		// }
	}

	@Override
	public boolean equals(final Object o) {
		if (!(o instanceof ErlModel)) {
			return false;
		}
		return super.equals(o);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}

	// /**
	// * Finds the given project in the list of the Erlang model's children.
	// * Returns null if not found.
	// */
	// public IErlProject findErlangProject(final IProject project) {
	// try {
	// final List<IErlProject> projects = getErlangProjects();
	// for (final IErlProject erlangProject : projects) {
	// if (project.equals(erlangProject.getProject())) {
	// return erlangProject;
	// }
	// }
	// } catch (final ErlModelException e) {
	// // Erlang model doesn't exist: cannot find any project
	// }
	// return null;
	// }

	/**
	 * @see IErlElement
	 */
	public Kind getKind() {
		return Kind.MODEL;
	}

	/**
	 * @see ErlElement#getHandleMemento()
	 */
	public String getHandleMemento() {
		return getName();
	}

	/**
	 * Returns the <code>char</code> that marks the start of this handles
	 * contribution to a memento.
	 */
	protected char getHandleMementoDelimiter() {
		Assert.isTrue(false, "Should not be called"); //$NON-NLS-1$
		return 0;
	}

	/**
	 * @see IErlModel
	 */
	public IErlProject getErlangProject(final String projectName) {
		final IErlElement e = getChildNamed(projectName);
		if (e != null) {
			return (IErlProject) e;
		}
		return makeErlangProject(getWorkspace().getRoot().getProject(
				projectName));
	}

	public IErlProject makeErlangProject(final IProject project) {
		final IErlProject ep = new ErlProject(project, this);
		addChild(ep);
		return ep;
	}

	/**
	 * @see IErlModel
	 */
	@SuppressWarnings("unchecked")
	public List<IErlProject> getErlangProjects() throws ErlModelException {
		final ArrayList<IErlProject> list = (ArrayList<IErlProject>) getChildrenOfType(Kind.PROJECT);
		return list;
	}

	/*
	 * @see IErlElement
	 */
	public IResource getResource() {
		return ResourcesPlugin.getWorkspace().getRoot();
	}

	/**
	 * @see IOpenable
	 */
	@Override
	public IResource getUnderlyingResource() {
		return null;
	}

	/**
	 * Returns the workbench associated with this object.
	 */
	public IWorkspace getWorkspace() {
		return ResourcesPlugin.getWorkspace();
	}

	/**
	 * @see IErlModel
	 */
	public void move(final IErlElement[] elements,
			final IErlElement[] containers, final IErlElement[] siblings,
			final String[] renamings, final boolean force,
			final IProgressMonitor monitor) throws ErlModelException {
		// if (elements != null && elements.length > 0 && elements[0] != null
		// && elements[0].getElementType() < IErlElement.TYPE)
		// {
		// runOperation(new MoveResourceElementsOperation(elements,
		// containers, force), elements, siblings, renamings, monitor);
		// }
		// else
		// {
		// runOperation(
		// new MoveElementsOperation(elements, containers, force),
		// elements, siblings, renamings, monitor);
		// }
	}

	/**
	 * @see IErlModel
	 */
	public void rename(final IErlElement[] elements,
			final IErlElement[] destinations, final String[] renamings,
			final boolean force, final IProgressMonitor monitor)
			throws ErlModelException {
		// MultiOperation op;
		// if (elements != null && elements.length > 0 && elements[0] != null
		// && elements[0].getElementType() < IErlElement.TYPE)
		// {
		// op = new RenameResourceElementsOperation(elements, destinations,
		// renamings, force);
		// }
		// else
		// {
		// op = new RenameElementsOperation(elements, destinations, renamings,
		// force);
		// }
		//
		// op.runOperation(monitor);
	}

	// /**
	// * Configures and runs the <code>MultiOperation</code>.
	// */
	// protected void runOperation(MultiOperation op, IErlElement[] elements,
	// IErlElement[] siblings, String[] renamings,
	// IProgressMonitor monitor) throws ErlModelException
	// {
	// op.setRenamings(renamings);
	// if (siblings != null)
	// {
	// for (int i = 0; i < elements.length; i++)
	// {
	// op.setInsertBefore(elements[i], siblings[i]);
	// }
	// }
	// op.runOperation(monitor);
	// }

	/**
	 * @private Debugging purposes
	 */
	@Override
	protected void toStringInfo(final int tab, final StringBuilder buffer,
			final Object info) {
		buffer.append(tabString(tab));
		buffer.append("Erlang Model"); //$NON-NLS-1$
		if (info == null) {
			buffer.append(" (not open)"); //$NON-NLS-1$
		}
	}

	/**
	 * Helper method - returns the targeted item (IResource if internal or
	 * java.io.File if external), or null if unbound Internal items must be
	 * referred to using container relative paths.
	 */
	public static Object getTarget(final IContainer container,
			final IPath path, boolean checkResourceExistence) {

		if (path == null) {
			return null;
		}

		// lookup - inside the container
		if (path.getDevice() == null) { // container relative paths should not
			// contain a device
			// (see http://dev.eclipse.org/bugs/show_bug.cgi?id=18684)
			// (case of a workspace rooted at d:\ )
			final IResource resource = container.findMember(path);
			if (resource != null) {
				if (!checkResourceExistence || resource.exists()) {
					return resource;
				}
				return null;
			}
		}

		// if path is relative, it cannot be an external path
		// (see http://dev.eclipse.org/bugs/show_bug.cgi?id=22517)
		if (!path.isAbsolute()) {
			return null;
		}

		// lookup - outside the container
		final File externalFile = new File(path.toOSString());
		if (!checkResourceExistence) {
			return externalFile;
		}
		if (externalFile.exists()) {
			return externalFile;
		}
		return null;
	}

	/**
	 * Compute the non-java resources contained in this java project.
	 */
	private IProject[] computeNonErlangResources() {
		final IProject[] projects = ResourcesPlugin.getWorkspace().getRoot()
				.getProjects();
		final int length = projects.length;
		IProject[] resources = null;
		int index = 0;
		for (int i = 0; i < length; i++) {
			final IProject project = projects[i];
			if (!ErlideUtil.hasErlangNature(project)) {
				if (resources == null) {
					resources = new IProject[length];
				}
				resources[index++] = project;
			}
		}
		if (index == 0) {
			return NO_NON_ERLANG_RESOURCES;
		}
		if (index < length) {
			System.arraycopy(resources, 0, resources = new IProject[index], 0,
					index);
		}
		return resources;
	}

	/**
	 * Returns an array of non-java resources contained in the receiver.
	 */
	public IProject[] getNonErlangResources() throws ErlModelException {

		if (nonErlangProjects == null) {
			nonErlangProjects = computeNonErlangResources();
		}
		return nonErlangProjects;
	}

	public void notifyChange(final IErlElement element) {
		// ErlLogger.debug("^> notifying change of " +
		// element.getElementName());
		for (int i = 0; i < fListeners.size(); i++) {
			fListeners.get(i).elementChanged(element);
		}
	}

	public void addModelChangeListener(final IErlModelChangeListener listener) {
		if (!fListeners.contains(listener)) {
			fListeners.add(listener);
		}
	}

	public void removeModelChangeListener(final IErlModelChangeListener listener) {
		fListeners.remove(listener);
	}

	public boolean isVisibleInOutline() {
		return false;
	}

	@Override
	protected void closing(final Object info) throws ErlModelException {
		// TODO Auto-generated method stub
	}

	public IErlElement findElement(final IResource rsrc) {
		return findElement(rsrc, false);
	}

	public IErlElement findElement(final IResource rsrc,
			final boolean openElements) {
		final IPath path = rsrc.getFullPath();
		IParent p = this;
		for (final String segment : path.segments()) {
			final IErlElement c = p.getChildNamed(segment);
			if (c == null) {
				return null;
			}
			if (openElements) {
				if (c instanceof IOpenable) {
					final IOpenable o = (IOpenable) c;
					try {
						o.open(null);
					} catch (final ErlModelException e) {
						e.printStackTrace();
						return null;
					}
				}
			}
			final IResource resource = c.getResource();
			if (resource != null && resource.equals(rsrc)) {
				return c;
			}
			p = (IParent) c;
		}
		return null;
	}

	public IErlModule getModule(final IFile file) {
		IErlElement element = findElement(file, false);
		if (element == null) {
			// Ok, if it's not found, we'll try to build it...
			element = findElement(file, true);
		}
		return (IErlModule) element;
	}

	public IErlProject findProject(final IProject project) {
		final IErlElement e = findElement(project);
		return (IErlProject) e;
	}

	public IErlModule getModule(final String name) {
		for (final IErlElement e : getChildren()) {
			if (e instanceof IErlProject) {
				final IErlProject p = (IErlProject) e;
				try {
					final IErlModule m = p.getModule(name);
					if (m != null) {
						return m;
					}
				} catch (final ErlModelException e1) {
					e1.printStackTrace();
				}
			}
		}
		return null;
	}
}
