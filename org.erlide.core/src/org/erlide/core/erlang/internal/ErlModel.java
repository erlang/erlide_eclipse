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

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModelChangeListener;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IOpenable;
import org.erlide.core.erlang.util.Assert;
import org.erlide.core.erlang.util.Util;

/**
 * Implementation of
 * <code>IErlModel<code>. The Erlang Model maintains a cache of
 * active <code>IErlProject</code>s in a workspace. A Erlang Model is specific to a
 * workspace. To retrieve a workspace's model, use the
 * <code>#getErlangModel(IWorkspace)</code> method.
 *
 * @see IErlModel
 */
public class ErlModel extends Openable implements IErlModel {

	/**
	 * A array with all the non-java projects contained by this model
	 */
	private IProject[] nonJavaResources;

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
	protected boolean buildStructure(IProgressMonitor pm,
			IResource underlyingResource)
	// throws ErlModelException
	{
		// determine my children
		final IProject[] projects = ResourcesPlugin.getWorkspace().getRoot()
				.getProjects();
		for (final IProject project : projects) {
			if (ErlangCore.hasErlangNature(project)) {
				addChild(ErlangCore.getModelManager().create(project));
			}
		}

		return true;
	}

	/*
	 * @see IErlModel
	 */
	public boolean contains(IResource resource) {
		switch (resource.getType()) {
		case IResource.ROOT:
		case IResource.PROJECT:
			return true;
		}
		// file or folder
		IErlProject[] projects;
		try {
			projects = this.getErlangProjects();
		} catch (final ErlModelException e) {
			return false;
		}
		for (IErlProject element : projects) {
			final ErlProject project = (ErlProject) element;
			if (!project.contains(resource)) {
				return false;
			}
		}
		return true;
	}

	/**
	 * @see IErlModel
	 */
	public void copy(IErlElement[] elements, IErlElement[] containers,
			IErlElement[] siblings, String[] renamings, boolean force,
			IProgressMonitor monitor) throws ErlModelException {
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
	public void delete(IErlElement[] elements, boolean force,
			IProgressMonitor monitor) throws ErlModelException {
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
	public boolean equals(Object o) {
		if (!(o instanceof ErlModel)) {
			return false;
		}
		return super.equals(o);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}

	/**
	 * Finds the given project in the list of the Erlang model's children.
	 * Returns null if not found.
	 */
	public IErlProject findErlangProject(IProject project) {
		try {
			final IErlProject[] projects = this.getErlangProjects();
			for (final IErlProject erlangProject : projects) {
				if (project.equals(erlangProject.getProject())) {
					return erlangProject;
				}
			}
		} catch (final ErlModelException e) {
			// Erlang model doesn't exist: cannot find any project
		}
		return null;
	}

	/**
	 * @see IErlElement
	 */
	public ErlElementType getElementType() {
		return ErlElementType.MODEL;
	}

	/**
	 * @see ErlElement#getHandleMemento()
	 */
	public String getHandleMemento() {
		return getElementName();
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
	public IErlProject getErlangProject(String projectName) {
		for (IErlElement element : fChildren) {
			if (element.getElementName().equals(projectName)) {
				return (IErlProject) element;
			}
		}
		return makeErlangProject(getWorkspace().getRoot().getProject(
				projectName));
	}

	/**
	 * @exception IllegalArgumentException
	 *                if the given resource is not one of an IProject, IFolder,
	 *                or IFile.
	 */
	public IErlProject makeErlangProject(IResource resource) {
		switch (resource.getType()) {
		case IResource.FOLDER:
			return new ErlProject(((IFolder) resource).getProject(), this);
		case IResource.FILE:
			return new ErlProject(((IFile) resource).getProject(), this);
		case IResource.PROJECT:
			return new ErlProject((IProject) resource, this);
		default:
			throw new IllegalArgumentException(Util
					.bind("element.invalidResourceForProject")); //$NON-NLS-1$
		}
	}

	/**
	 * @see IErlModel
	 */
	@SuppressWarnings("unchecked")
	public IErlProject[] getErlangProjects() throws ErlModelException {
		final ArrayList<IErlProject> list = (ArrayList<IErlProject>) getChildrenOfType(ErlElementType.PROJECT);
		return list.toArray(new IErlProject[list.size()]);
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
	public void move(IErlElement[] elements, IErlElement[] containers,
			IErlElement[] siblings, String[] renamings, boolean force,
			IProgressMonitor monitor) throws ErlModelException {
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
	public void rename(IErlElement[] elements, IErlElement[] destinations,
			String[] renamings, boolean force, IProgressMonitor monitor)
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
	protected void toStringInfo(int tab, StringBuffer buffer, Object info) {
		buffer.append(this.tabString(tab));
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
	public static Object getTarget(IContainer container, IPath path,
			boolean checkResourceExistence) {

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
			if (!ErlangCore.hasErlangNature(project)) {
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

		if (nonJavaResources == null) {
			nonJavaResources = computeNonErlangResources();
		}
		return nonJavaResources;
	}

	public void notifyChange(IErlElement element) {
		// ErlLogger.debug("^> notifying change of " +
		// element.getElementName());
		for (int i = 0; i < fListeners.size(); i++) {
			(fListeners.get(i)).elementChanged(element);
		}
	}

	public void addModelChangeListener(IErlModelChangeListener listener) {
		if (!fListeners.contains(listener)) {
			fListeners.add(listener);
		}
	}

	public void removeModelChangeListener(IErlModelChangeListener listener) {
		fListeners.remove(listener);
	}

	public boolean isVisibleInOutline() {
		return false;
	}

	/**
	 * @see org.erlide.core.erlang.IErlModel#findFunction(java.lang.String,
	 *      java.lang.String, int)
	 */
	public IErlFunction[] findFunction(String project, String module,
			String function, int arity) {
		final ArrayList<IErlFunction> funs = new ArrayList<IErlFunction>(20);

		final IErlModule[] modules = findModule(project, module);
		for (final IErlModule mod : modules) {
			try {
				for (int j = 0; j < mod.getChildren().length; j++) {
					final IErlElement el = mod.getChildren()[j];
					if (el instanceof IErlFunction) {
						final IErlFunction fun = (IErlFunction) el;
						final boolean arityOk = (arity == UNKNOWN_ARITY)
								|| (arity == fun.getArity());
						if (arityOk && fun.getElementName().matches(function)) {
							funs.add(fun);
						}
					}
				}
			} catch (final ErlModelException e) {
				e.printStackTrace();
			}
		}

		return funs.toArray(new IErlFunction[funs.size()]);
	}

	public IErlModule[] findModule(String project, String module) {
		final ArrayList<IErlModule> mods = new ArrayList<IErlModule>(20);

		if (module == UNKNOWN_MODULE) {
			module = "*";
		}
		final IErlProject prj = getErlangProject(project);

		try {
			for (int i = 0; i < prj.getChildren().length; i++) {
				final IErlElement el = prj.getChildren()[i];
				if (el instanceof IErlModule) {
					final IErlModule mod = (IErlModule) el;
					if (mod.getElementName().matches(module)) {
						mods.add(mod);
					}
				}
			}
		} catch (final ErlModelException e) {
			e.printStackTrace();
		}

		return mods.toArray(new IErlModule[mods.size()]);
	}

}
