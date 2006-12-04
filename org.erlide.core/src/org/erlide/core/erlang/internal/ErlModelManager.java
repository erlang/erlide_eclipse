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

import java.io.BufferedInputStream;
// import java.io.BufferedOutputStream;
import java.io.DataInputStream;
// import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
// import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.ISaveContext;
import org.eclipse.core.resources.ISaveParticipant;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceDescription;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Preferences;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.core.runtime.Status;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.builder.ErlangBuilder;
import org.erlide.core.erlang.ErlElementDelta;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlElementDelta;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModelManager;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IRegion;
import org.erlide.core.erlang.util.ElementChangedEvent;
import org.erlide.core.erlang.util.IElementChangedListener;
import org.erlide.core.erlang.util.Util;

/**
 * The <code>ErlModelManager</code> manages instances of
 * <code>IErlModel</code>. <code>IElementChangedListener</code>s register
 * with the <code>ErlModelManager</code>, and receive
 * <code>ElementChangedEvent</code>s for all <code>IErlModel</code>s.
 * <p>
 * The single instance of <code>ErlModelManager</code> is available from the
 * static method <code>ErlModelManager.getErlangModelManager()</code>.
 */
public class ErlModelManager implements IErlModelManager {

	/**
	 * Unique handle onto the ErlModel
	 */
	final ErlModel erlangModel = new ErlModel();

	private final HashSet optionNames = new HashSet(20);

	private final Map<String, IErlElement> elements = new HashMap<String, IErlElement>(
			10);

	/**
	 * Queue of reconcile deltas on working copies that have yet to be fired.
	 * This is a table form IWorkingCopy to IErlElementDelta
	 */
	HashMap reconcileDeltas = new HashMap();

	/**
	 * The singleton manager
	 */
	private static final ErlModelManager MANAGER = new ErlModelManager();

	/**
	 * Queue of deltas created explicily by the model that have yet to be fired.
	 */
	List<IErlElementDelta> erlModelDeltas = Collections
			.synchronizedList(new ArrayList<IErlElementDelta>());

	public static final int DEFAULT_CHANGE_EVENT = 0; // must not collide with

	// ElementChangedEvent event masks

	/**
	 * Registers the given delta with this manager. This API is to be used to
	 * registerd deltas that are created explicitly by the C Model. Deltas
	 * created as translations of <code>IResourceDeltas</code> are to be
	 * registered with <code>#registerResourceDelta</code>.
	 */
	public void registerErlModelDelta(IErlElementDelta delta) {
		erlModelDeltas.add(delta);
		// TODO
	}

	/**
	 * Set of elements which are out of sync with their buffers.
	 */
	protected Map<IErlElement, IErlElement> elementsOutOfSynchWithBuffers = new HashMap<IErlElement, IErlElement>(
			11);

	public static boolean verbose = true;

	/**
	 * Turns delta firing on/off. By default it is on.
	 */
	protected boolean fFire = true;

	/**
	 * Listeners for element changes
	 */
	protected List<IElementChangedListener> elementChangedListeners = new ArrayList<IElementChangedListener>();

	/**
	 * Returns the Erlang element corresponding to the given resource, or
	 * <code>null</code> if unable to associate the given resource with a
	 * Erlang element.
	 * <p>
	 * The resource must be one of:
	 * <ul>
	 * <li>a project - the element returned is the corresponding
	 * <code>IErlProject</code> </li>
	 * <li>a <code>.erl</code> file - the element returned is the
	 * corresponding <code>IErlModule</code></li>
	 * <li>a <code>.beam</code> file - the element returned is the
	 * corresponding <code>IErlBeamFile</code></li>
	 * <li>a folder - the element returned is the corresponding
	 * <code>IErlPackage</code> -- not implemented yet</li>
	 * <li>the workspace root resource - the element returned is the
	 * <code>IErlModel</code></li>
	 * </ul>
	 * <p>
	 * Creating a Erlang element has the side effect of creating and opening all
	 * of the element's parents if they are not yet open.
	 */
	public IErlElement create(IResource resource, IErlProject project) {
		if (resource == null) {
			return null;
		}
		final int type = resource.getType();
		switch (type) {
		case IResource.PROJECT:
			return create((IProject) resource);
		case IResource.FILE:
			return create((IFile) resource, project);
		case IResource.FOLDER:
			return create((IFolder) resource, project);
		case IResource.ROOT:
			return create((IWorkspaceRoot) resource);
		default:
			return null;
		}
	}

	/**
	 * Returns the Erlang element corresponding to the given file, its project
	 * being the given project. Returns <code>null</code> if unable to
	 * associate the given file with a Erlang element.
	 * 
	 * <p>
	 * The file must be one of:
	 * <ul>
	 * <li>a <code>.erl</code> file - the element returned is the
	 * corresponding <code>IErlModule</code></li>
	 * <li>a <code>.beam</code> file - the element returned is the
	 * corresponding <code>IClassFile</code></li>
	 * </ul>
	 * <p>
	 * Creating a Erlang element has the side effect of creating and opening all
	 * of the element's parents if they are not yet open.
	 */
	public IErlElement create(IFile file, IErlProject project) {
		if (file == null) {
			return null;
		}
		if (project == null) {
			project = create(file.getProject());
		}

		if (file.getFileExtension() != null) {
			final String name = file.getName();
			if (Util.isErlangFileName(name)) {
				return MANAGER.createModuleFrom(file, project);
			}
		}
		return null;
	}

	/**
	 * Returns the package fragment or package fragment root corresponding to
	 * the given folder, its parent or great parent being the given project. or
	 * <code>null</code> if unable to associate the given folder with a Erlang
	 * element.
	 * <p>
	 * Note that a package fragment root is returned rather than a default
	 * package.
	 * <p>
	 * Creating a Erlang element has the side effect of creating and opening all
	 * of the element's parents if they are not yet open.
	 */
	public IErlElement create(IFolder folder, IErlProject project) {
		if (folder == null) {
			return null;
		}
		if (project == null) {
			project = create(folder.getProject());
		}
		final IErlElement element = null;
		// determineIfOnClasspath(folder, project);
		return element;
	}

	/**
	 * @see org.erlide.core.erlang.IErlModelManager#createModuleFrom(org.eclipse.core.resources.IFile,
	 *      org.erlide.core.erlang.IErlProject)
	 */
	public IErlModule createModuleFrom(IFile file, IErlProject project) {
		// System.out.println("createModuleFrom:: " + file + " " + project);
		if (file == null) {
			return null;
		}

		if (project == null) {
			project = create(file.getProject());
		}

		final String key = project.getElementName() + "/" + file.getName();
		if (elements.containsKey(key)) {
			return (IErlModule) elements.get(key);
		} else {
			final String ext = file.getFileExtension();
			if (ext.equals("erl") || ext.equals("hrl")) {
				final IErlModule module = new ErlModule(project,
						file.getName(), ext.equals("erl"));
				elements.put(key, module);
				return module;
			} else {
				return null;
			}
		}
	}

	/**
	 * @see org.erlide.core.erlang.IErlModelManager#createModuleFrom(org.eclipse.core.resources.IFile,
	 *      org.erlide.core.erlang.IErlProject)
	 */
	public IErlModule createModuleFrom(String name, String text,
			IErlProject project) {
		// System.out.println("createModuleFrom:: " + file + " " + project);
		if (name == null || text == null || project == null) {
			return null;
		}

		final String key = project.getElementName() + "/" + name;
		if (elements.containsKey(key)) {
			return (IErlModule) elements.get(key);
		}
		final IPath path = new Path(key);
		final String ext = path.getFileExtension();
		if (!(ext.equals("erl") || ext.equals("hrl"))) {
			return null;
		}
		final IErlModule module = new ErlModule(project, name, text, ext
				.equals("erl"));
		elements.put(key, module);
		return module;
	}

	/**
	 * Returns the Erlang project corresponding to the given project.
	 * <p>
	 * Creating a Erlang Project has the side effect of creating and opening all
	 * of the project's parents if they are not yet open.
	 * <p>
	 * Note that no check is done at this time on the existence or the Erlang
	 * nature of this project.
	 * 
	 * @param project
	 *            the given project
	 * @return the Erlang project corresponding to the given project, null if
	 *         the given project is null
	 */
	public IErlProject create(IProject project) {
		if (project == null) {
			return null;
		}

		return erlangModel.makeErlangProject(project);
	}

	/**
	 * Returns the Erlang element corresponding to the given file, or
	 * <code>null</code> if unable to associate the given file with a Erlang
	 * element.
	 * 
	 * <p>
	 * The file must be one of:
	 * <ul>
	 * <li>a <code>.Erlang</code> file - the element returned is the
	 * corresponding <code>IErlModule</code></li>
	 * <li>a <code>.class</code> file - the element returned is the
	 * corresponding <code>IClassFile</code></li>
	 * <li>a <code>.jar</code> file - the element returned is the
	 * corresponding <code>IPackageFragmentRoot</code></li>
	 * </ul>
	 * <p>
	 * Creating a Erlang element has the side effect of creating and opening all
	 * of the element's parents if they are not yet open.
	 * 
	 * @param file
	 *            the given file
	 * @return the Erlang element corresponding to the given file, or
	 *         <code>null</code> if unable to associate the given file with a
	 *         Erlang element
	 */
	public IErlElement create(IFile file) {
		return create(file, null/* unknown Erlang project */);
	}

	/**
	 * Returns the package fragment or package fragment root corresponding to
	 * the given folder, or <code>null</code> if unable to associate the given
	 * folder with a Erlang element.
	 * <p>
	 * Note that a package fragment root is returned rather than a default
	 * package.
	 * <p>
	 * Creating a Erlang element has the side effect of creating and opening all
	 * of the element's parents if they are not yet open.
	 * 
	 * @param folder
	 *            the given folder
	 * @return the package fragment or package fragment root corresponding to
	 *         the given folder, or <code>null</code> if unable to associate
	 *         the given folder with a Erlang element
	 */
	public IErlElement create(IFolder folder) {
		return create(folder, null/* unknown Erlang project */);
	}

	/**
	 * Returns the Erlang element corresponding to the given resource, or
	 * <code>null</code> if unable to associate the given resource with a
	 * Erlang element.
	 * <p>
	 * The resource must be one of:
	 * <ul>
	 * <li>a project - the element returned is the corresponding
	 * <code>IErlProject</code></li>
	 * <li>a <code>.Erlang</code> file - the element returned is the
	 * corresponding <code>IErlModule</code></li>
	 * <li>a <code>.class</code> file - the element returned is the
	 * corresponding <code>IClassFile</code></li>
	 * <li>a <code>.jar</code> file - the element returned is the
	 * corresponding <code>IPackageFragmentRoot</code></li>
	 * <li>a folder - the element returned is the corresponding
	 * <code>IPackageFragmentRoot</code> or <code>IPackageFragment</code>
	 * </li>
	 * <li>the workspace root resource - the element returned is the
	 * <code>IErlModel</code></li>
	 * </ul>
	 * <p>
	 * Creating a Erlang element has the side effect of creating and opening all
	 * of the element's parents if they are not yet open.
	 * 
	 * @param resource
	 *            the given resource
	 * @return the Erlang element corresponding to the given resource, or
	 *         <code>null</code> if unable to associate the given resource
	 *         with a Erlang element
	 */
	public IErlElement create(IResource resource) {
		return create(resource, null);
	}

	/**
	 * Returns the Erlang model.
	 * 
	 * @param root
	 *            the given root
	 * @return the Erlang model, or <code>null</code> if the root is null
	 */
	public IErlModel create(IWorkspaceRoot root) {
		if (root == null) {
			return null;
		}
		return getDefault().getErlangModel();
	}

	/**
	 * @see org.erlide.core.erlang.IErlModelManager#createModuleFrom(org.eclipse.core.resources.IFile)
	 */
	public IErlModule createModuleFrom(IFile file) {
		return createModuleFrom(file, null);
	}

	/**
	 * Table from IProject to PerProjectInfo. NOTE: this object itself is used
	 * as a lock to synchronize creation/removal of per project infos
	 */
	protected Map<IProject, PerProjectInfo> perProjectInfos = new HashMap<IProject, PerProjectInfo>(
			5);

	public class PerProjectInfo {

		public IProject fProject;

		public Object fSavedState;

		public boolean fTriedRead;

		public IPath outputLocation;

		public Preferences preferences;

		public PerProjectInfo(IProject project) {

			this.fTriedRead = false;
			this.fSavedState = null;
			this.fProject = project;
		}

		@Override
		public String toString() {
			final StringBuffer buffer = new StringBuffer();
			buffer.append("Info for "); //$NON-NLS-1$
			buffer.append(this.fProject.getFullPath());
			// buffer.append("\nRaw classpath:\n"); //$NON-NLS-1$
			// if (this.rawClasspath == null)
			// {
			// buffer.append(" <null>\n"); //$NON-NLS-1$
			// }
			// else
			// {
			// for (int i = 0, length = this.rawClasspath.length; i < length;
			// i++)
			// {
			// buffer.append(" "); //$NON-NLS-1$
			// buffer.append(this.rawClasspath[i]);
			// buffer.append('\n');
			// }
			// }
			// buffer.append("Resolved classpath:\n"); //$NON-NLS-1$
			// IClasspathEntry[] resolvedCP = this.resolvedClasspath;
			// if (resolvedCP == null)
			// {
			// buffer.append(" <null>\n"); //$NON-NLS-1$
			// }
			// else
			// {
			// for (int i = 0, length = resolvedCP.length; i < length; i++)
			// {
			// buffer.append(" "); //$NON-NLS-1$
			// buffer.append(resolvedCP[i]);
			// buffer.append('\n');
			// }
			// }
			buffer.append("Output location:\n  "); //$NON-NLS-1$
			if (this.outputLocation == null) {
				buffer.append("<null>"); //$NON-NLS-1$
			} else {
				buffer.append(this.outputLocation);
			}
			return buffer.toString();
		}
	}

	// public static boolean CP_RESOLVE_VERBOSE = false;

	/**
	 * Update the classpath variable cache
	 */
	public static class PluginPreferencesListener implements
			Preferences.IPropertyChangeListener {

		/**
		 * @see org.eclipse.core.runtime.Preferences.IPropertyChangeListener#propertyChange(Preferences.PropertyChangeEvent)
		 */
		public void propertyChange(Preferences.PropertyChangeEvent event) {

			// String propertyName = event.getProperty();
		}
	}

	/**
	 * Constructs a new ErlModelManager
	 */
	private ErlModelManager() {
		// singleton: prevent others from creating a new instance
	}

	/**
	 * @see ISaveParticipant
	 */
	public void doneSaving(ISaveContext context) {
		// nothing to do
	}

	/**
	 * Returns the set of elements which are out of synch with their buffers.
	 */
	public Map<IErlElement, IErlElement> getElementsOutOfSynchWithBuffers() {
		return this.elementsOutOfSynchWithBuffers;
	}

	/**
	 * @see org.erlide.core.erlang.IErlModelManager#getInfo(org.erlide.core.erlang.IErlElement)
	 */
	public synchronized Object getInfo(IErlElement element) {
		return element;
	}

	/**
	 * @see org.erlide.core.erlang.IErlModelManager#getErlangModel()
	 */
	public final ErlModel getErlangModel() {
		return erlangModel;
	}

	/**
	 * Returns the singleton ErlModelManager
	 */
	public static final IErlModelManager getDefault() {
		return MANAGER;
	}

	/**
	 * @see org.erlide.core.erlang.IErlModelManager#getLastBuiltState(org.eclipse.core.resources.IProject,
	 *      org.eclipse.core.runtime.IProgressMonitor)
	 */
	public Object getLastBuiltState(IProject project, IProgressMonitor monitor) {
		if (!ErlangCore.hasErlangNature(project)) {
			return null;
		}

		final PerProjectInfo info = getPerProjectInfo(project, true);
		// create if missing
		if (!info.fTriedRead) {
			info.fTriedRead = true;
			try {
				if (monitor != null) {
					monitor.subTask(Util.bind(
							"build.readStateProgress", project.getName())); //$NON-NLS-1$
				}
				info.fSavedState = readState(project);
			} catch (final CoreException e) {
				e.printStackTrace();
			}
		}
		return info.fSavedState;
	}

	/*
	 * Returns the per-project info for the given project. If specified, create
	 * the info if the info doesn't exist.
	 */
	/**
	 * @see org.erlide.core.erlang.IErlModelManager#getPerProjectInfo(org.eclipse.core.resources.IProject,
	 *      boolean)
	 */
	public PerProjectInfo getPerProjectInfo(IProject project, boolean create) {
		synchronized (perProjectInfos) {
			PerProjectInfo info = (PerProjectInfo) perProjectInfos.get(project);
			if (info == null && create) {
				info = new PerProjectInfo(project);
				perProjectInfos.put(project, info);
			}
			return info;
		}
	}

	/*
	 * Returns the per-project info for the given project. If the info doesn't
	 * exist, check for the project existence and create the info. @throws
	 * ErlModelException if the project doesn't exist.
	 */
	/**
	 * @see org.erlide.core.erlang.IErlModelManager#getPerProjectInfoCheckExistence(org.eclipse.core.resources.IProject)
	 */
	public PerProjectInfo getPerProjectInfoCheckExistence(IProject project)
			throws ErlModelException {
		PerProjectInfo info = getPerProjectInfo(project, false);
		if (info == null) {
			if (!ErlangCore.hasErlangNature(project)) {
				throw ((ErlProject) create(project)).newNotPresentException();
			}
			info = getPerProjectInfo(project, true);
		}
		return info;
	}

	/**
	 * Returns the File to use for saving and restoring the last built state for
	 * the given project.
	 */
	private File getSerializationFile(IProject project) {
		if (!project.exists()) {
			return null;
		}
		final IPath workingLocation = project
				.getWorkingLocation(ErlangPlugin.PLUGIN_ID);
		return workingLocation.append("state.dat").toFile(); //$NON-NLS-1$
	}

	/**
	 * @see org.erlide.core.erlang.IErlModelManager#prepareToSave(org.eclipse.core.resources.ISaveContext)
	 */
	public void prepareToSave(ISaveContext context) /* throws CoreException */
	{
		// nothing to do
	}

	/*
	 * Puts the infos in the given map (keys are IErlangElements and values are
	 * ErlangElementInfos) in the Erlang model cache in an atomic way. First
	 * checks that the info for the opened element (or one of its ancestors) has
	 * not been added to the cache. If it is the case, another thread has opened
	 * the element (or one of its ancestors). So returns without updating the
	 * cache.
	 */
	protected synchronized void putInfos(IErlElement openedElement,
			Map newElements) {
		// remove children
		// Object existingInfo = this.cache.peekAtInfo(openedElement);
		// if (openedElement instanceof IParent
		// && existingInfo instanceof ErlElementInfo)
		// {
		// IErlElement[] children = ((ErlElementInfo) existingInfo)
		// .getChildren();
		// for (int i = 0, size = children.length; i < size; ++i)
		// {
		// ErlElement child = (ErlElement) children[i];
		// try
		// {
		// child.close();
		// }
		// catch (ErlModelException e)
		// {
		// // ignore
		// }
		// }
		// }
		//
		// Iterator iterator = newElements.keySet().iterator();
		// while (iterator.hasNext())
		// {
		// IErlElement element = (IErlElement) iterator.next();
		// Object info = newElements.get(element);
		// this.cache.putInfo(element, info);
		// }
	}

	/**
	 * Reads the build state for the relevant project.
	 */
	protected Object readState(IProject project) throws CoreException {
		final File file = getSerializationFile(project);
		if (file != null && file.exists()) {
			try {
				final DataInputStream in = new DataInputStream(
						new BufferedInputStream(new FileInputStream(file)));
				try {
					final String pluginID = in.readUTF();
					if (!pluginID.equals(ErlangPlugin.PLUGIN_ID)) {
						throw new IOException(Util
								.bind("build.wrongFileFormat")); //$NON-NLS-1$
					}
					final String kind = in.readUTF();
					if (!kind.equals("STATE")) {
						throw new IOException(Util
								.bind("build.wrongFileFormat")); //$NON-NLS-1$
					}
					if (in.readBoolean()) {
						return ErlangBuilder.readState(project, in);
					}
					if (ErlangBuilder.DEBUG) {
						System.out
								.println("Saved state thinks last build failed for "
										+ project.getName());
					}
				} finally {
					in.close();
				}
			} catch (final IOException e) {
				e.printStackTrace();
				throw new CoreException(
						new Status(
								IStatus.ERROR,
								ErlangPlugin.PLUGIN_ID,
								Platform.PLUGIN_ERROR,
								"Error reading last build state for project " + project.getName(), e)); //$NON-NLS-1$
			}
		}
		return null;
	}

	/*
	 * Removes all cached info for the given element (including all children)
	 * from the cache. Returns the info for the given element, or null if it was
	 * closed.
	 */
	/**
	 * @see org.erlide.core.erlang.IErlModelManager#removeInfoAndChildren(org.erlide.core.erlang.internal.ErlElement)
	 */
	public synchronized Object removeInfoAndChildren(ErlElement element)
			throws ErlModelException {
		// Object info = this.cache.peekAtInfo(element);
		// if (info != null)
		// {
		// boolean wasVerbose = false;
		// try
		// {
		// if (verbose)
		// {
		// System.out
		// .println("CLOSING Element (" + Thread.currentThread() + "): " +
		// element.toStringWithAncestors()); //$NON-NLS-1$//$NON-NLS-2$
		// wasVerbose = true;
		// verbose = false;
		// }
		// element.closing(info);
		// if (element instanceof IParent
		// && info instanceof ErlElementInfo)
		// {
		// IErlElement[] children = ((ErlElementInfo) info)
		// .getChildren();
		// for (int i = 0, size = children.length; i < size; ++i)
		// {
		// ErlElement child = (ErlElement) children[i];
		// child.close();
		// }
		// }
		// this.cache.removeInfo(element);
		// if (wasVerbose)
		// {
		// System.out
		// .println("-> Package cache size = " + this.cache.pkgSize());
		// //$NON-NLS-1$
		// System.out
		// .println("-> Openable cache filling ratio = " +
		// NumberFormat.getInstance().format(this.cache.openableFillingRatio())
		// + "%"); //$NON-NLS-1$//$NON-NLS-2$
		// }
		// }
		// finally
		// {
		// ErlModelManager.verbose = wasVerbose;
		// }
		// return info;
		// }
		return null;
	}

	/**
	 * @see org.erlide.core.erlang.IErlModelManager#removePerProjectInfo(org.erlide.core.erlang.internal.ErlProject)
	 */
	public void removePerProjectInfo(IErlProject erlangProject) {
		synchronized (perProjectInfos) { // use the perProjectInfo collection
			// as its own lock
			final IProject project = erlangProject.getProject();
			final PerProjectInfo info = (PerProjectInfo) perProjectInfos
					.get(project);
			if (info != null) {
				perProjectInfos.remove(project);
			}
		}
	}

	/**
	 * @see ISaveParticipant
	 */
	public void rollback(ISaveContext context) {
		// nothing to do
	}

	/* Unused */
	/*
	 * private void saveState(PerProjectInfo info, ISaveContext context) throws
	 * CoreException {
	 *  // passed this point, save actions are non trivial if (context.getKind() ==
	 * ISaveContext.SNAPSHOT) { return; }
	 *  // save built state if (info.fTriedRead) { saveBuiltState(info); } }
	 */

	/**
	 * Saves the built state for the project.
	 */
	/* Unused */
	/*
	 * private void saveBuiltState(PerProjectInfo info) throws CoreException { //
	 * if (ErlangBuilder.DEBUG) // System.out.println(Util.bind( //
	 * "build.saveStateProgress", info.project.getName())); //$NON-NLS-1$ final
	 * File file = getSerializationFile(info.fProject); if (file == null) {
	 * return; } // long t = System.currentTimeMillis(); try { final
	 * DataOutputStream out = new DataOutputStream( new BufferedOutputStream(new
	 * FileOutputStream(file))); try { out.writeUTF(ErlangPlugin.PLUGIN_ID);
	 * out.writeUTF("STATE"); //$NON-NLS-1$ if (info.fSavedState == null) {
	 * out.writeBoolean(false); } else { out.writeBoolean(true); //
	 * ErlangBuilder.writeState(info.savedState, out); } } finally {
	 * out.close(); } } catch (final RuntimeException e) { try { file.delete(); }
	 * catch (final SecurityException se) { // could not delete file: cannot do
	 * much more } throw new CoreException( new Status( IStatus.ERROR,
	 * ErlangPlugin.PLUGIN_ID, Platform.PLUGIN_ERROR, Util .bind(
	 * "build.cannotSaveState", info.fProject.getName()), e)); //$NON-NLS-1$ }
	 * catch (final IOException e) { try { file.delete(); } catch (final
	 * SecurityException se) { // could not delete file: cannot do much more }
	 * throw new CoreException( new Status( IStatus.ERROR,
	 * ErlangPlugin.PLUGIN_ID, Platform.PLUGIN_ERROR, Util .bind(
	 * "build.cannotSaveState", info.fProject.getName()), e)); //$NON-NLS-1$ } //
	 * if (ErlangBuilder.DEBUG) // { // t = System.currentTimeMillis() - t; //
	 * System.out.println(Util.bind( // "build.saveStateComplete",
	 * String.valueOf(t))); //$NON-NLS-1$ // } }
	 */

	/**
	 * @see ISaveParticipant
	 */
	public void saving(ISaveContext context) throws CoreException {

		// // save container values on snapshot/full save
		// Preferences preferences =
		// ErlangCore.getPlugin().getPluginPreferences();
		// IErlProject[] projects = getErlangModel().getErlangProjects();
		// for (int i = 0, length = projects.length; i < length; i++)
		// {
		// IErlProject project = projects[i];
		// // clone while iterating (see
		// // https://bugs.eclipse.org/bugs/show_bug.cgi?id=59638)
		// Map projectContainers = containerClone(project);
		// if (projectContainers == null)
		// continue;
		// for (Iterator keys = projectContainers.keySet().iterator(); keys
		// .hasNext();)
		// {
		// IPath containerPath = (IPath) keys.next();
		// IClasspathContainer container = (IClasspathContainer)
		// projectContainers
		// .get(containerPath);
		// String containerKey = CP_CONTAINER_PREFERENCES_PREFIX
		// + project.getElementName() + "|" + containerPath;//$NON-NLS-1$
		// String containerString = CP_ENTRY_IGNORE;
		// try
		// {
		// if (container != null)
		// {
		// containerString = ((ErlProject) project)
		// .encodeClasspath(container
		// .getClasspathEntries(), null, false);
		// }
		// }
		// catch (ErlModelException e)
		// {
		// // could not encode entry: leave it as CP_ENTRY_IGNORE
		// }
		// preferences.setDefault(containerKey, CP_ENTRY_IGNORE); // use
		// // this
		// // default
		// // to get
		// // rid of
		// // removed
		// // ones
		// preferences.setValue(containerKey, containerString);
		// }
		// }
		// ErlangCore.getPlugin().savePluginPreferences();
		//
		// if (context.getKind() == ISaveContext.FULL_SAVE)
		// {
		// // will need delta since this save (see
		// // https://bugs.eclipse.org/bugs/show_bug.cgi?id=38658)
		// context.needDelta();
		//
		// // clean up indexes on workspace full save
		// // (see https://bugs.eclipse.org/bugs/show_bug.cgi?id=52347)
		// IndexManager manager = this.indexManager;
		// if (manager != null)
		// {
		// manager.cleanUpIndexes();
		// }
		// }
		//
		// IProject savedProject = context.getProject();
		// if (savedProject != null)
		// {
		// if (!ErlProject.hasErlangNature(savedProject))
		// return; // ignore
		// PerProjectInfo info = getPerProjectInfo(savedProject, true /*
		// * create
		// * info
		// */);
		// saveState(info, context);
		// return;
		// }
		//
		// ArrayList vStats = null; // lazy initialized
		// for (Iterator iter = perProjectInfos.values().iterator(); iter
		// .hasNext();)
		// {
		// try
		// {
		// PerProjectInfo info = (PerProjectInfo) iter.next();
		// saveState(info, context);
		// }
		// catch (CoreException e)
		// {
		// if (vStats == null)
		// vStats = new ArrayList();
		// vStats.add(e.getStatus());
		// }
		// }
		// if (vStats != null)
		// {
		// IStatus[] stats = new IStatus[vStats.size()];
		// vStats.toArray(stats);
		// throw new CoreException(new MultiStatus(ErlangCore.PLUGIN_ID,
		// IStatus.ERROR, stats,
		// Util.bind("build.cannotSaveStates"), null)); //$NON-NLS-1$
		// }
	}

	/**
	 * Record the order in which to build the Erlang projects (batch build).
	 * This order is based on the projects classpath settings.
	 */
	protected void setBuildOrder(String[] erlangBuildOrder)
			throws ErlModelException {

		// optional behaviour
		// possible value of index 0 is Compute
		if (!ErlangCore.COMPUTE.equals(ErlangPlugin
				.getOption(ErlangCore.CORE_ERLANG_BUILD_ORDER))) {
			return; // cannot be customized at project level
		}

		if (erlangBuildOrder == null || erlangBuildOrder.length <= 1) {
			return;
		}

		final IWorkspace workspace = ResourcesPlugin.getWorkspace();
		final IWorkspaceDescription description = workspace.getDescription();
		final String[] wksBuildOrder = description.getBuildOrder();

		String[] newOrder;
		if (wksBuildOrder == null) {
			newOrder = erlangBuildOrder;
		} else {
			// remove projects which are already mentionned in Erlang builder
			// order
			final int erlangCount = erlangBuildOrder.length;
			// create a set for fast check
			final HashMap<String, String> newSet = new HashMap<String, String>(
					erlangCount);
			for (int i = 0; i < erlangCount; i++) {
				newSet.put(erlangBuildOrder[i], erlangBuildOrder[i]);
			}
			int removed = 0;
			final int oldCount = wksBuildOrder.length;
			for (int i = 0; i < oldCount; i++) {
				if (newSet.containsKey(wksBuildOrder[i])) {
					wksBuildOrder[i] = null;
					removed++;
				}
			}
			// add Erlang ones first
			newOrder = new String[oldCount - removed + erlangCount];
			System.arraycopy(erlangBuildOrder, 0, newOrder, 0, erlangCount);
			// Erlang projects are built first

			// copy previous items in their respective order
			int index = erlangCount;
			for (int i = 0; i < oldCount; i++) {
				if (wksBuildOrder[i] != null) {
					newOrder[index++] = wksBuildOrder[i];
				}
			}
		}
		// commit the new build order out
		description.setBuildOrder(newOrder);
		try {
			workspace.setDescription(description);
		} catch (final CoreException e) {
			throw new ErlModelException(e);
		}
	}

	/**
	 * @see org.erlide.core.erlang.IErlModelManager#setLastBuiltState(org.eclipse.core.resources.IProject,
	 *      java.lang.Object)
	 */
	public void setLastBuiltState(IProject project, Object state) {
		if (ErlangCore.hasErlangNature(project)) {
			// should never be requested on non-Erlang projects
			final PerProjectInfo info = getPerProjectInfo(project, true);
			info.fTriedRead = true; // no point trying to re-read once using
			// setter
			info.fSavedState = state;
		}
		if (state == null) {
			// delete state file to ensure a full build happens if the workspace
			// crashes
			try {
				final File file = getSerializationFile(project);
				if (file != null && file.exists()) {
					file.delete();
				}
			} catch (final SecurityException se) {
				// could not delete file: cannot do much more
			}
		}
	}

	/**
	 * @see org.erlide.core.erlang.IErlModelManager#shutdown()
	 */
	public void shutdown() {
		// Note: no need to close the Erlang model as this just removes Erlang
		// element infos from the Erlang model cache
	}

	/**
	 * Adds the given listener for changes to Erlang elements. Has no effect if
	 * an identical listener is already registered. After completion of this
	 * method, the given listener will be registered for exactly the specified
	 * events. If they were previously registered for other events, they will be
	 * deregistered.
	 * <p>
	 * Once registered, a listener starts receiving notification of changes to
	 * Erlang elements in the model. The listener continues to receive
	 * notifications until it is replaced or removed.
	 * </p>
	 * <p>
	 * Listeners can listen for several types of event as defined in
	 * <code>ElementChangeEvent</code>. Clients are free to register for any
	 * number of event types however if they register for more than one, it is
	 * their responsibility to ensure they correctly handle the case where the
	 * same Erlang element change shows up in multiple notifications. Clients
	 * are guaranteed to receive only the events for which they are registered.
	 * </p>
	 * 
	 * @param listener
	 *            the listener
	 * @param eventMask
	 *            the bit-wise OR of all event types of interest to the listener
	 * @see IElementChangedListener
	 * @see ElementChangedEvent
	 * @see #removeElementChangedListener(IElementChangedListener)
	 */
	public void addElementChangedListener(IElementChangedListener listener,
			int eventMask) {
		// getDefault().addElementChangedListener(listener, eventMask);
	}

	/**
	 * Removes the given element changed listener. Has no affect if an identical
	 * listener is not registered.
	 * 
	 * @param listener
	 *            the listener
	 */
	public void removeElementChangedListener(IElementChangedListener listener) {
		// getDefault().removeElementChangedListener(listener);
	}

	/**
	 * Returns a new empty region.
	 * 
	 * @return a new empty region
	 */
	public IRegion newRegion() {
		return new Region();
	}

	/**
	 * Adds the given listener for changes to Erlang elements. Has no effect if
	 * an identical listener is already registered.
	 * 
	 * This listener will only be notified during the POST_CHANGE resource
	 * change notification and any reconcile operation (POST_RECONCILE). For
	 * finer control of the notification, use
	 * <code>addElementChangedListener(IElementChangedListener,int)</code>,
	 * which allows to specify a different eventMask.
	 * 
	 * @param listener
	 *            the listener
	 * @see ElementChangedEvent
	 */
	public void addElementChangedListener(IElementChangedListener listener) {
		addElementChangedListener(listener, ElementChangedEvent.POST_CHANGE
				| ElementChangedEvent.POST_RECONCILE);
	}

	/**
	 * @see org.erlide.core.erlang.IErlModelManager#getOptionNames()
	 */
	public HashSet getOptionNames() {
		return this.optionNames;
	}

	public IErlElement create(IPath path) {
		final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		// Assume it is fullpath relative to workspace
		IResource res = root.findMember(path);
		if (res == null) {
			final IPath rootPath = root.getLocation();
			if (path.equals(rootPath)) {
				return getDefault().getErlangModel();
			}
			res = root.getContainerForLocation(path);
			if (res == null || !res.exists()) {
				res = root.getFileForLocation(path);
			}
			if (res != null && !res.exists()) {
				res = null;
			}
		}

		// In case this is an external resource see if we can find
		// a file for it.
		if (res == null) {
			final IFile[] files = root.findFilesForLocation(path);
			if (files.length > 0) {
				res = files[0];
			}
		}

		return create(res, null);
	}

	public void fire(int eventType) {
		fire(null, eventType);
	}

	/**
	 * Fire C Model deltas, flushing them after the fact. If the firing mode has
	 * been turned off, this has no effect.
	 */
	void fire(IErlElementDelta customDeltas, int eventType) {
		if (fFire) {
			IErlElementDelta deltaToNotify;
			if (customDeltas == null) {
				deltaToNotify = mergeDeltas(this.erlModelDeltas);
			} else {
				deltaToNotify = customDeltas;
			}

			IElementChangedListener[] listeners;
			int listenerCount;
			int[] listenerMask;
			// Notification
			synchronized (elementChangedListeners) {
				listeners = new IElementChangedListener[elementChangedListeners
						.size()];
				elementChangedListeners.toArray(listeners);
				listenerCount = listeners.length;
				listenerMask = null;
			}

			switch (eventType) {
			case DEFAULT_CHANGE_EVENT:
				// firePreAutoBuildDelta(deltaToNotify, listeners, listenerMask,
				// listenerCount);
				firePostChangeDelta(deltaToNotify, listeners, listenerMask,
						listenerCount);
				fireReconcileDelta(listeners, listenerMask, listenerCount);
				break;
			// case ElementChangedEvent.PRE_AUTO_BUILD :
			// firePreAutoBuildDelta(deltaToNotify, listeners, listenerMask,
			// listenerCount);
			// break;
			case ElementChangedEvent.POST_CHANGE:
				firePostChangeDelta(deltaToNotify, listeners, listenerMask,
						listenerCount);
				fireReconcileDelta(listeners, listenerMask, listenerCount);
				break;
			case ElementChangedEvent.POST_RECONCILE:
				fireReconcileDelta(listeners, listenerMask, listenerCount);
				break;
			case ElementChangedEvent.POST_SHIFT:
				fireShiftEvent(deltaToNotify, listeners, listenerMask,
						listenerCount);
				return;
			}
		}
	}

	// private void firePreAutoBuildDelta(IErlElementDelta deltaToNotify,
	// IElementChangedListener[] listeners, int[] listenerMask, int
	// listenerCount) {
	//
	// if (verbose) {
	// System.out.println("FIRING PRE_AUTO_BUILD Delta [" +
	// Thread.currentThread() +
	// "]:"); //$NON-NLS-1$//$NON-NLS-2$
	// System.out.println(deltaToNotify == null ? "<NONE>" :
	// deltaToNotify.toString());
	// //$NON-NLS-1$
	// }
	// if (deltaToNotify != null) {
	// notifyListeners(deltaToNotify, ElementChangedEvent.PRE_AUTO_BUILD,
	// listeners,
	// listenerMask, listenerCount);
	// }
	// }

	private void firePostChangeDelta(IErlElementDelta deltaToNotify,
			IElementChangedListener[] listeners, int[] listenerMask,
			int listenerCount) {

		// post change deltas
		if (verbose) {
			System.out
					.println("FIRING POST_CHANGE Delta [" + Thread.currentThread() + "]:"); //$NON-NLS-1$//$NON-NLS-2$
			System.out
					.println(deltaToNotify == null ? "<NONE>" : deltaToNotify.toString()); //$NON-NLS-1$
		}
		if (deltaToNotify != null) {
			// flush now so as to keep listener reactions to post their own
			// deltas for
			// subsequent iteration
			this.flush();
			notifyListeners(deltaToNotify, ElementChangedEvent.POST_CHANGE,
					listeners, listenerMask, listenerCount);
		}
	}

	private void fireReconcileDelta(IElementChangedListener[] listeners,
			int[] listenerMask, int listenerCount) {
		final IErlElementDelta deltaToNotify = mergeDeltas(this.reconcileDeltas
				.values());
		if (verbose) {
			System.out
					.println("FIRING POST_RECONCILE Delta [" + Thread.currentThread() + "]:"); //$NON-NLS-1$//$NON-NLS-2$
			System.out
					.println(deltaToNotify == null ? "<NONE>" : deltaToNotify.toString()); //$NON-NLS-1$
		}
		if (deltaToNotify != null) {
			// flush now so as to keep listener reactions to post their own
			// deltas for
			// subsequent iteration
			this.reconcileDeltas = new HashMap();
			notifyListeners(deltaToNotify, ElementChangedEvent.POST_RECONCILE,
					listeners, listenerMask, listenerCount);
		}
	}

	private void fireShiftEvent(IErlElementDelta deltaToNotify,
			IElementChangedListener[] listeners, int[] listenerMask,
			int listenerCount) {

		// post change deltas
		if (verbose) {
			System.out
					.println("FIRING POST_SHIFT event [" + Thread.currentThread() + "]:"); //$NON-NLS-1$//$NON-NLS-2$
			System.out
					.println(deltaToNotify == null ? "<NONE>" : deltaToNotify.toString()); //$NON-NLS-1$
		}
		if (deltaToNotify != null) {
			this.flush();
			notifyListeners(deltaToNotify, ElementChangedEvent.POST_SHIFT,
					listeners, listenerMask, listenerCount);
		}
	}

	private IErlElementDelta mergeDeltas(Collection deltas) {

		synchronized (deltas) {
			if (deltas.size() == 0) {
				return null;
			}
			if (deltas.size() == 1) {
				return (IErlElementDelta) deltas.iterator().next();
			}
			if (deltas.size() <= 1) {
				return null;
			}

			final Iterator iterator = deltas.iterator();
			final IErlElement cRoot = getErlangModel();
			final ErlElementDelta rootDelta = new ErlElementDelta(0, 0, cRoot);
			boolean insertedTree = false;
			while (iterator.hasNext()) {
				final ErlElementDelta delta = (ErlElementDelta) iterator.next();
				final IErlElement element = delta.getElement();
				if (cRoot.equals(element)) {
					final IErlElementDelta[] children = delta
							.getChildren(IErlElementDelta.ALL);
					for (int j = 0; j < children.length; j++) {
						final ErlElementDelta projectDelta = (ErlElementDelta) children[j];
						rootDelta.insertDeltaTree(projectDelta.getElement(),
								projectDelta);
						insertedTree = true;
					}
					final IResourceDelta[] resourceDeltas = delta
							.getResourceDeltas();
					if (resourceDeltas != null) {
						for (int i = 0, length = resourceDeltas.length; i < length; i++) {
							rootDelta.addResourceDelta(resourceDeltas[i]);
							insertedTree = true;
						}
					}
				} else {
					rootDelta.insertDeltaTree(element, delta);
					insertedTree = true;
				}
			}
			if (insertedTree) {
				return rootDelta;
			}
			return null;
		}
	}

	/**
	 * Flushes all deltas without firing them.
	 */
	protected void flush() {
		erlModelDeltas.clear();
	}

	public void notifyListeners(IErlElementDelta deltaToNotify, int eventType,
			IElementChangedListener[] listeners, int[] listenerMask,
			int listenerCount) {

		final ElementChangedEvent extraEvent = new ElementChangedEvent(
				deltaToNotify, eventType);
		for (int i = 0; i < listenerCount; i++) {
			if (listenerMask == null || (listenerMask[i] & eventType) != 0) {
				final IElementChangedListener listener = listeners[i];
				long start = -1;
				if (verbose) {
					System.out
							.print("Listener #" + (i + 1) + "=" + listener.toString());//$NON-NLS-1$//$NON-NLS-2$
					start = System.currentTimeMillis();
				}
				// wrap callbacks with Safe runnable for subsequent listeners to
				// be called
				// when some are causing grief
				SafeRunner.run(new ISafeRunnable() {

					public void handleException(Throwable exception) {
						// CCorePlugin.log(exception, "Exception occurred in
						// listener of C
						// element change notification"); //$NON-NLS-1$
						ErlangPlugin.log(exception);
					}

					public void run() throws Exception {
						listener.elementChanged(extraEvent);
					}
				});
				if (verbose) {
					System.out
							.println(" -> " + (System.currentTimeMillis() - start) + "ms"); //$NON-NLS-1$ //$NON-NLS-2$
				}
			}
		}
	}

	public IErlProject createEmptyProject() {
		return new ErlProject();
	}

}
