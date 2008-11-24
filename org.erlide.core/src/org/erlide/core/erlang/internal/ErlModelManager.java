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
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
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
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.core.runtime.Status;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.builder.BuilderUtils;
import org.erlide.core.builder.ErlangBuilder;
import org.erlide.core.erlang.ErlElementDelta;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlElementDelta;
import org.erlide.core.erlang.IErlFolder;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModelManager;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IParent;
import org.erlide.core.erlang.IWorkingCopy;
import org.erlide.core.erlang.util.ElementChangedEvent;
import org.erlide.core.erlang.util.IElementChangedListener;
import org.erlide.core.erlang.util.Util;
import org.erlide.core.util.ErlideUtil;
import org.erlide.runtime.ErlLogger;

/**
 * The <code>ErlModelManager</code> manages instances of <code>IErlModel</code>.
 * <code>IElementChangedListener</code>s register with the
 * <code>ErlModelManager</code>, and receive <code>ElementChangedEvent</code>s
 * for all <code>IErlModel</code>s.
 * <p>
 * The single instance of <code>ErlModelManager</code> is available from the
 * static method <code>ErlModelManager.getErlangModelManager()</code>.
 */
public class ErlModelManager implements IErlModelManager {

	/**
	 * Unique handle onto the ErlModel
	 */
	private final ErlModel erlangModel = new ErlModel();

	private final HashSet<String> optionNames = new HashSet<String>(20);

	// private final Map<String, IErlElement> elements = new HashMap<String,
	// IErlElement>(
	// 10);

	/**
	 * Queue of reconcile deltas on working copies that have yet to be fired.
	 * This is a table form IWorkingCopy to IErlElementDelta
	 */
	private HashMap<IWorkingCopy, IErlElementDelta> reconcileDeltas = new HashMap<IWorkingCopy, IErlElementDelta>();

	/**
	 * The singleton manager
	 */
	private static final ErlModelManager MANAGER = new ErlModelManager();

	/**
	 * Queue of deltas created explicitly by the model that have yet to be
	 * fired.
	 */
	private final List<IErlElementDelta> erlModelDeltas = Collections
			.synchronizedList(new ArrayList<IErlElementDelta>());

	public static final int DEFAULT_CHANGE_EVENT = 0; // must not collide with

	// ElementChangedEvent event masks

	/**
	 * Registers the given delta with this manager. This API is to be used to
	 * registered deltas that are created explicitly by the C Model. Deltas
	 * created as translations of <code>IResourceDeltas</code> are to be
	 * registered with <code>#registerResourceDelta</code>.
	 */
	public void registerModelDelta(final IErlElementDelta delta) {
		erlModelDeltas.add(delta);
		// TODO
	}

	public static boolean verbose = Boolean.getBoolean("erlide.model.verbose");

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
	 * <code>null</code> if unable to associate the given resource with a Erlang
	 * element.
	 * <p>
	 * The resource must be one of:
	 * <ul>
	 * <li>a project - the element returned is the corresponding
	 * <code>IErlProject</code></li>
	 * <li>a <code>.erl</code> file - the element returned is the corresponding
	 * <code>IErlModule</code></li>
	 * <li>a <code>.beam</code> file - the element returned is the corresponding
	 * <code>IErlBeamFile</code></li>
	 * <li>a folder - the element returned is the corresponding
	 * <code>IErlPackage</code> -- not implemented yet</li>
	 * <li>the workspace root resource - the element returned is the
	 * <code>IErlModel</code></li>
	 * </ul>
	 * <p>
	 * Creating a Erlang element has the side effect of creating and opening all
	 * of the element's parents if they are not yet open.
	 */
	public IErlElement create(final IResource resource, final IErlElement parent) {
		if (resource == null) {
			return null;
		}
		final IErlElement e = erlangModel.findElement(resource);
		if (e != null) {
			return e; // TODO or should this give an exception?
		}
		final int type = resource.getType();
		switch (type) {
		case IResource.PROJECT:
			return createProject((IProject) resource); // , parent);
		case IResource.FILE:
			return createFile((IFile) resource, parent);
		case IResource.FOLDER:
			return createFolder((IFolder) resource, parent);
		case IResource.ROOT:
			return createRoot((IWorkspaceRoot) resource);
		default:
			return null;
		}
		// TODO should we make Erlidemodelevents and fire them?
	}

	private void remove(final IResource rsrc) {
		final IErlElement e = erlangModel.findElement(rsrc);
		if (e != null) {
			final IParent p = (IParent) e.getParent();
			p.removeChild(e);
		}
		// TODO should we make Erlidemodelevents and fire them?
	}

	private void change(final IResource rsrc) {
		final IErlElement e = erlangModel.findElement(rsrc);
		if (e != null) {
			e.resourceChanged();
		}
		// TODO should we make Erlidemodelevents and fire them?
	}

	/**
	 * Returns the Erlang element corresponding to the given file, its project
	 * being the given project. Returns <code>null</code> if unable to associate
	 * the given file with a Erlang element.
	 * 
	 * <p>
	 * The file must be one of:
	 * <ul>
	 * <li>a <code>.erl</code> file - the element returned is the corresponding
	 * <code>IErlModule</code></li>
	 * <li>a <code>.beam</code> file - the element returned is the corresponding
	 * <code>IClassFile</code></li>
	 * </ul>
	 * <p>
	 * Creating a Erlang element has the side effect of creating and opening all
	 * of the element's parents if they are not yet open.
	 */
	public IErlElement createFile(final IFile file, IErlElement parent) {
		if (file == null) {
			return null;
		}
		if (parent == null) {
			final IContainer parentResource = file.getParent();
			if (parentResource != null) {
				parent = erlangModel.findElement(parentResource);
			}
		}
		if (ErlideUtil.hasModuleExt(file.getName())) {
			return createModuleFrom(file, parent);
		}
		return null;
	}

	/**
	 */
	public IErlFolder createFolder(final IFolder folder,
			final IErlElement parent) {
		if (folder == null) {
			return null;
		}
		final IErlFolder f = new ErlFolder(folder, parent);
		final IParent p = (IParent) parent;
		p.addChild(f);
		return f;
	}

	/**
	 * @see org.erlide.core.erlang.IErlModelManager#createModuleFrom(org.eclipse.core.resources.IFile,
	 *      org.erlide.core.erlang.IErlProject)
	 */
	public IErlModule createModuleFrom(final IFile file,
			final IErlElement parent) {
		// ErlLogger.debug("createModuleFrom:: " + file + " " + project);
		if (file == null) {
			return null;
		}

		// if (project == null) {
		// project = createProject(file.getProject());
		// }

		// final String key = parent.getName() + "/" + file.getName();
		// if (elements.containsKey(key)) {
		// return (IErlModule) elements.get(key);
		// }
		final String ext = file.getFileExtension();
		if (ErlideUtil.isModuleExt(ext)) {
			String initialText = null;
			if (file.exists()) {
				try {
					initialText = new String(Util
							.getResourceContentsAsCharArray(file));
				} catch (final ErlModelException e) {
					initialText = "";
				}
			}
			final IErlModule module = new ErlModule(parent, file.getName(),
					initialText, file);
			((IParent) parent).addChild(module);
			// elements.put(key, module);
			return module;
		}
		return null;
	}

	/**
	 * @see org.erlide.core.erlang.IErlModelManager#createModuleFrom(org.eclipse.core.resources.IFile,
	 *      org.erlide.core.erlang.IErlProject)
	 */
	public IErlModule createModuleFrom(final String name, final String text,
			final IErlElement parent) {
		// ErlLogger.debug("createModuleFrom:: " + file + " " + project);
		if (name == null || text == null) {
			return null;
		}

		// final String key = project.getName() + "/" + name;
		// if (elements.containsKey(key)) {
		// return (IErlModule) elements.get(key);
		// }
		// final IPath path = new Path(key);
		// final String ext = path.getFileExtension();
		if (ErlideUtil.hasModuleExt(name)) {
			final IErlModule module = new ErlModule(parent, name, text, null);
			// elements.put(key, module);
			return module;
		}
		return null;
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
	public IErlProject createProject(final IProject project) {
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
	 * <li>a <code>.beam</code> file - the element returned is the corresponding
	 * <code>IBeamFile</code></li>
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
	// public IErlElement create(final IFile file) {
	// return create(file, null/* unknown Erlang project */);
	// }
	/**
	 * Create IErlFolder from folder
	 * <p>
	 * Creating an Erlang element has the side effect of creating and opening
	 * all of the element's parents if they are not yet open.
	 * 
	 * @param folder
	 *            the given folder
	 * @return an Erlang folder
	 */
	// public IErlFolder create(final IFolder folder) {
	// return create(folder, null/* unknown Erlang project */);
	// }
	/**
	 * Returns the Erlang element corresponding to the given resource, or
	 * <code>null</code> if unable to associate the given resource with a Erlang
	 * element.
	 * <p>
	 * The resource must be one of:
	 * <ul>
	 * <li>a project - the element returned is the corresponding
	 * <code>IErlProject</code></li>
	 * <li>a <code>.Erlang</code> file - the element returned is the
	 * corresponding <code>IErlModule</code></li>
	 * <li>a <code>.class</code> file - the element returned is the
	 * corresponding <code>IClassFile</code></li>
	 * <li>a <code>.jar</code> file - the element returned is the corresponding
	 * <code>IPackageFragmentRoot</code></li>
	 * <li>a folder - the element returned is the corresponding
	 * <code>IPackageFragmentRoot</code> or <code>IPackageFragment</code></li>
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
	 *         <code>null</code> if unable to associate the given resource with
	 *         a Erlang element
	 */
	public IErlElement create(final IResource resource) {
		return create(resource, null);
	}

	/**
	 * Returns the Erlang model.
	 * 
	 * @param root
	 *            the given root
	 * @return the Erlang model, or <code>null</code> if the root is null
	 */
	private IErlModel createRoot(final IWorkspaceRoot root) {
		if (root == null) {
			return null;
		}
		return getDefault().getErlangModel();
	}

	/**
	 * @see org.erlide.core.erlang.IErlModelManager#createModuleFrom(org.eclipse.core.resources.IFile)
	 */
	public IErlModule createModuleFrom(final IFile file) {
		return createModuleFrom(file, null);
	}

	// /**
	// * Update the classpath variable cache
	// */
	// public static class PluginPreferencesListener implements
	// Preferences.IPropertyChangeListener {
	//
	// /**
	// * @see
	// org.eclipse.core.runtime.Preferences.IPropertyChangeListener#propertyChange(Preferences.PropertyChangeEvent)
	// */
	// public void propertyChange(final Preferences.PropertyChangeEvent event) {
	//
	// // String propertyName = event.getProperty();
	// }
	// }

	private class ResourceChangeListener implements IResourceChangeListener {
		public void resourceChanged(final IResourceChangeEvent event) {
			if (event.getType() != IResourceChangeEvent.POST_CHANGE) {
				return;
			}
			final IResourceDelta rootDelta = event.getDelta();
			final ArrayList<IResource> added = new ArrayList<IResource>();
			final ArrayList<IResource> changed = new ArrayList<IResource>();
			final ArrayList<IResource> removed = new ArrayList<IResource>();
			final IResourceDeltaVisitor visitor = new IResourceDeltaVisitor() {
				public boolean visit(IResourceDelta delta) {
					ErlLogger.debug("delta " + delta.getKind() + " for "
							+ delta.getResource().getLocation());
					IResource resource = delta.getResource();
					boolean erlangFile = resource.getType() == IResource.FILE
							&& ErlideUtil.hasModuleExt(resource.getName());
					boolean erlangProject = resource.getType() == IResource.PROJECT
							&& ErlideUtil.hasErlangNature((IProject) resource);
					boolean erlangFolder = resource.getType() == IResource.FOLDER;
					// &&
					// ErlideUtil.isOnSourcePathOrParentToFolderOnSourcePath((IFolder)
					// resource);
					if (erlangFile || erlangProject || erlangFolder) {
						if (delta.getKind() == IResourceDelta.ADDED) {
							added.add(resource);
						}
						if (delta.getKind() == IResourceDelta.CHANGED) {
							changed.add(resource);
						}
						if (delta.getKind() == IResourceDelta.REMOVED) {
							removed.add(resource);
						}
					}
					return !erlangFile;
				}
			};
			try {
				rootDelta.accept(visitor);
			} catch (final CoreException e) {
				ErlLogger.warn(e);
			}
			for (final IResource rsrc : added) {
				create(rsrc);
			}
			for (final IResource rsrc : changed) {
				change(rsrc);
			}
			// make sure we don't dispose trees before leaves...
			Collections.sort(removed, new Comparator<IResource>() {

				public int compare(final IResource o1, final IResource o2) {
					if (o1.equals(o2)) {
						return 0;
					} else if (o1.getFullPath().isPrefixOf(o2.getFullPath())) {
						return 1;
					} else {
						return -1;
					}
				}

			});
			for (final IResource rsrc : removed) {
				remove(rsrc);
			}
		}
	}

	/**
	 * Constructs a new ErlModelManager
	 */
	private ErlModelManager() {
		// singleton: prevent others from creating a new instance

		final IWorkspace workspace = ResourcesPlugin.getWorkspace();
		final IResourceChangeListener listener = new ResourceChangeListener();
		workspace.addResourceChangeListener(listener);
	}

	/**
	 * @see ISaveParticipant
	 */
	public void doneSaving(final ISaveContext context) {
		// nothing to do
	}

	/**
	 * @see org.erlide.core.erlang.IErlModelManager#getInfo(org.erlide.core.erlang.IErlElement)
	 */
	public synchronized Object getInfo(final IErlElement element) {
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
	public Object getLastBuiltState(final IProject project,
			final IProgressMonitor monitor) {
		if (!ErlideUtil.hasErlangNature(project)) {
			return null;
		}

		// final PerProjectInfo info = getPerProjectInfo(project, true);
		// // create if missing
		// if (!info.fTriedRead) {
		// info.fTriedRead = true;
		// try {
		// if (monitor != null) {
		// monitor.subTask(Util.bind(
		// "build.readStateProgress", project.getName())); //$NON-NLS-1$
		// }
		// info.fSavedState = readState(project);
		// } catch (final CoreException e) {
		// e.printStackTrace();
		// }
		// }
		// return info.fSavedState;
		return null;
	}

	/**
	 * Returns the File to use for saving and restoring the last built state for
	 * the given project.
	 */
	private File getSerializationFile(final IProject project) {
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
	public void prepareToSave(final ISaveContext context) {
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
	// protected synchronized void putInfos(IErlElement openedElement,
	// Map newElements) {
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
	// }
	/**
	 * Reads the build state for the relevant project.
	 */
	protected Object readState(final IProject project) throws CoreException {
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
					if (BuilderUtils.isDebugging()) {
						System.out
								.println("Saved state thinks last build failed for "
										+ project.getName());
					}
				} finally {
					in.close();
				}
			} catch (final IOException e) {
				ErlLogger.warn(e);
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
	public synchronized Object removeInfoAndChildren(final ErlElement element)
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
	 * @see ISaveParticipant
	 */
	public void rollback(final ISaveContext context) {
		// nothing to do
	}

	/* Unused */
	/*
	 * private void saveState(PerProjectInfo info, ISaveContext context) throws
	 * CoreException { // passed this point, save actions are non trivial if
	 * (context.getKind() == ISaveContext.SNAPSHOT) { return; } // save built
	 * state if (info.fTriedRead) { saveBuiltState(info); } }
	 */

	/**
	 * Saves the built state for the project.
	 */
	/* Unused */
	/*
	 * private void saveBuiltState(PerProjectInfo info) throws CoreException {
	 * // if (ErlangBuilder.DEBUG) // ErlLogger.debug(Util.bind( //
	 * "build.saveStateProgress", info.project.getName())); //$NON-NLS-1$ final
	 * File file = getSerializationFile(info.fProject); if (file == null) {
	 * return; } // long t = System.currentTimeMillis(); try { final
	 * DataOutputStream out = new DataOutputStream( new BufferedOutputStream(new
	 * FileOutputStream(file))); try { out.writeUTF(ErlangPlugin.PLUGIN_ID);
	 * out.writeUTF("STATE"); //$NON-NLS-1$ if (info.fSavedState == null) {
	 * out.writeBoolean(false); } else { out.writeBoolean(true); //
	 * ErlangBuilder.writeState(info.savedState, out); } } finally {
	 * out.close(); } } catch (final RuntimeException e) { try { file.delete();
	 * } catch (final SecurityException se) { // could not delete file: cannot
	 * do much more } throw new CoreException( new Status( IStatus.ERROR,
	 * ErlangPlugin.PLUGIN_ID, Platform.PLUGIN_ERROR, Util .bind(
	 * "build.cannotSaveState", info.fProject.getName()), e)); //$NON-NLS-1$ }
	 * catch (final IOException e) { try { file.delete(); } catch (final
	 * SecurityException se) { // could not delete file: cannot do much more }
	 * throw new CoreException( new Status( IStatus.ERROR,
	 * ErlangPlugin.PLUGIN_ID, Platform.PLUGIN_ERROR, Util .bind(
	 * "build.cannotSaveState", info.fProject.getName()), e)); //$NON-NLS-1$ }
	 * // if (ErlangBuilder.DEBUG) // { // t = System.currentTimeMillis() - t;
	 * // ErlLogger.debug(Util.bind( // "build.saveStateComplete",
	 * String.valueOf(t))); //$NON-NLS-1$ // } }
	 */

	/**
	 * @see ISaveParticipant
	 */
	public void saving(final ISaveContext context) throws CoreException {

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
	protected void setBuildOrder(final String[] erlangBuildOrder)
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
	public void setLastBuiltState(final IProject project, final Object state) {
		if (ErlideUtil.hasErlangNature(project)) {
			// should never be requested on non-Erlang projects
			// final PerProjectInfo info = getPerProjectInfo(project, true);
			// info.fTriedRead = true; // no point trying to re-read once using
			// // setter
			// info.fSavedState = state;
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
	public void addElementChangedListener(
			final IElementChangedListener listener, final int eventMask) {
		// getDefault().addElementChangedListener(listener, eventMask);
	}

	/**
	 * Removes the given element changed listener. Has no affect if an identical
	 * listener is not registered.
	 * 
	 * @param listener
	 *            the listener
	 */
	public void removeElementChangedListener(
			final IElementChangedListener listener) {
		// getDefault().removeElementChangedListener(listener);
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
	public void addElementChangedListener(final IElementChangedListener listener) {
		addElementChangedListener(listener, ElementChangedEvent.POST_CHANGE);
		// | ElementChangedEvent.POST_RECONCILE);
	}

	/**
	 * @see org.erlide.core.erlang.IErlModelManager#getOptionNames()
	 */
	public HashSet<String> getOptionNames() {
		return optionNames;
	}

	// public IErlElement create(final IPath path) {
	// final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
	// // Assume it is fullpath relative to workspace
	// IResource res = root.findMember(path);
	// if (res == null) {
	// final IPath rootPath = root.getLocation();
	// if (rootPath.equals(path)) {
	// return getDefault().getErlangModel();
	// }
	// res = root.getContainerForLocation(path);
	// if (res == null || !res.exists()) {
	// res = root.getFileForLocation(path);
	// }
	// if (res != null && !res.exists()) {
	// res = null;
	// }
	// }
	//
	// // In case this is an external resource see if we can find
	// // a file for it.
	// if (res == null) {
	// final IFile[] files = root.findFilesForLocation(path);
	// if (files.length > 0) {
	// res = files[0];
	// }
	// }
	//
	// return create(res, null);
	// }

	public void fire(final int eventType) {
		fire(null, eventType);
	}

	/**
	 * Fire C Model deltas, flushing them after the fact. If the firing mode has
	 * been turned off, this has no effect.
	 */
	private void fire(final IErlElementDelta customDeltas, final int eventType) {
		if (fFire) {
			IErlElementDelta deltaToNotify;
			if (customDeltas == null) {
				deltaToNotify = mergeDeltas(erlModelDeltas);
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
	// ErlLogger.debug("FIRING PRE_AUTO_BUILD Delta [" +
	// Thread.currentThread() +
	// "]:"); //$NON-NLS-1$//$NON-NLS-2$
	// ErlLogger.debug(deltaToNotify == null ? "<NONE>" :
	// deltaToNotify.toString());
	// //$NON-NLS-1$
	// }
	// if (deltaToNotify != null) {
	// notifyListeners(deltaToNotify, ElementChangedEvent.PRE_AUTO_BUILD,
	// listeners,
	// listenerMask, listenerCount);
	// }
	// }

	private void firePostChangeDelta(final IErlElementDelta deltaToNotify,
			final IElementChangedListener[] listeners,
			final int[] listenerMask, final int listenerCount) {

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
			flush();
			notifyListeners(deltaToNotify, ElementChangedEvent.POST_CHANGE,
					listeners, listenerMask, listenerCount);
		}
	}

	private void fireReconcileDelta(final IElementChangedListener[] listeners,
			final int[] listenerMask, final int listenerCount) {
		final IErlElementDelta deltaToNotify = mergeDeltas(reconcileDeltas
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
			reconcileDeltas = new HashMap<IWorkingCopy, IErlElementDelta>();
			notifyListeners(deltaToNotify, ElementChangedEvent.POST_RECONCILE,
					listeners, listenerMask, listenerCount);
		}
	}

	private void fireShiftEvent(final IErlElementDelta deltaToNotify,
			final IElementChangedListener[] listeners,
			final int[] listenerMask, final int listenerCount) {

		// post change deltas
		if (verbose) {
			System.out
					.println("FIRING POST_SHIFT event [" + Thread.currentThread() + "]:"); //$NON-NLS-1$//$NON-NLS-2$
			System.out
					.println(deltaToNotify == null ? "<NONE>" : deltaToNotify.toString()); //$NON-NLS-1$
		}
		if (deltaToNotify != null) {
			flush();
			notifyListeners(deltaToNotify, ElementChangedEvent.POST_SHIFT,
					listeners, listenerMask, listenerCount);
		}
	}

	private IErlElementDelta mergeDeltas(
			final Collection<IErlElementDelta> deltas) {

		synchronized (deltas) {
			if (deltas.size() == 0) {
				return null;
			}
			if (deltas.size() == 1) {
				return deltas.iterator().next();
			}
			if (deltas.size() <= 1) {
				return null;
			}

			final Iterator<IErlElementDelta> iterator = deltas.iterator();
			final IErlElement cRoot = getErlangModel();
			final ErlElementDelta rootDelta = new ErlElementDelta(0, 0, cRoot);
			boolean insertedTree = false;
			while (iterator.hasNext()) {
				final ErlElementDelta delta = (ErlElementDelta) iterator.next();
				final IErlElement element = delta.getElement();
				if (cRoot.equals(element)) {
					final IErlElementDelta[] children = delta
							.getChildren(IErlElementDelta.ALL);
					for (final IErlElementDelta element0 : children) {
						final ErlElementDelta projectDelta = (ErlElementDelta) element0;
						rootDelta.insertDeltaTree(projectDelta.getElement(),
								projectDelta);
						insertedTree = true;
					}
					final IResourceDelta[] resourceDeltas = delta
							.getResourceDeltas();
					if (resourceDeltas != null) {
						for (final IResourceDelta element0 : resourceDeltas) {
							rootDelta.addResourceDelta(element0);
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

	public void notifyListeners(final IErlElementDelta deltaToNotify,
			final int eventType, final IElementChangedListener[] listeners,
			final int[] listenerMask, final int listenerCount) {

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

					public void handleException(final Throwable exception) {
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

	private static Map<Object, IErlModule> moduleMap = new HashMap<Object, IErlModule>();
	private static Map<IErlModule, Object> mapModule = new HashMap<IErlModule, Object>();

	public IErlModule getModuleFromFile(final String name,
			final String initialText, final String path, final Object key) {
		IErlModule m = moduleMap.get(key);
		if (m == null) {
			m = new ErlModuleWithoutResource(null, name, initialText, path);
			if (key != null) {
				moduleMap.put(key, m);
				mapModule.put(m, key);
			}
		}
		return m;
	}

	public void removeModule(final IErlModule module) {
		final Object key = mapModule.get(module);
		if (key != null) {
			mapModule.remove(module);
			moduleMap.remove(key);
		}
	}

	public IErlModule getModuleFromText(final String name,
			final String initialText, final Object key) {
		return getModuleFromFile(name, initialText, "", key);
	}

}
