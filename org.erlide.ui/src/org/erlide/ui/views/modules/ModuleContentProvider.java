/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Eric Merritt
 *******************************************************************************/

package org.erlide.ui.views.modules;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.AbstractTreeViewer;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.widgets.Control;
import org.erlide.basiccore.ErlLogger;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.util.PluginUtils;

/**
 * Provides content for the module navigarot
 * 
 * 
 * @author Eric Merritt [cyberlync at gmail dot com]
 */
public class ModuleContentProvider implements ITreeContentProvider,
		IResourceChangeListener {

	/**
	 * The controlling tree viewer
	 */
	protected Viewer controlViewer;

	public ModuleContentProvider() {
		super();

		final IWorkspace workspace = ResourcesPlugin.getWorkspace();
		workspace.addResourceChangeListener(this,
				IResourceChangeEvent.POST_CHANGE);

		// ErlModelManager mgr = ErlModelManager.getErlangModelManager();
		// ErlModel mdl = mgr.getErlangModel();
	}

	/**
	 * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
	 */
	public Object[] getChildren(Object parentElement) {
		try {
			if (parentElement instanceof IWorkspaceRoot) {
				return getWorkspaceChildren((IWorkspaceRoot) parentElement);
			} else if (parentElement instanceof IErlProject) {
				return getProjectChildren((IErlProject) parentElement);
			} else if (parentElement instanceof IContainer) {
				return getContainerChildren((IContainer) parentElement);
			} else if (parentElement instanceof IFile) {
				return null;
			}
		} catch (final CoreException ce) {
			ce.printStackTrace(); // for now dont do a thing
		}
		return null;

	}

	/**
	 * Get the children of the container
	 * 
	 * @param con
	 *            the container
	 * @return the children of the container
	 * 
	 * @throws CoreException
	 */
	private Object[] getContainerChildren(IContainer con) throws CoreException {

		if (PluginUtils.isOnSourcePath(con)) {
		}

		final IResource[] resources = con.members(IResource.DEPTH_ZERO);

		final List<IResource> lst = new ArrayList<IResource>(resources.length);
		for (IResource element : resources) {
			if (element instanceof IContainer) {
				lst.add(element);
			}
		}

		for (IResource element : resources) {
			if (element instanceof IFile) {
				lst.add(element);
			}
		}

		return lst.toArray();
	}

	/**
	 * Get the children for the project
	 * 
	 * @param project
	 *            the specific project
	 * @return the children of the project
	 * 
	 * @throws CoreException
	 */
	private Object[] getProjectChildren(IErlProject project)
			throws CoreException {
		final IErlModule[] resources = project.getModules();
		final IResource[] nonerl = project.getNonErlangResources();

		final List<IResource> lst = new ArrayList<IResource>(resources.length
				+ nonerl.length);
		for (IErlModule element : resources) {
			ErlLogger.log("> " + element.getElementName());
		}

		for (IResource element : nonerl) {
			if (element instanceof IContainer) {
				lst.add(element);
			}
			if (element instanceof IFile) {
				lst.add(element);
			}
		}

		return lst.toArray();

	}

	/**
	 * Return the children for the workspace. We only want erlide projects in
	 * the list
	 * 
	 * @param root
	 *            The workspace root
	 * @return the core exception
	 * @throws CoreException
	 *             if a problem occures
	 */
	private Object[] getWorkspaceChildren(IWorkspaceRoot root)
			throws CoreException {
		final List<IErlProject> list = new LinkedList<IErlProject>();

		final IProject[] projects = root.getProjects();

		for (IProject element : projects) {
			if (isErlangProject(element)) {
				final IErlProject ep = ErlangCore.getModelManager().create(
						element);
				list.add(ep);
			}
		}

		return list.toArray();
	}

	/**
	 * Tests the project for erlide project natures.
	 * 
	 * @param project
	 *            The descriptions
	 * @return indicator of weather or not this is a erlide project
	 * @throws CoreException
	 *             if a problem occures
	 */
	private boolean isErlangProject(IProject project) throws CoreException {
		if (!project.isOpen()) {
			return true;
		}

		final String[] natureIds = project.getDescription().getNatureIds();

		for (String element : natureIds) {
			if (ErlangPlugin.NATURE_ID.equals(element)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
	 */
	public Object getParent(Object element) {
		if (element instanceof IResource) {
			return ((IResource) element).getParent();
		}

		return null;
	}

	/**
	 * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
	 */
	public boolean hasChildren(Object element) {
		try {
			if (element instanceof IWorkspaceRoot) {
				return ((IWorkspaceRoot) element).members().length != 0;
			} else if (element instanceof IErlProject) {
				return ((IErlProject) element).getChildren().length != 0;
			} else if (element instanceof IErlProject) {
				return ((IProject) element).members().length != 0;
			} else if (element instanceof IContainer) {
				return ((IContainer) element).members().length != 0;
			} else if (element instanceof IFile) {
				return false;
			}
		} catch (final CoreException ce) {
			// ignore
		}
		return false;

	}

	/**
	 * In erlang this is more or less the same as get children
	 * 
	 * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
	 */
	public Object[] getElements(Object inputElement) {
		return getChildren(inputElement);
	}

	/**
	 * @see org.eclipse.jface.viewers.IContentProvider#dispose()
	 */
	public void dispose() {
		// nothing to do here
	}

	/**
	 * 
	 * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer,
	 *      java.lang.Object, java.lang.Object)
	 */
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {

		controlViewer = viewer;
		IWorkspace oldWorkspace = null;
		IWorkspace newWorkspace = null;

		if (oldInput instanceof IWorkspace) {
			oldWorkspace = (IWorkspace) oldInput;
		} else if (oldInput instanceof IContainer) {
			oldWorkspace = ((IContainer) oldInput).getWorkspace();
		}

		if (newInput instanceof IWorkspace) {
			newWorkspace = (IWorkspace) newInput;
		} else if (newInput instanceof IContainer) {
			newWorkspace = ((IContainer) newInput).getWorkspace();
		}

		if (oldWorkspace != newWorkspace) {
			if (oldWorkspace != null) {
				oldWorkspace.removeResourceChangeListener(this);
			}
			if (newWorkspace != null) {
				newWorkspace.addResourceChangeListener(this,
						IResourceChangeEvent.POST_CHANGE);
			}
		}
	}

	/**
	 * 
	 * @see org.eclipse.core.resources.IResourceChangeListener#resourceChanged(org.eclipse.core.resources.IResourceChangeEvent)
	 */
	public final void resourceChanged(final IResourceChangeEvent event) {
		final IResourceDelta delta = event.getDelta();
		final Control ctrl = controlViewer.getControl();
		if (ctrl != null && !ctrl.isDisposed()) {
			// Do a sync exec, not an async exec, since the resource delta
			// must be traversed in this method. It is destroyed
			// when this method returns.
			ctrl.getDisplay().syncExec(new Runnable() {

				public void run() {
					processDelta(delta);
				}
			});
		}
	}

	/**
	 * Process a resource delta.
	 * 
	 * @param delta
	 *            the delta to process
	 */
	protected void processDelta(IResourceDelta delta) {
		// This method runs inside a syncExec. The widget may have been
		// destroyed
		// by the time this is run. Check for this and do nothing if so.
		final Control ctrl = controlViewer.getControl();
		if (ctrl == null || ctrl.isDisposed()) {
			return;
		}

		// Get the affected resource
		final IResource resource = delta.getResource();

		// If any children have changed type, just do a full refresh of this
		// parent, since a simple update on such children won't work,
		// and trying to map the change to a remove and add is too dicey.
		// The case is: folder A renamed to existing file B, answering yes to
		// overwrite B.
		final IResourceDelta[] affectedChildren = delta
				.getAffectedChildren(IResourceDelta.CHANGED);
		for (IResourceDelta element : affectedChildren) {
			if ((element.getFlags() & IResourceDelta.TYPE) != 0) {
				((StructuredViewer) controlViewer).refresh(resource);
				return;
			}
		}

		// Check the flags for changes the Navigator cares about.
		// See ResourceLabelProvider for the aspects it cares about.
		// Notice we don't care about F_CONTENT or F_MARKERS currently.
		final int changeFlags = delta.getFlags();
		if ((changeFlags & (IResourceDelta.OPEN | IResourceDelta.SYNC)) != 0) {
			((StructuredViewer) controlViewer).update(resource, null);
		}
		// Replacing a resource may affect its label and its children
		if ((changeFlags & IResourceDelta.REPLACED) != 0) {
			((StructuredViewer) controlViewer).refresh(resource, true);
			return;
		}

		for (IResourceDelta element : affectedChildren) {
			processDelta(element);
		}

		boolean addedAndRemoved = false;
		try {
			final IResourceDelta[] addedChildren = delta
					.getAffectedChildren(IResourceDelta.ADDED);
			final IResourceDelta[] removedChildren = delta
					.getAffectedChildren(IResourceDelta.REMOVED);
			addedAndRemoved = addedChildren.length > 0
					& removedChildren.length > 0;

			// Disable redraw until the operation is finished so we don't get a
			// flash of both the new and old item (in the case of rename)
			// Only do this if we're both adding and removing files (the rename
			// case)
			if (addedAndRemoved) {
				controlViewer.getControl().setRedraw(false);
			}
			// Process additions before removals as to not cause selection
			// preservation prior to new objects being added
			// Handle added children. Issue one update for all insertions.
			if (addedChildren.length > 0) {
				final Object[] affected = new Object[addedChildren.length];
				for (int i = 0; i < addedChildren.length; i++) {
					affected[i] = addedChildren[i].getResource();
				}
				if (controlViewer instanceof AbstractTreeViewer) {
					((AbstractTreeViewer) controlViewer)
							.add(resource, affected);
				} else {
					((StructuredViewer) controlViewer).refresh(resource);
				}
			}

			// Handle removed children. Issue one update for all removals.
			if (removedChildren.length > 0) {
				final Object[] affected = new Object[removedChildren.length];
				for (int i = 0; i < removedChildren.length; i++) {
					affected[i] = removedChildren[i].getResource();
				}
				if (controlViewer instanceof AbstractTreeViewer) {
					((AbstractTreeViewer) controlViewer).remove(affected);
				} else {
					((StructuredViewer) controlViewer).refresh(resource);
				}
			}
		} finally {
			if (addedAndRemoved) {
				controlViewer.getControl().setRedraw(true);
			}
		}

	}
}