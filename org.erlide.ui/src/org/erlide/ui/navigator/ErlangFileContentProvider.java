package org.erlide.ui.navigator;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.progress.UIJob;
import org.erlide.basiccore.ErlLogger;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IParent;
import org.erlide.ui.util.ErlModelUtils;

public class ErlangFileContentProvider implements ITreeContentProvider,
		IResourceChangeListener, IResourceDeltaVisitor {

	private static final Object[] NO_CHILDREN = new Object[0];

	private static final String ERLANGFILE_EXT = "erl"; //$NON-NLS-1$

	private StructuredViewer viewer;

	/**
	 * Create the PropertiesContentProvider instance.
	 * 
	 * Adds the content provider as a resource change listener to track changes
	 * on disk.
	 * 
	 */
	public ErlangFileContentProvider() {
		// super(false);
		ResourcesPlugin.getWorkspace().addResourceChangeListener(this,
				IResourceChangeEvent.POST_CHANGE);
	}

	/**
	 * Return the model elements for a *.erl IFile or NO_CHILDREN for otherwise.
	 */
	public Object[] getChildren(Object parentElement) {
		Object[] result = NO_CHILDREN;
		try {
			if (parentElement instanceof IFile) {
				IErlModule mod = ErlModelUtils.getModule((IFile) parentElement);
				if (mod != null) {
					mod.open(null);
					result = mod.getChildren();
				}
			} else if (parentElement instanceof IParent) {
				IParent parent = (IParent) parentElement;
				result = parent.getChildren();
			}
		} catch (ErlModelException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		ErlLogger.debug("// " + result.length + " children");
		return result;
	}

	/**
	 * Load the model from the given file, if possible.
	 * 
	 * @param modelFile
	 *            The IFile which contains the persisted model
	 */

	public Object getParent(Object element) {
		if (element instanceof IErlElement) {
			IErlElement elt = (IErlElement) element;
			IErlElement parent = elt.getParent();
			if (parent instanceof IErlModule) {
				IErlModule mod = (IErlModule) parent;
				try {
					if (mod != null) {
						return mod.getCorrespondingResource();
					}
				} catch (ErlModelException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
		return null;
	}

	public boolean hasChildren(Object element) {
		boolean result = false;
		if (element instanceof IParent) {
			IParent parent = (IParent) element;
			result = parent.hasChildren();
		} else if (element instanceof IFile) {
			IErlModule mod = ErlModelUtils.getModule((IFile) element);
			if (mod != null) {
				try {
					mod.open(null);
				} catch (ErlModelException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				result = mod.hasChildren();
			}
		}
		// ErlLogger.debug("// hasChildren " + result);
		return result;
	}

	public Object[] getElements(Object inputElement) {
		return getChildren(inputElement);
	}

	public void dispose() {
		ResourcesPlugin.getWorkspace().removeResourceChangeListener(this);
	}

	public void inputChanged(Viewer aViewer, Object oldInput, Object newInput) {
		viewer = (StructuredViewer) aViewer;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.resources.IResourceChangeListener#resourceChanged(org.eclipse.core.resources.IResourceChangeEvent)
	 */
	public void resourceChanged(IResourceChangeEvent event) {

		IResourceDelta delta = event.getDelta();
		try {
			delta.accept(this);
		} catch (CoreException e) {
			e.printStackTrace();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.resources.IResourceDeltaVisitor#visit(org.eclipse.core.resources.IResourceDelta)
	 */
	public boolean visit(IResourceDelta delta) {

		IResource source = delta.getResource();
		switch (source.getType()) {
		case IResource.ROOT:
		case IResource.PROJECT:
		case IResource.FOLDER:
			viewer.refresh();
			return true;
		case IResource.FILE:
			final IFile file = (IFile) source;
			if (ERLANGFILE_EXT.equals(file.getFileExtension())) {
				// updateModel(file);
				new UIJob("Update Erlang Model in CommonViewer") { //$NON-NLS-1$
					@Override
					public IStatus runInUIThread(IProgressMonitor monitor) {
						if (viewer != null && !viewer.getControl().isDisposed()) {
							viewer.refresh(file);
						}
						return Status.OK_STATUS;
					}
				}.schedule();
			}
			return false;
		}
		return false;
	}

}
