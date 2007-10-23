package org.erlide.ui.navigator;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

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

public class ErlangFileContentProvider implements ITreeContentProvider,
		IResourceChangeListener, IResourceDeltaVisitor {

	private static final Object[] NO_CHILDREN = new Object[0];

	private static final Object ERLANGFILE_EXT = "erl"; //$NON-NLS-1$

	private final Map/* <IFile, PropertiesTreeData[]> */<IFile, ErlangFileTreeData[]> cachedModelMap = new HashMap<IFile, ErlangFileTreeData[]>();

	private StructuredViewer viewer;

	/**
	 * Create the PropertiesContentProvider instance.
	 * 
	 * Adds the content provider as a resource change listener to track changes
	 * on disk.
	 * 
	 */
	public ErlangFileContentProvider() {
		ResourcesPlugin.getWorkspace().addResourceChangeListener(this,
				IResourceChangeEvent.POST_CHANGE);
	}

	/**
	 * Return the model elements for a *.erl IFile or NO_CHILDREN for otherwise.
	 */
	public Object[] getChildren(Object parentElement) {
		Object[] children = null;
		if (parentElement instanceof ErlangFileTreeData) {
			children = NO_CHILDREN;
		} else if (parentElement instanceof IFile) {
			/* possible model file */
			IFile modelFile = (IFile) parentElement;
			if (ERLANGFILE_EXT.equals(modelFile.getFileExtension())) {
				children = (ErlangFileTreeData[]) cachedModelMap.get(modelFile);
				if (children == null && updateModel(modelFile) != null) {
					children = (ErlangFileTreeData[]) cachedModelMap
							.get(modelFile);
				}
			}
		}
		return children != null ? children : NO_CHILDREN;
	}

	/**
	 * Load the model from the given file, if possible.
	 * 
	 * @param modelFile
	 *            The IFile which contains the persisted model
	 */
	private synchronized Properties updateModel(IFile modelFile) {

		if (ERLANGFILE_EXT.equals(modelFile.getFileExtension())) {
			Properties model = new Properties();
			if (modelFile.exists()) {
				try {
					model.load(modelFile.getContents());

					String propertyName;
					List<ErlangFileTreeData> properties = new ArrayList<ErlangFileTreeData>();
					for (Enumeration<?> names = model.propertyNames(); names
							.hasMoreElements();) {
						propertyName = (String) names.nextElement();
						properties.add(new ErlangFileTreeData(propertyName,
								propertyName, modelFile));
					}
					ErlangFileTreeData[] propertiesTreeData = (ErlangFileTreeData[]) properties
							.toArray(new ErlangFileTreeData[properties.size()]);

					cachedModelMap.put(modelFile, propertiesTreeData);
					return model;
				} catch (IOException e) {
				} catch (CoreException e) {
				}
			} else {
				cachedModelMap.remove(modelFile);
			}
		}
		return null;
	}

	public Object getParent(Object element) {
		if (element instanceof ErlangFileTreeData) {
			ErlangFileTreeData data = (ErlangFileTreeData) element;
			return data.getFile();
		}
		return null;
	}

	public boolean hasChildren(Object element) {
		if (element instanceof ErlangFileTreeData) {
			return false;
		} else if (element instanceof IFile) {
			return ERLANGFILE_EXT.equals(((IFile) element).getFileExtension());
		}
		return false;
	}

	public Object[] getElements(Object inputElement) {
		return getChildren(inputElement);
	}

	public void dispose() {
		cachedModelMap.clear();
		ResourcesPlugin.getWorkspace().removeResourceChangeListener(this);
	}

	public void inputChanged(Viewer aViewer, Object oldInput, Object newInput) {
		if (oldInput != null && !oldInput.equals(newInput))
			cachedModelMap.clear();
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
			return true;
		case IResource.FILE:
			final IFile file = (IFile) source;
			if (ERLANGFILE_EXT.equals(file.getFileExtension())) {
				updateModel(file);
				new UIJob("Update Erlang Model in CommonViewer") { //$NON-NLS-1$
					public IStatus runInUIThread(IProgressMonitor monitor) {
						if (viewer != null && !viewer.getControl().isDisposed())
							viewer.refresh(file);
						return Status.OK_STATUS;
					}
				}.schedule();
			}
			return false;
		}
		return false;
	}

}
