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

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.util.PluginUtils;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.IErlideUIConstants;

/**
 * Provides custom labels for the namespace
 * 
 * 
 * @author Eric Merritt [cyberlync at gmail dot com]
 */
public class ModuleLabelProvider implements ILabelProvider {

	/**
	 * @see org.eclipse.jface.viewers.ILabelProvider#getImage(java.lang.Object)
	 */
	public Image getImage(Object element) {
		if (element instanceof IWorkspaceRoot) {
			return null;
		} else if (element instanceof IErlProject) {
			final IErlProject project = (IErlProject) element;
			if (project.isOpen()) {
				return ErlideUIPlugin.getDefault().getImage(
						IErlideUIConstants.IMG_PROJECT_LABEL);
			}
			return ErlideUIPlugin.getDefault().getImage(
					IErlideUIConstants.IMG_PROJECT_CLOSED_LABEL);
		} else if (element instanceof IContainer) {
			if (PluginUtils.isOnSourcePath((IContainer) element)) {
				return ErlideUIPlugin.getDefault().getImage(
						IErlideUIConstants.IMG_PACKAGE_FOLDER_LABEL);
			}

			return ErlideUIPlugin.getDefault().getImage(
					IErlideUIConstants.IMG_FOLDER_LABLE);

		} else if (element instanceof IFile) {
			return ErlideUIPlugin.getDefault().getImage(
					IErlideUIConstants.IMG_FILE_LABEL);
		}

		return null;
	}

	/**
	 * @see org.eclipse.jface.viewers.ILabelProvider#getText(java.lang.Object)
	 */
	public String getText(Object element) {
		if (element instanceof IWorkspaceRoot) {
			return "";
		} else if (element instanceof IErlProject) {
			return "EP " + ((IErlProject) element).getElementName();
		} else if (element instanceof IProject) {
			return "P " + ((IProject) element).getName();
		} else if (element instanceof IContainer) {
			return ((IContainer) element).getName();
		} else if (element instanceof IFile) {
			return ((IFile) element).getName();
		}

		return "";
	}

	/**
	 * @see org.eclipse.jface.viewers.IBaseLabelProvider#addListener(org.eclipse.jface.viewers.ILabelProviderListener)
	 */
	public void addListener(ILabelProviderListener listener) {
		// TODO Auto-generated method stub

	}

	/**
	 * @see org.eclipse.jface.viewers.IBaseLabelProvider#dispose()
	 */
	public void dispose() {
		// TODO Auto-generated method stub

	}

	/**
	 * @see org.eclipse.jface.viewers.IBaseLabelProvider#isLabelProperty(java.lang.Object,
	 *      java.lang.String)
	 */
	public boolean isLabelProperty(Object element, String property) {
		// TODO Auto-generated method stub
		return false;
	}

	/**
	 * @see org.eclipse.jface.viewers.IBaseLabelProvider#removeListener(org.eclipse.jface.viewers.ILabelProviderListener)
	 */
	public void removeListener(ILabelProviderListener listener) {
		// TODO Auto-generated method stub

	}

}
