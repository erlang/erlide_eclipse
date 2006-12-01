/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Eric Merritt
 *******************************************************************************/
package org.erlide.basicui.util;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;

/**
 * Simple utility functions
 * 
 * 
 * @author Eric Merritt [cyberlync at yahoo dot com]
 */
public final class PluginUtils {

	private PluginUtils() {
	}

	/**
	 * Returns whether the types of the resources in the given selection are
	 * among the specified resource types.
	 * 
	 * @param selection
	 *            the selection
	 * @param resourceMask
	 *            resource mask formed by bitwise OR of resource type constants
	 *            (defined on <code>IResource</code>)
	 * @return <code>true</code> if all selected elements are resources of the
	 *         right type, and <code>false</code> if at least one element is
	 *         either a resource of some other type or a non-resource
	 * @see IResource#getType()
	 */
	public static boolean allResourcesAreOfType(IStructuredSelection selection,
			int resourceMask) {
		final Iterator resources = selection.iterator();
		while (resources.hasNext()) {
			final Object next = resources.next();
			if (!(next instanceof IResource)) {
				return false;
			}
			if (!resourceIsType((IResource) next, resourceMask)) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Returns the selection adapted to IResource. Returns null if any of the
	 * entries are not adaptable.
	 * 
	 * @param selection
	 *            the selection
	 * @param resourceMask
	 *            resource mask formed by bitwise OR of resource type constants
	 *            (defined on <code>IResource</code>)
	 * @return IStructuredSelection or null if any of the entries are not
	 *         adaptable.
	 * @see IResource#getType()
	 */
	public static IStructuredSelection allResources(
			IStructuredSelection selection, int resourceMask) {
		final Iterator adaptables = selection.iterator();
		final List<IResource> result = new ArrayList<IResource>();
		while (adaptables.hasNext()) {
			final Object next = adaptables.next();
			if (next instanceof IAdaptable) {
				final Object resource = ((IAdaptable) next)
						.getAdapter(IResource.class);
				if (resource == null) {
					return null;
				} else if (resourceIsType((IResource) resource, resourceMask)) {
					result.add((IResource)resource);
				}
			} else {
				return null;
			}
		}
		return new StructuredSelection(result);
	}

	/**
	 * Returns whether the type of the given resource is among the specified
	 * resource types.
	 * 
	 * @param resource
	 *            the resource
	 * @param resourceMask
	 *            resource mask formed by bitwise OR of resource type constants
	 *            (defined on <code>IResource</code>)
	 * @return <code>true</code> if the resources has a matching type, and
	 *         <code>false</code> otherwise
	 * @see IResource#getType()
	 */
	public static boolean resourceIsType(IResource resource, int resourceMask) {
		return (resource.getType() & resourceMask) != 0;
	}
}
