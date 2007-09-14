/*******************************************************************************
 * Copyright (c) 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     QNX Software System
 *******************************************************************************/
package org.erlide.ui.internal.compare;

import org.eclipse.compare.ITypedElement;
import org.eclipse.compare.structuremergeviewer.DocumentRangeNode;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.text.IDocument;
import org.eclipse.swt.graphics.Image;
import org.erlide.core.erlang.IErlElement.ErlElementType;
import org.erlide.ui.ErlideUIPlugin;

/**
 * 
 */

class ErlNode extends DocumentRangeNode implements ITypedElement {

	private ErlNode fParent;

	private ErlElementType fType;

	public ErlNode(ErlNode parent, ErlElementType type, String id, IDocument doc,
			int start, int length) {
		super(type.hashCode(), id, doc, start, length);
		fParent = parent;
		fType = type;
		if (parent != null) {
			parent.addChild(this);
		}
	}

	public ErlNode(ErlNode parent, ErlElementType type, String id, int start,
			int length) {
		this(parent, type, id, parent.getDocument(), start, length);
	}

	/**
	 * @see ITypedInput#getParent
	 */
	public ErlNode getParent() {
		return fParent;
	}

	/**
	 * @see ITypedInput#getNodeType
	 */
	public ErlElementType getNodeType() {
		return fType;
	}

	/**
	 * @see ITypedInput#getName
	 */
	public String getName() {
		return getId();
	}

	/**
	 * @see ITypedInput#getType
	 */
	public String getType() {
		return ".erl";
	}

	/**
	 * @see ITypedInput#getImage
	 */
	public Image getImage() {
		final ImageDescriptor descriptor = ErlideUIPlugin.getDefault()
				.getImageDescriptor("erl");
		return ErlideUIPlugin.getImageDescriptorRegistry().get(descriptor);
	}
}
