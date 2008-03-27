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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.List;

import org.eclipse.compare.IEditableContent;
import org.eclipse.compare.IStreamContentAccessor;
import org.eclipse.compare.structuremergeviewer.Differencer;
import org.eclipse.compare.structuremergeviewer.IDiffContainer;
import org.eclipse.compare.structuremergeviewer.IStructureComparator;
import org.eclipse.compare.structuremergeviewer.IStructureCreator;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IParent;
import org.erlide.ui.ErlideUIPlugin;

/**
 * 
 */
public class ErlStructureCreator implements IStructureCreator {

	private static final String NAME = "ErlStructureCreator.name"; //$NON-NLS-1$

	private final IErlProject fProject;

	private final String fName;

	public ErlStructureCreator(String title) {
		this();
	}

	public ErlStructureCreator() {
		this(ErlangCore.getModelManager().createEmptyProject(), "comptemp.erl"); // FIXME
		// title??
	}

	/**
	 * @param project
	 * @param name
	 */
	public ErlStructureCreator(IErlProject project, String name) {
		super();
		// TODO Auto-generated constructor stub
		fProject = project;
		fName = name;
	}

	/**
	 * @see IStructureCreator#getTitle
	 */
	public String getName() {
		return ErlideUIPlugin.getResourceString(NAME);
	}

	/**
	 * @see IStructureCreator#getStructure
	 */
	public IStructureComparator getStructure(Object input) {

		String s = null;
		if (input instanceof IStreamContentAccessor) {
			try {
				s = readString(((IStreamContentAccessor) input).getContents());
			} catch (final CoreException ex) {
			}
		}

		if (s == null) {
			s = "";
		}
		final Document doc = new Document(s);

		final IErlModule module = ErlangCore.getModelManager()
				.createModuleFrom(fName, s, fProject, null);
		ErlNode root = null;
		try {
			module.open(null);
			root = recursiveMakeErlNodes(module, null, doc);
		} catch (final ErlModelException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return root;
	}

	private ErlNode recursiveMakeErlNodes(IErlElement element, ErlNode parent,
			Document doc) throws ErlModelException {
		final ErlNode n = new ErlNode(parent, element.getElementType(), element
				.getElementName(), doc, 0, 0);
		if (element instanceof IParent) {
			final IParent p = (IParent) element;
			final List<IErlElement> children = p.getChildren();
			for (IErlElement element0 : children) {
				recursiveMakeErlNodes(element0, n, doc);
			}
		}
		return n;
	}

	/**
	 * @see IStructureCreator#canSave
	 */
	public boolean canSave() {
		return true;
	}

	/**
	 * @see IStructureCreator#locate
	 */
	public IStructureComparator locate(Object path, Object source) {
		return null;
	}

	/**
	 * @see IStructureCreator#canRewriteTree
	 */
	public boolean canRewriteTree() {
		return false;
	}

	/**
	 * @see IStructureCreator#rewriteTree
	 */
	public void rewriteTree(Differencer differencer, IDiffContainer root) {
	}

	/**
	 * @see IStructureCreator#save
	 */
	public void save(IStructureComparator structure, Object input) {
		if (input instanceof IEditableContent && structure instanceof ErlNode) {
			final IDocument doc = ((ErlNode) structure).getDocument();
			final IEditableContent bca = (IEditableContent) input;
			final String c = doc.get();
			bca.setContent(c.getBytes());
		}
	}

	/**
	 * @see IStructureCreator#getContents
	 */
	public String getContents(Object node, boolean ignoreWhitespace) {
		if (node instanceof IStreamContentAccessor) {
			final IStreamContentAccessor sca = (IStreamContentAccessor) node;
			try {
				return readString(sca.getContents());
			} catch (final CoreException ex) {
			}
		}
		return null;
	}

	/**
	 * Returns null if an error occurred.
	 */
	private static String readString(InputStream is) {
		if (is == null) {
			return null;
		}
		BufferedReader reader = null;
		try {
			final StringBuilder buffer = new StringBuilder();
			final char[] part = new char[2048];
			int read = 0;
			reader = new BufferedReader(new InputStreamReader(is));

			while ((read = reader.read(part)) != -1) {
				buffer.append(part, 0, read);
			}

			return buffer.toString();

		} catch (final IOException ex) {
		} finally {
			if (reader != null) {
				try {
					reader.close();
				} catch (final IOException ex) {
				}
			}
		}
		return null;
	}

}
