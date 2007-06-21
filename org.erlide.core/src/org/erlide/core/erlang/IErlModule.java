/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     IBM Corporation - added J2SE 1.5 support
 *     Vlad Dumitrescu
 * *******************************************************************************/
package org.erlide.core.erlang;

import java.util.ArrayList;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.reconciler.DirtyRegion;
import org.erlide.core.util.ErlangFunction;
import org.erlide.core.util.ErlangIncludeFile;

import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * Represents an entire Erlang compilation unit (<code>.erl</code> or
 * <code>.hrl</code> source file). Compilation unit elements need to be opened
 * before they can be navigated or manipulated. The children are of type
 * <code>IErlAttribute</code>, and <code>IErlFunction</code>, and appear
 * in the order in which they are declared in the source. If a <code>.erl</code>
 * file cannot be parsed, its structure remains unknown. Use
 * <code>IErlElement.isStructureKnown</code> to determine whether this is the
 * case.
 * <p>
 * This interface is not intended to be implemented by clients.
 * </p>
 */
public interface IErlModule extends IErlElement, ISourceReference, IParent,
		IOpenable, ISourceManipulation {

	/**
	 * Returns the smallest element within this module that includes the given
	 * source position (that is, a clause, attribute, etc.), or
	 * <code>null</code> if there is no element other than the compilation
	 * unit itself at the given position, or if the given position is not within
	 * the source range of this compilation unit.
	 * 
	 * @param position
	 *            a source position inside the compilation unit
	 * @return the innermost Erlang element enclosing a given source position or
	 *         <code>null</code> if none (excluding the compilation unit).
	 * @throws ErlModelException
	 *             if the compilation unit does not exist or if an exception
	 *             occurs while accessing its corresponding resource
	 */
	IErlElement getElementAt(int position) throws ErlModelException;

	/**
	 * Returns whether the resource of this working copy has changed since the
	 * inception of this working copy. Returns <code>false</code> if this
	 * compilation unit is not in working copy mode.
	 * 
	 * @return whether the resource has changed
	 * 
	 */
	boolean hasResourceChanged();

	/**
	 * Is this module a real one, or a header file?
	 * 
	 * @return true if .erl, false if .hrl
	 */
	boolean isModule();

	IErlComment[] getComments();

	OtpErlangObject getParseTree();

	long getTimestamp();

	void reconcile(IDocument doc, DirtyRegion dirtyRegion);

	void reconcile(IDocument document);

	void getContentProposals(String prefix, String indent, int offset,
			ArrayList<ICompletionProposal> result);

	void reset();

	IErlImport findImport(ErlangFunction function);

	IErlImport[] getImports();

	IErlPreprocessorDef findPreprocessorDef(String definedName, String type);

	ErlangIncludeFile[] getIncludedFiles();

	IErlScanner getScanner();

}
