/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.util;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.text.edits.MalformedTreeException;
import org.eclipse.text.edits.TextEdit;
import org.erlide.runtime.ErlLogger;

/**
 * 
 * 
 * @author Vlad Dumitrescu
 */
public class Util {

	/**
	 * Apply the given edit on the given string and return the updated string.
	 * Return the given string if anything wrong happen while applying the edit.
	 * 
	 * @param original
	 *            the given string
	 * @param edit
	 *            the given edit
	 * 
	 * @return the updated string
	 */
	public static final String editedString(String original, TextEdit edit) {
		if (edit == null) {
			return original;
		}
		final SimpleDocument document = new SimpleDocument(original);
		try {
			edit.apply(document, TextEdit.NONE);
			return document.get();
		} catch (final MalformedTreeException e) {
			ErlLogger.warn(e);
		} catch (final BadLocationException e) {
			ErlLogger.warn(e);
		}
		return original;
	}

}
