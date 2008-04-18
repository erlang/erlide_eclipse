/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Eric Merritt
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.editors.erl;

import java.util.Arrays;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DefaultIndentLineAutoEditStrategy;
import org.eclipse.jface.text.DocumentCommand;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextUtilities;
import org.eclipse.ui.editors.text.EditorsUI;
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditorPreferenceConstants;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlMember;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.IBackend;
import org.erlide.ui.ErlideUIPlugin;

import erlang.ErlideIndent;

/**
 * The erlang auto indent strategy
 * 
 * 
 * @author Eric Merritt [cyberlync at gmail dot com]
 */
public class AutoIndentStrategy extends DefaultIndentLineAutoEditStrategy {

	private final ErlangEditor fEditor;

	public AutoIndentStrategy(ErlangEditor editor) {
		super();
		fEditor = editor;
	}

	/**
	 * The default indent depth
	 */
	// private static final int INDENT_DEPTH = 4;
	/**
	 * Get the actual indent itself
	 * 
	 * @param depth
	 *            the depth of the indent;
	 * @return the indent
	 */
	private String getIndent(int depth) {
		final char[] x = new char[depth];
		Arrays.fill(x, ' ');

		return new String(x);
	}

	protected void autoIndentAfterNewLine(IDocument d, DocumentCommand c) {
		try {
			indentAfterNewLine(d, c);
		} catch (final BadLocationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	@SuppressWarnings("boxing")
	protected void indentAfterNewLine(IDocument d, DocumentCommand c)
			throws BadLocationException {
		final int offset = c.offset;
		String txt = null;
		final IErlElement element = fEditor.getElementAt(offset, false);
		final IErlMember member = (IErlMember) element;
		if (member != null) {
			int start;
			try {
				start = member.getSourceRange().getOffset();
				txt = d.get(start, offset - start);
			} catch (final ErlModelException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		if (txt == null) {
			txt = d.get(0, offset);
		}
		final int lineN = d.getLineOfOffset(offset);
		final int lineOffset = d.getLineOffset(lineN);
		final int lineLength = d.getLineLength(lineN);
		final String line = d.get(offset, lineLength + lineOffset - offset);
		try {
			final IBackend b = BackendManager.getDefault().getIdeBackend();
			int tabw = ErlideUIPlugin
					.getDefault()
					.getPreferenceStore()
					.getInt(
							AbstractDecoratedTextEditorPreferenceConstants.EDITOR_TAB_WIDTH);
			if (tabw == 0) {
				tabw = EditorsUI
						.getPreferenceStore()
						.getInt(
								AbstractDecoratedTextEditorPreferenceConstants.EDITOR_TAB_WIDTH);
			}

			final int[] prefs = new int[] {}; // TODO hämta prefs (sista
												// argumentet)
			final int indents[] = ErlideIndent.indentLine(b, line, txt, -1,
					tabw, prefs);

			c.text += getIndent(indents[0]);
			c.length += indents[1];
		} catch (final Exception e) {
			e.printStackTrace();
		}
	}

	// private int getLastLineIndent(String txt) {
	// int offset = txt.lastIndexOf('\n');
	// if (offset == -1) {
	// offset = 0;
	// }
	// int r = 0;
	// while (offset < txt.length()
	// && Character.isWhitespace(txt.charAt(offset))) {
	// ++r;
	// }
	// return r;
	// }

	private int getIndent(String line) {
		int i = 0;
		while (i < line.length() && line.charAt(i) == ' ') {
			++i;
		}
		return i;
	}

	/**
	 * Override a DocumentCommand if it ends with a line delim (CR) to include
	 * space characters for autoindentation
	 * 
	 * @param d
	 *            the document
	 * @param c
	 *            the command
	 */

	@Override
	public void customizeDocumentCommand(IDocument d, DocumentCommand c) {
		if (c.length == 0
				&& c.text != null
				&& TextUtilities.endsWith(d.getLegalLineDelimiters(), c.text) != -1) {
			autoIndentAfterNewLine(d, c);
		}
	}

}
