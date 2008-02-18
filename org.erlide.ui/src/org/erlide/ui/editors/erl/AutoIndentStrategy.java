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

import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;

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
			} catch (ErlModelException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		if (txt == null) {
			txt = d.get(0, offset);
		}
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
			final OtpErlangObject r1 = b.rpcx("erlide_indent",
					"indent_next_line", "si", txt, tabw);
			// OtpErlangObject r1 = BackendUtil.checkRpc(b.rpc("erlide_indent",
			// "indent_next_line",
			// new OtpErlangLong(offset)));
			final int depth = ((OtpErlangLong) r1).intValue();

			c.text += getIndent(depth);
			// int cntr = 0;
			// int depth = INDENT_DEPTH;
			// char val;
			// boolean end = false;
			// for (int i = c.offset - c.text.length() + 1; (i >= 0) && (!end);
			// i--)
			// {
			// val = d.getChar(i);
			//
			// switch (val)
			// {
			// case '"':
			// i = findStringStart(d, i - 1);
			// break;
			// case ';':
			// case '.':
			// cntr++;
			// break;
			// case '>':
			// if ((i > 0) && (d.getChar(i - 1) == '-'))
			// {
			// cntr--;
			//
			// if (cntr < 0)
			// {
			// depth += findIndentDepth(d, i);
			// end = true;
			// }
			// }
			// break;
			// }
			//
			// }
			// c.text += getIndent(depth);
		} catch (final Exception e) {
		}
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
