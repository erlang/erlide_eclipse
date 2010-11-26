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
package org.erlide.ui.editors.erl.autoedit;

import java.util.Map;
import java.util.TreeMap;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DocumentCommand;
import org.eclipse.jface.text.IAutoEditStrategy;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextUtilities;
import org.eclipse.ui.editors.text.EditorsUI;
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditorPreferenceConstants;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlMember;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.prefs.plugin.IndentationPreferencePage;

import erlang.ErlideIndent;
import erlang.IndentResult;

/**
 * The erlang auto indent strategy
 * 
 * 
 * @author Eric Merritt [cyberlync at gmail dot com]
 */
public class AutoIndentStrategy implements IAutoEditStrategy {
    // extends DefaultIndentLineAutoEditStrategy {

    private final ErlangEditor fEditor;

    public AutoIndentStrategy(final ErlangEditor editor) {
        super();
        fEditor = editor;
    }

    private void autoIndentAfterNewLine(final IDocument d,
            final DocumentCommand c) {
        try {
            indentAfterNewLine(d, c);
        } catch (final BadLocationException e) {
            ErlLogger.warn(e);
        }
    }

    protected void indentAfterNewLine(final IDocument d, final DocumentCommand c)
            throws BadLocationException {
        final int offset = c.offset;
        String txt = null;
        fEditor.reconcileNow();
        final IErlElement element = fEditor.getElementAt(offset, false);
        final IErlMember member = (IErlMember) element;
        if (member != null) {
            int start;
            try {
                start = member.getSourceRange().getOffset();
                txt = d.get(start, offset - start);
            } catch (final ErlModelException e) {
                ErlLogger.warn(e);
            }
        }
        if (txt == null) {
            txt = d.get(0, offset);
        }
        final int lineN = d.getLineOfOffset(offset);
        final int lineOffset = d.getLineOffset(lineN);
        final int lineLength = d.getLineLength(lineN);
        final String oldLine = d.get(offset, lineLength + lineOffset - offset);
        try {
            final Backend b = ErlangCore.getBackendManager().getIdeBackend();
            final int tabw = getTabWidthFromPreferences();

            final Map<String, String> prefs = new TreeMap<String, String>();
            IndentationPreferencePage.addKeysAndPrefs(prefs);
            SmartTypingPreferencePage.addAutoNLKeysAndPrefs(prefs);
            final boolean useTabs = getUseTabsFromPreferences();
            final IndentResult res = ErlideIndent.indentLine(b, oldLine, txt,
                    c.text, tabw, useTabs, prefs);

            if (res.isAddNewLine()) {
                c.text += "\n";
            }
            c.text += res.getText();
            c.length += res.getRemoveNext();
        } catch (final Exception e) {
            ErlLogger.warn(e);
        }
    }

    /**
     * @return
     */
    public static int getTabWidthFromPreferences() {
        int tabw = ErlideUIPlugin
                .getDefault()
                .getPreferenceStore()
                .getInt(AbstractDecoratedTextEditorPreferenceConstants.EDITOR_TAB_WIDTH);
        if (tabw == 0) {
            tabw = EditorsUI
                    .getPreferenceStore()
                    .getInt(AbstractDecoratedTextEditorPreferenceConstants.EDITOR_TAB_WIDTH);
        }
        return tabw;
    }

    public static boolean getUseTabsFromPreferences() {
        return !EditorsUI
                .getPreferenceStore()
                .getBoolean(
                        AbstractDecoratedTextEditorPreferenceConstants.EDITOR_SPACES_FOR_TABS);
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

    // FIXME flytta en del av denna logik till erlang!! (t.ex. s� vill man
    // inte
    // vara "elektrisk" i kommentarer)
    public void customizeDocumentCommand(final IDocument d,
            final DocumentCommand c) {
        if (c.length == 0 && c.text != null) {
            if (TextUtilities.endsWith(d.getLegalLineDelimiters(), c.text) != -1) {
                autoIndentAfterNewLine(d, c);
            } else if (c.text.endsWith(",")) {
                autoIndentAfterNewLine(d, c);
            } else if (c.text.endsWith(";")) {
                autoIndentAfterNewLine(d, c);
            } else if (c.text.endsWith(".")) {
                autoIndentAfterNewLine(d, c);
            } else if (c.text.endsWith(">")) {
                try {
                    if (c.offset > 0 && c.offset <= d.getLength()
                            && d.getChar(c.offset - 1) == '-') {
                        autoIndentAfterNewLine(d, c);
                    }
                } catch (final BadLocationException e) {
                    // never mind...
                }
            }
        }
    }
}
