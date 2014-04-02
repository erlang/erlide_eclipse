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
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.erlang.IErlMember;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.services.text.IndentResult;
import org.erlide.engine.services.text.IndentService;
import org.erlide.ui.editors.erl.AbstractErlangEditor;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.prefs.plugin.IndentationPreferencePage;
import org.erlide.util.ErlLogger;

/**
 * The erlang auto indent strategy
 *
 *
 * @author Eric Merritt [cyberlync at gmail dot com]
 */
public class AutoIndentStrategy implements IAutoEditStrategy {
    // extends DefaultIndentLineAutoEditStrategy {

    private final AbstractErlangEditor editor;

    public AutoIndentStrategy(final AbstractErlangEditor editor) {
        super();
        this.editor = editor;
    }

    private void autoIndentAfterNewLine(final IDocument d, final DocumentCommand c) {
        try {
            indentAfterNewLine(d, c);
        } catch (final BadLocationException e) {
            ErlLogger.warn(e);
        }
    }

    protected void indentAfterNewLine(final IDocument d, final DocumentCommand c)
            throws BadLocationException {
        if (editor == null) {
            return;
        }
        final int offset = c.offset;
        String txt = null;
        editor.reconcileNow();
        final IErlElement element = editor.getElementAt(offset, false);
        final IErlMember member = (IErlMember) element;
        if (member != null) {
            final int start = member.getSourceRange().getOffset();
            if (offset >= start) {
                txt = d.get(start, offset - start);
            }
        }
        if (txt == null) {
            txt = "";
        }
        final int lineN = d.getLineOfOffset(offset);
        final int lineOffset = d.getLineOffset(lineN);
        final int lineLength = d.getLineLength(lineN);
        final String oldLine = d.get(offset, lineLength + lineOffset - offset);
        try {
            final int tabw = getTabWidthFromPreferences();

            final Map<String, String> prefs = new TreeMap<String, String>();
            IndentationPreferencePage.addKeysAndPrefs(prefs);
            SmartTypingPreferencePage.addAutoNLKeysAndPrefs(prefs);
            final boolean useTabs = getUseTabsFromPreferences();
            final IndentResult res = ErlangEngine.getInstance()
                    .getService(IndentService.class)
                    .indentLine(oldLine, txt, c.text, tabw, useTabs, prefs);

            if (res.isAddNewLine()) {
                c.text += "\n";
            }
            c.text += res.getText();
            c.length += res.getRemoveNext();
        } catch (final Exception e) {
            ErlLogger.warn(e);
        }
    }

    public static int getIndentWidthFromPreferences() {
        final int tabw = ErlideUIPlugin.getDefault().getPreferenceStore()
                .getInt(ErlangEditor.EDITOR_INDENT_WIDTH);
        return tabw;
    }

    public static int getTabWidthFromPreferences() {
        final int tabw = EditorsUI.getPreferenceStore().getInt(
                AbstractDecoratedTextEditorPreferenceConstants.EDITOR_TAB_WIDTH);
        return tabw;
    }

    public static boolean getUseTabsFromPreferences() {
        return !EditorsUI.getPreferenceStore().getBoolean(
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
    @Override
    public void customizeDocumentCommand(final IDocument d, final DocumentCommand c) {
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
