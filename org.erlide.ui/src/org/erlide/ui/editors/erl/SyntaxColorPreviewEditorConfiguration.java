/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.editors.erl;

import java.util.Map;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.IAutoEditStrategy;
import org.eclipse.jface.text.ITextDoubleClickStrategy;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.source.ISourceViewer;
import org.erlide.ui.prefs.HighlightStyle;
import org.erlide.ui.prefs.SyntaxColorPreviewHighlightScanner;
import org.erlide.ui.prefs.TokenHighlight;
import org.erlide.ui.util.IColorManager;

public class SyntaxColorPreviewEditorConfiguration extends EditorConfiguration {

    private final Map<TokenHighlight, HighlightStyle> styles;

    public SyntaxColorPreviewEditorConfiguration(final IPreferenceStore store,
            final IColorManager lcolorManager,
            final Map<TokenHighlight, HighlightStyle> styles) {
        super(store, null, lcolorManager);
        this.styles = styles;
    }

    /**
     * The double click strategy
     * 
     * @see org.eclipse.jface.text.source.SourceViewerConfiguration#getDoubleClickStrategy(org.eclipse.jface.text.source.ISourceViewer,
     *      java.lang.String)
     */
    @Override
    public ITextDoubleClickStrategy getDoubleClickStrategy(
            final ISourceViewer sourceViewer, final String contentType) {
        return null;
    }

    /**
     * 
     * @param sourceViewer
     * @param contentType
     * @return
     */
    public IAutoEditStrategy getAutoEditStrategy(
            final ISourceViewer sourceViewer, final String contentType) {
        return null;
    }

    @Override
    public ITextHover getTextHover(final ISourceViewer sourceViewer,
            final String contentType) {
        return null;
    }

    @Override
    protected ErlHighlightScanner getHighlightScanner(
            final ISourceViewer sourceViewer) {
        if (fHighlightScanner == null) {
            fHighlightScanner = new SyntaxColorPreviewHighlightScanner(
                    colorManager, sourceViewer, styles);
        }
        return fHighlightScanner;
    }

}
