/*******************************************************************************
 * Copy    right (c) 2009 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.console;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.rules.ITokenScanner;
import org.eclipse.jface.text.source.ISourceViewer;
import org.erlide.runtime.shell.IoRequest.IoRequestKind;
import org.erlide.ui.editors.erl.EditorConfiguration;
import org.erlide.ui.editors.erl.scanner.ErlCodeScanner;
import org.erlide.ui.editors.erl.scanner.ErlDamagerRepairer;
import org.erlide.ui.util.IColorManager;

final public class ErlangConsoleSourceViewerConfiguration extends
        EditorConfiguration {

    public ErlangConsoleSourceViewerConfiguration(final IPreferenceStore store,
            final IColorManager colorManager) {
        super(store, null, colorManager);
    }

    /**
     * Creates the reconciler
     * 
     * @see org.eclipse.jface.text.source.SourceViewerConfiguration#getPresentationReconciler(org.eclipse.jface.text.source.ISourceViewer)
     */
    @Override
    public IPresentationReconciler getPresentationReconciler(
            final ISourceViewer sourceViewer) {
        final PresentationReconciler reconciler = new PresentationReconciler();
        DefaultDamagerRepairer dr;

        final ITokenScanner scan = new ErlCodeScanner(colorManager);
        dr = new ErlDamagerRepairer(scan);
        reconciler.setDamager(dr, IoRequestKind.INPUT.name());
        reconciler.setRepairer(dr, IoRequestKind.INPUT.name());

        final ITokenScanner scan3 = new ConsoleOutputScanner(colorManager);
        dr = new ErlDamagerRepairer(scan3);
        reconciler.setDamager(dr, IoRequestKind.OUTPUT.name());
        reconciler.setRepairer(dr, IoRequestKind.OUTPUT.name());

        reconciler.setDamager(dr, IoRequestKind.PROMPT.name());
        reconciler.setRepairer(dr, IoRequestKind.PROMPT.name());

        reconciler.setDamager(dr, IoRequestKind.STDOUT.name());
        reconciler.setRepairer(dr, IoRequestKind.STDOUT.name());

        reconciler.setDamager(dr, IoRequestKind.STDERR.name());
        reconciler.setRepairer(dr, IoRequestKind.STDERR.name());

        reconciler.setDamager(dr, IoRequestKind.HEADER.name());
        reconciler.setRepairer(dr, IoRequestKind.HEADER.name());

        // this is for the input field
        final ITokenScanner scan2 = new ErlCodeScanner(colorManager);
        dr = new ErlDamagerRepairer(scan2);
        reconciler.setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE);
        reconciler.setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE);

        return reconciler;
    }
}
