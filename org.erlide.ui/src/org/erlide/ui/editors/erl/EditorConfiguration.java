/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.editors.erl;

import java.util.Map;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.IAutoEditStrategy;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.reconciler.IReconciler;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.editors.text.EditorsUI;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.ui.editors.erl.autoedit.AutoIndentStrategy;
import org.erlide.ui.editors.erl.hover.ErlTextHover;
import org.erlide.ui.editors.internal.reconciling.ErlReconciler;
import org.erlide.ui.editors.internal.reconciling.ErlReconcilingStrategy;
import org.erlide.ui.internal.information.ErlInformationPresenter;
import org.erlide.ui.internal.information.PresenterControlCreator;
import org.erlide.ui.util.IColorManager;
import org.erlide.ui.util.eclipse.text.BrowserInformationControl;

/**
 * The editor configurator
 *
 * @author Eric Merritt [cyberlync at gmail dot com]
 */
public class EditorConfiguration extends ErlangSourceViewerConfiguration {

    final AbstractErlangEditor editor;
    private ErlReconciler reconciler;

    public EditorConfiguration(final IPreferenceStore store,
            final AbstractErlangEditor editor, final IColorManager colorManager) {
        super(store, colorManager);
        this.editor = editor;
    }

    @Override
    public IAutoEditStrategy[] getAutoEditStrategies(final ISourceViewer sourceViewer,
            final String contentType) {
        if (contentType.equals(IDocument.DEFAULT_CONTENT_TYPE)) {
            return new IAutoEditStrategy[] { new AutoIndentStrategy(editor) };
        }
        return NO_AUTOEDIT;
    }

    @Override
    public ITextHover getTextHover(final ISourceViewer sourceViewer,
            final String contentType) {
        return new ErlTextHover(editor);
    }

    protected ITextEditor getEditor() {
        return editor;
    }

    @Override
    public IReconciler getReconciler(final ISourceViewer sourceViewer) {
        final ErlReconcilingStrategy strategy = new ErlReconcilingStrategy(editor);
        final IErlModule module = editor != null ? editor.getModule() : null;
        final String path = module != null ? module.getFilePath() : null;
        reconciler = new ErlReconciler(strategy, true, true, path, module, getEditor());
        reconciler.setProgressMonitor(new NullProgressMonitor());
        reconciler.setIsAllowedToModifyDocument(false);
        reconciler.setDelay(500);
        return reconciler;
    }

    @Override
    protected IErlProject getProject() {
        return ErlangEngine.getInstance().getModelUtilService().getProject(getModule());
    }

    @Override
    protected IErlModule getModule() {
        if (editor == null) {
            return null;
        }
        return editor.getModule();
    }

    @Override
    public IInformationControlCreator getInformationControlCreator(
            final ISourceViewer sourceViewer) {
        return new IInformationControlCreator() {

            @Override
            public IInformationControl createInformationControl(final Shell parent) {
                if (parent.getText().length() == 0
                        && BrowserInformationControl.isAvailable(parent)) {
                    final BrowserInformationControl info = new BrowserInformationControl(
                            parent, JFaceResources.DIALOG_FONT,
                            EditorsUI.getTooltipAffordanceString()) {
                        @Override
                        public IInformationControlCreator getInformationPresenterControlCreator() {
                            return new PresenterControlCreator(editor);
                        }
                    };
                    return info;
                }
                return new DefaultInformationControl(parent,
                        EditorsUI.getTooltipAffordanceString(),
                        new ErlInformationPresenter(true));
            }
        };
    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    @Override
    protected Map getHyperlinkDetectorTargets(final ISourceViewer sourceViewer) {
        final Map map = super.getHyperlinkDetectorTargets(sourceViewer);
        map.put("org.erlide.ui.hyperlinktarget", getEditor());
        return map;
    }

    public void resetReconciler() {
        if (reconciler != null) {
            reconciler.reset();
        }
    }

    public void reconcileNow() {
        if (reconciler != null) {
            reconciler.reconcileNow();
        }
    }
}
