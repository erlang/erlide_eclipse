/*******************************************************************************
 * Copyright (c) 2000, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.debug.ui.properties;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.jface.text.TextViewerUndoManager;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerActivation;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.texteditor.ITextEditorActionDefinitionIds;
import org.erlide.jinterface.ErlLogger;
import org.erlide.launch.debug.IErlangBreakpoint;
import org.erlide.ui.editors.erl.scanner.IErlangPartitions;

/**
 * The widget for the conditional editor on the breakpoints properties page
 */
public class BreakpointConditionEditor {

    SourceViewer fViewer;
    // private IContentAssistProcessor fCompletionProcessor;
    private String fOldValue;
    private String fErrorMessage;
    private final ErlangLineBreakpointPropertyPage fPage;
    private final IErlangBreakpoint fBreakpoint;
    private IHandlerService fHandlerService;
    private IHandler fHandler;
    private IHandlerActivation fActivation;
    private IDocumentListener fDocumentListener;

    /**
     * Constructor
     * 
     * @param parent
     *            the parent to add this widget to
     * @param page
     *            the page that is associated with this widget
     */
    public BreakpointConditionEditor(final Composite parent,
            final ErlangLineBreakpointPropertyPage page) {
        fPage = page;
        fBreakpoint = fPage.getBreakpoint();
        String condition = "";
        try {
            condition = fBreakpoint.getCondition();
            fErrorMessage = "Enter a condition";
            fOldValue = ""; //$NON-NLS-1$

            fViewer = new SourceViewer(parent, null, SWT.BORDER | SWT.V_SCROLL
                    | SWT.H_SCROLL | SWT.LEFT_TO_RIGHT);
            fViewer.setInput(parent);
            final IDocument document = new Document();
            fViewer.setDocumentPartitioning(IErlangPartitions.ERLANG_PARTITIONING);
            // setDocumentPartitioning
            // JDIDebugUIPlugin.getDefault().getJavaTextTools()
            // .setupJavaDocumentPartitioner(document,
            // IJavaPartitions.JAVA_PARTITIONING);
            // we can only do code assist if there is an associated type
            // final IResource r = fBreakpoint.getMarker().getResource();
            // if (r != null) {
            // final IErlModel model = ErlangCore.getModel();
            // final IErlElement element = model.findElement(r);
            // if (element!= null && element.getKind() ==
            // IErlElement.Kind.MODULE) {
            // final int lineNumber = fBreakpoint.getMarker()
            // .getAttribute(IMarker.LINE_NUMBER, -1);
            // IJavaDebugContentAssistContext context = null;
            // final IType type = BreakpointUtils.getType(fBreakpoint);
            // if (type == null) {
            // context = new TypeContext(null, -1);
            // } else {
            // try {
            // String source = null;
            // final ICompilationUnit compilationUnit = type
            // .getCompilationUnit();
            // if (compilationUnit != null
            // && compilationUnit.getJavaProject().getProject()
            // .exists()) {
            // source = compilationUnit.getSource();
            // } else {
            // final IClassFile classFile = type.getClassFile();
            // if (classFile != null) {
            // source = classFile.getSource();
            // }
            // }
            // final int lineNumber = fBreakpoint.getMarker()
            // .getAttribute(IMarker.LINE_NUMBER, -1);
            // int position = -1;
            // if (source != null && lineNumber != -1) {
            // try {
            // position = new Document(source)
            // .getLineOffset(lineNumber - 1);
            // } catch (final BadLocationException e) {
            // JDIDebugUIPlugin.log(e);
            // }
            // }
            // context = new TypeContext(type, position);
            // } catch (final CoreException e) {
            // JDIDebugUIPlugin.log(e);
            // }
            // }
            // fCompletionProcessor = new
            // JavaDebugContentAssistProcessor(context);
            // fViewer.configure(new DisplayViewerConfiguration() {
            // public IContentAssistProcessor getContentAssistantProcessor() {
            // return fCompletionProcessor;
            // }
            // });
            fViewer.setEditable(true);
            // if we don't check upstream tracing can throw assertion exceptions
            // see bug 181914
            document.set(condition == null ? "" : condition); //$NON-NLS-1$
            fViewer.setDocument(document);
            fViewer.setUndoManager(new TextViewerUndoManager(10));
            fViewer.getUndoManager().connect(fViewer);
            fDocumentListener = new IDocumentListener() {
                @Override
                public void documentAboutToBeChanged(final DocumentEvent event) {
                }

                @Override
                public void documentChanged(final DocumentEvent event) {
                    valueChanged();
                }
            };
            fViewer.getDocument().addDocumentListener(fDocumentListener);
            final GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
            gd.heightHint = fPage.convertHeightInCharsToPixels(10);
            gd.widthHint = fPage.convertWidthInCharsToPixels(40);
            fViewer.getControl().setLayoutData(gd);
            fHandler = new AbstractHandler() {
                @Override
                public Object execute(final ExecutionEvent event)
                        throws org.eclipse.core.commands.ExecutionException {
                    fViewer.doOperation(ISourceViewer.CONTENTASSIST_PROPOSALS);
                    return null;
                }
            };
            fHandlerService = (IHandlerService) PlatformUI.getWorkbench()
                    .getAdapter(IHandlerService.class);
        } catch (final CoreException exception) {
            ErlLogger.warn(exception);
        }
    }

    /**
     * Returns the condition defined in the source viewer.
     * 
     * @return the contents of this condition editor
     */
    public String getCondition() {
        return fViewer.getDocument().get();
    }

    /**
     * @see org.eclipse.jface.preference.FieldEditor#refreshValidState()
     */
    protected void refreshValidState() {
        if (!fViewer.isEditable()) {
            fPage.removeErrorMessage(fErrorMessage);
        } else {
            final String text = fViewer.getDocument().get();
            if (!(text != null && text.trim().length() > 0)) {
                fPage.addErrorMessage(fErrorMessage);
            } else {
                fPage.removeErrorMessage(fErrorMessage);
            }
        }
    }

    /**
     * @see org.eclipse.jface.preference.FieldEditor#setEnabled(boolean,
     *      org.eclipse.swt.widgets.Composite)
     */
    public void setEnabled(final boolean enabled) {
        fViewer.setEditable(enabled);
        fViewer.getTextWidget().setEnabled(enabled);
        if (enabled) {
            // fViewer.updateViewerColors();
            fViewer.getTextWidget().setFocus();
            fActivation = fHandlerService.activateHandler(
                    ITextEditorActionDefinitionIds.CONTENT_ASSIST_PROPOSALS,
                    fHandler);
        } else {
            final Color color = fViewer.getControl().getDisplay()
                    .getSystemColor(SWT.COLOR_WIDGET_BACKGROUND);
            fViewer.getTextWidget().setBackground(color);
            if (fActivation != null) {
                fHandlerService.deactivateHandler(fActivation);
            }
        }
        valueChanged();
    }

    /**
     * Handle that the value changed
     */
    protected void valueChanged() {
        final String newValue = fViewer.getDocument().get();
        if (!newValue.equals(fOldValue)) {
            fOldValue = newValue;
        }
        refreshValidState();
    }

    /**
     * Dispose of the handlers, etc
     */
    public void dispose() {
        if (fViewer.isEditable()) {
            fHandlerService.deactivateHandler(fActivation);
        }
        fViewer.getDocument().removeDocumentListener(fDocumentListener);
        // fViewer.dispose();
    }
}
