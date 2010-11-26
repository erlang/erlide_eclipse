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

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IStatusLineManager;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.texteditor.BasicTextEditorActionContributor;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.ITextEditorActionDefinitionIds;
import org.erlide.ui.editors.erl.actions.GotoAnnotationAction;

public class ErlEditorActionBarContributor extends
        BasicTextEditorActionContributor {

    public ErlEditorActionBarContributor() {
        super();

        // contentAssistProposal = new RetargetTextEditorAction(ErlideUIMessages
        // .getResourceBundle(), "ContentAssistProposal.");
        // ErlLogger.debug("AC 1");

        fPreviousAnnotation = new GotoAnnotationAction(
                "PreviousAnnotation.", false); //$NON-NLS-1$
        fNextAnnotation = new GotoAnnotationAction("NextAnnotation.", true); //$NON-NLS-1$
    }

    // protected RetargetTextEditorAction contentAssistProposal;
    private final GotoAnnotationAction fPreviousAnnotation;

    private final GotoAnnotationAction fNextAnnotation;

    @Override
    public void contributeToMenu(final IMenuManager menuManager) {
        final IMenuManager navigateMenu = menuManager
                .findMenuUsingPath(IWorkbenchActionConstants.M_NAVIGATE);
        if (navigateMenu != null) {
            // navigateMenu.add(new Separator());
            // navigateMenu.add(fQuickOutline);
        }
    }

    @Override
    public void setActiveEditor(final IEditorPart part) {
        super.setActiveEditor(part);

        ITextEditor editor = null;
        if (part instanceof ITextEditor) {
            editor = (ITextEditor) part;
        }

        if (part instanceof ErlangEditor) {
            final ErlangEditor erlangEditor = (ErlangEditor) part;
            erlangEditor.getActionGroup().fillActionBars(getActionBars());
        }
        // contentAssistProposal.setAction(getAction(editor,
        // "ContentAssistProposal"));

        // IActionBars bars = getActionBars();
        // bars.setGlobalActionHandler(ErlangActionIds.COMMENT,
        // getAction(editor,
        // "Comment"));
        // bars.setGlobalActionHandler(ErlangActionIds.UNCOMMENT,
        // getAction(editor,
        // "Uncomment"));

        final IActionBars actionBars = getActionBars();
        final IStatusLineManager manager = actionBars.getStatusLineManager();
        manager.setMessage(null);
        manager.setErrorMessage(null);

        fPreviousAnnotation.setEditor(editor);
        fNextAnnotation.setEditor(editor);

        final IAction showOutline = getAction(editor,
                IErlangEditorActionDefinitionIds.SHOW_OUTLINE);
        actionBars.setGlobalActionHandler(
                IErlangEditorActionDefinitionIds.SHOW_OUTLINE, showOutline);

    }

    /*
     * @see IEditorActionBarContributor#init(IActionBars, IWorkbenchPage)
     */
    @Override
    public void init(final IActionBars bars, final IWorkbenchPage page) {
        super.init(bars, page);
        // register actions that have a dynamic editor.
        bars.setGlobalActionHandler(
                ITextEditorActionDefinitionIds.GOTO_NEXT_ANNOTATION,
                fNextAnnotation);
        bars.setGlobalActionHandler(
                ITextEditorActionDefinitionIds.GOTO_PREVIOUS_ANNOTATION,
                fPreviousAnnotation);
        bars.setGlobalActionHandler(ActionFactory.NEXT.getId(), fNextAnnotation);
        bars.setGlobalActionHandler(ActionFactory.PREVIOUS.getId(),
                fPreviousAnnotation);
    }

}
