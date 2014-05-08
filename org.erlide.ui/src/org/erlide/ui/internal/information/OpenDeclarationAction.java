package org.erlide.ui.internal.information;

import org.eclipse.jface.action.Action;
import org.eclipse.ui.PartInitException;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.services.search.OpenResult;
import org.erlide.ui.ErlideImage;
import org.erlide.ui.actions.OpenUtils;
import org.erlide.ui.editors.erl.AbstractErlangEditor;
import org.erlide.ui.editors.util.EditorUtility;
import org.erlide.ui.internal.ErlBrowserInformationControlInput;
import org.erlide.ui.util.eclipse.text.BrowserInformationControl;
import org.erlide.ui.views.EdocView;
import org.erlide.util.ErlLogger;

/**
 * Action that opens the current hover input element.
 *
 * @since 3.4
 */
public final class OpenDeclarationAction extends Action {
    private final BrowserInformationControl fInfoControl;
    private final EdocView edocView;

    public OpenDeclarationAction(final BrowserInformationControl infoControl,
            final AbstractErlangEditor editor) {
        fInfoControl = infoControl;
        edocView = null;
        setText("Open declaration");
        ErlideImage.setLocalImageDescriptors(this, "goto_input.gif");
    }

    public OpenDeclarationAction(final EdocView edocView) {
        this.edocView = edocView;
        fInfoControl = null;
        setText("Open declaration");
        ErlideImage.setLocalImageDescriptors(this, "goto_input.gif");
    }

    /*
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        ErlBrowserInformationControlInput input = null;
        if (fInfoControl != null) {
            input = (ErlBrowserInformationControlInput) fInfoControl.getInput();
            fInfoControl.notifyDelayedInputChange(null);
            fInfoControl.dispose();
        } else if (edocView != null) {
            input = edocView.getInput();
        }
        if (input != null) {
            // TODO: add hover location to editor navigation history?
            try {
                final Object element = input.getInputElement();
                if (element instanceof IErlElement) {
                    EditorUtility.openElementInEditor(element, true);
                } else if (element instanceof OpenResult) {
                    final OpenResult or = (OpenResult) element;
                    try {
                        final AbstractErlangEditor editor = input.getEditor();
                        new OpenUtils().openOpenResult(editor, editor.getModule(), -1,
                                null, or, null);
                    } catch (final Exception e) {
                        ErlLogger.error(e);
                    }
                }
            } catch (final PartInitException e) {
                ErlLogger.error(e);
            }
        }
    }
}
