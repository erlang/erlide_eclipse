package org.erlide.ui.editors.erl.actions;

import java.util.ResourceBundle;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.erl.outline.QuickOutlinePopupDialog;

public class ShowOutlineAction extends TextEditorAction {

    public ShowOutlineAction(final ResourceBundle bundle, final String prefix,
            final ITextEditor editor) {
        super(bundle, prefix, editor);
    }

    @Override
    public void run() {
        if (getTextEditor() instanceof ErlangEditor) {
            final ErlangEditor editor = (ErlangEditor) getTextEditor();
            final Shell parent = getTextEditor().getSite().getShell();
            final QuickOutlinePopupDialog quickOutlinePopupDialog = new QuickOutlinePopupDialog(
                    parent, SWT.NONE, editor, editor, editor);

            quickOutlinePopupDialog.open();
        }
    }
}
