package org.erlide.ui.editors.erl.correction.assists;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PlatformUI;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.services.parsing.ErlToken;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.erl.correction.QuickFixExecutor;

public class ConvertMultiStringQuickFix extends QuickFixExecutor {

    @Override
    public void run() throws Exception {
        super.run();
    }

    @Override
    public boolean appliesAt(final IQuickAssistInvocationContext invocationContext) {
        final ISourceViewer viewer = invocationContext.getSourceViewer();

        final IEditorPart editorPart = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage().getActiveEditor();
        if (!(editorPart instanceof ErlangEditor)) {
            return false;
        }
        final ErlangEditor editor = (ErlangEditor) editorPart;
        if (editor.getViewer() != viewer) {
            return false;
        }

        final IErlModule module = editor.getModule();

        final int offset = invocationContext.getOffset();
        final ErlToken token = module.getScanner().getTokenAt(offset);
        if (token == null || token.getKind() != ErlToken.KIND_STRING) {
            return false;
        }
        final IDocument doc = viewer.getDocument();
        String text;
        try {
            text = doc.get(token.getOffset(), token.getLength());
            if (text.contains("\n")) {
                return true;
            }
        } catch (final BadLocationException e) {
        }
        return false;
    }
}
