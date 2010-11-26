/**
 *
 */
package org.erlide.ui.editors.erl.test;

import java.util.ResourceBundle;
import java.util.Set;

import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.text.ErlangToolkit;
import org.erlide.jinterface.util.ErlLogger;

import erlang.ErlideScanner;

/**
 * @author jakob
 * 
 */
public class TestAction extends TextEditorAction {

    private final IErlModule module;

    public TestAction(final ResourceBundle bundle, final String prefix,
            final ITextEditor editor, final IErlModule module) {
        super(bundle, prefix, editor);
        this.module = module;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        super.run();
        if (module == null) {
            return;
        }
        final ITextEditor textEditor = getTextEditor();
        {
            final IDocument document = textEditor.getDocumentProvider()
                    .getDocument(textEditor.getEditorInput());
            final String text = document.get();
            final String s = ErlideScanner.checkAll(
                    ErlangToolkit.createScannerModuleName(module), text);
            ErlLogger.debug("%s", s);
            // return;
        }

        Set<IErlModule> deps;
        try {
            deps = module.getDirectDependents();
            ErlLogger.debug(deps.toString());
            deps = module.getAllDependents();
            ErlLogger.debug(deps.toString());
        } catch (final ErlModelException e) {
            e.printStackTrace();
        }

    }
}
