/**
 *
 */
package org.erlide.ui.editors.erl.test;

import java.util.ResourceBundle;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;
import org.erlide.core.internal.model.erlang.ErlideScanner;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.jinterface.ErlLogger;

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
            final String s = ErlideScanner.checkAll(module.getScannerName(),
                    text);
            ErlLogger.debug("%s", s);
            // return;
        }

        Set<IErlModule> deps;
        try {
            deps = module.getDirectDependentModules();
            ErlLogger.debug(deps.toString());
            deps = module.getAllDependentModules();
            ErlLogger.debug(deps.toString());
        } catch (final CoreException e) {
            e.printStackTrace();
        }

    }
}
