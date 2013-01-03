/**
 *
 */
package org.erlide.ui.editors.erl.test;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ResourceBundle;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;
import org.erlide.core.internal.model.erlang.ErlideScanner;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.utils.ErlLogger;

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
            final String scannerName = module.getScannerName();
            final String s = ErlideScanner.checkAll(scannerName, text);
            ErlLogger.debug("%s", s);
            final String scannerText = ErlideScanner.getText(scannerName);
            dumpText(scannerText, "/tmp/scanner.txt");
            dumpText(text, "/tmp/editor.txt");
            if (textEditor instanceof ErlangEditor) {
                final ErlangEditor editor = (ErlangEditor) textEditor;
                ErlideScanner.dumpLog(editor.getModule().getScannerName(),
                        "/tmp/x.scanner.log");
                editor.dumpReconcilerLog("/tmp/x.reconciler.log");
            }
            if (module != null) {
                return;
            }
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

    private void dumpText(final String text, final String filename) {
        try {
            final OutputStream out = new BufferedOutputStream(
                    new FileOutputStream(new File(filename)));
            out.write(text.getBytes());
            out.close();
        } catch (final IOException e) {
            e.printStackTrace();
        }
    }
}
