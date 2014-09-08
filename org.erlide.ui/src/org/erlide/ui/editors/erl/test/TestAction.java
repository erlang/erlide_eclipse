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
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.ISourceUnit;
import org.erlide.engine.services.parsing.InternalScanner;
import org.erlide.util.ErlLogger;
import org.erlide.util.Util;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

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
            final IDocument document = textEditor.getDocumentProvider().getDocument(
                    textEditor.getEditorInput());
            final String text = document.get();
            final String scannerName = module.getScannerName();

            final InternalScanner internalScanner = (InternalScanner) ErlangEngine
                    .getInstance().getSimpleScannerService();
            final OtpErlangObject checkAll = internalScanner.checkAll(scannerName, text,
                    true);
            String s;
            if (checkAll instanceof OtpErlangTuple) {
                final OtpErlangTuple t = (OtpErlangTuple) checkAll;
                s = Util.stringValue(t.elementAt(0));
                final OtpErlangObject o1 = t.elementAt(1);
                final OtpErlangObject o2 = t.elementAt(2);
                dumpText(o1.toString(), "/tmp/scannerTokens.txt");
                dumpText(o2.toString(), "/tmp/rescanTokens.txt");
            } else {
                s = Util.stringValue(checkAll);
            }
            ErlLogger.info("%s", s);
            if (module != null) {
                return;
            }
        }

        Set<ISourceUnit> deps;
        try {
            deps = module.getDirectDependentModules();
            ErlLogger.debug(deps.toString());
            deps = module.getAllDependentModules();
            ErlLogger.debug(deps.toString());
        } catch (final CoreException e) {
            ErlLogger.error(e);
        }

    }

    private void dumpText(final String text, final String filename) {
        try {
            final OutputStream out = new BufferedOutputStream(new FileOutputStream(
                    new File(filename)));
            out.write(text.getBytes());
            out.close();
        } catch (final IOException e) {
            ErlLogger.error(e);
        }
    }
}
