package org.erlide.ui.editors.erl.actions;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentExtension4;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchSite;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.runtime.shell.BackendShellListener;
import org.erlide.runtime.shell.IBackendShell;
import org.erlide.ui.actions.SelectionDispatchAction;
import org.erlide.ui.console.ErlangConsolePage;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.util.ErlLogger;

public class SendToConsoleAction extends SelectionDispatchAction {

    private final class ConsoleBackendShellListener implements
            BackendShellListener {

        private int offset;
        private int counter;

        public ConsoleBackendShellListener() {
            offset = 0;
            counter = 0;
        }

        public void setup(final int offset) {
            this.offset = offset;
            counter = 0;
        }

        @Override
        public void changed(final IBackendShell shell) {
            if (shell == null) {
                return;
            }
            counter++;
            if (counter != 2) {
                return;
            }
            final String[] messages = shell.getLastMessages(1);
            if (messages.length == 0) {
                return;
            }
            addMessage(offset, messages[0]);
        }
    }

    private final ITextEditor fEditor;
    private final boolean getOutput;
    private final ConsoleBackendShellListener consoleBackendShellListener;
    private ErlangConsolePage consolePage;

    @Override
    public void run(ITextSelection selection) {
        final ErlangConsolePage consolePage = ErlideUIPlugin.getDefault()
                .getConsolePage();
        // make sure we have a console page to send it to
        if (consolePage != null) {
            if (this.consolePage != consolePage && this.consolePage != null) {
                this.consolePage.getShell().removeListener(
                        consoleBackendShellListener);
            }
            // if selection is empty, grab the whole line
            selection = getLineSelection(selection, false);
            // try to make the text a full erlang expression, ending with dot
            String text = selection.getText().trim();
            if (text.endsWith(",") || text.endsWith(";")) { //$NON-NLS-1$ //$NON-NLS-2$
                text = text.substring(0, text.length() - 1);
            }
            if (!text.endsWith(".")) { //$NON-NLS-1$
                text += "."; //$NON-NLS-1$
            }
            text += "\n"; //$NON-NLS-1$
            // send it off to the console
            if (getOutput) {
                consoleBackendShellListener.setup(getLineSelection(selection,
                        true).getOffset());
                consolePage.getShell().addListener(consoleBackendShellListener);
            }
            consolePage.input(text);
        }
        super.run(selection);
    }

    public void addMessage(final int offset, final String message) {
        final IDocument document = fEditor.getDocumentProvider().getDocument(
                fEditor.getEditorInput());
        try {
            final String delimiter = document.getLineDelimiter(document
                    .getLineOfOffset(offset - 1));
            String nl = "";
            if (delimiter == null) {
                if (document instanceof IDocumentExtension4) {
                    final IDocumentExtension4 documentExtension4 = (IDocumentExtension4) document;
                    nl = documentExtension4.getDefaultLineDelimiter();
                } else {
                    final String[] delimiters = document
                            .getLegalLineDelimiters();
                    nl = delimiters[0];
                }
            }
            final Display display = ErlideUIPlugin.getStandardDisplay();
            final String addMessage = nl + message;
            display.asyncExec(new Runnable() {

                @Override
                public void run() {
                    try {
                        document.replace(offset, 0, addMessage);
                    } catch (final BadLocationException e) {
                    }
                }
            });
            ErlLogger.debug("message %s", message);
        } catch (final BadLocationException e) {
        }
    }

    protected ITextSelection getLineSelection(ITextSelection selection,
            final boolean beginningOfNextLine) {
        final IDocument document = fEditor.getDocumentProvider().getDocument(
                fEditor.getEditorInput());
        if (selection.getLength() == 0) { // don't use isEmpty()!
            selection = ErlangTextEditorAction.extendSelectionToWholeLines(
                    document, selection);
        }
        if (beginningOfNextLine) {
            final int endLine = selection.getEndLine();
            int offset;
            try {
                offset = document.getLineOffset(endLine)
                        + document.getLineLength(endLine);
                selection = new TextSelection(offset, 0);
            } catch (final BadLocationException e) {
                offset = document.getLength();
            }
        }
        return selection;
    }

    public SendToConsoleAction(final IWorkbenchSite site,
            final ResourceBundle bundle, final String prefix,
            final ITextEditor editor, final boolean getOutput) {
        super(site);
        this.getOutput = getOutput;
        setText(getString(bundle, prefix + "label")); //$NON-NLS-1$
        setToolTipText(getString(bundle, prefix + "tooltip")); //$NON-NLS-1$
        setDescription(getString(bundle, prefix + "description")); //$NON-NLS-1$
        fEditor = editor;
        consoleBackendShellListener = new ConsoleBackendShellListener();
    }

    protected static String getString(final ResourceBundle bundle,
            final String key) {
        try {
            return bundle.getString(key);
        } catch (final MissingResourceException x) {
        }
        return key;
    }

}
