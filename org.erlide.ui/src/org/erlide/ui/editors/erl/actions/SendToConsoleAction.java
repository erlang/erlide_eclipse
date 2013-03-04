package org.erlide.ui.editors.erl.actions;

import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.Set;

import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentExtension4;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchSite;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.backend.BackendCore;
import org.erlide.backend.IBackend;
import org.erlide.backend.IBackendManager;
import org.erlide.model.root.IErlProject;
import org.erlide.model.util.PluginUtils;
import org.erlide.runtime.shell.BackendShellListener;
import org.erlide.runtime.shell.IBackendShell;
import org.erlide.ui.actions.SelectionDispatchAction;
import org.erlide.ui.console.ErlConsoleManager;
import org.erlide.ui.console.IErlangConsole;
import org.erlide.ui.console.IErlangConsolePage;
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

    private final ITextEditor editor;
    private final boolean getOutput;
    private final ConsoleBackendShellListener consoleBackendShellListener;
    IErlProject project;

    @Override
    public void run(ITextSelection selection) {
        final IBackendManager backendManager = BackendCore.getBackendManager();
        final Set<IBackend> executionBackends = backendManager
                .getExecutionBackends(project.getWorkspaceProject());
        IErlangConsole console = null;
        final ErlConsoleManager erlConsoleManager = ErlideUIPlugin.getDefault()
                .getErlConsoleManager();
        for (final IBackend backend : executionBackends) {
            console = erlConsoleManager.getConsole(backend);
            if (console != null) {
                break;
            }
        }
        if (console == null) {
            final String message = "There is no runtime launched for this backend. Please start a runtime to send commands to.";
            ErrorDialog
                    .openError(getShell(), "No runtime", message, PluginUtils
                            .makeStatus(new Exception("No runtime started")));

            return;
        }
        // make sure we have a console page to send it to
        final IErlangConsolePage consolePage = ErlideUIPlugin.getDefault()
                .getErlConsoleManager().getPage(console);
        console.getShell().removeListener(consoleBackendShellListener);
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
            consoleBackendShellListener.setup(getLineSelection(selection, true)
                    .getOffset());
            console.getShell().addListener(consoleBackendShellListener);
        }
        consolePage.input(text);
        super.run(selection);
    }

    public void addMessage(final int offset, final String message) {
        final IDocument document = editor.getDocumentProvider().getDocument(
                editor.getEditorInput());
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
        final IDocument document = editor.getDocumentProvider().getDocument(
                editor.getEditorInput());
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
            final ITextEditor editor, final boolean getOutput,
            final IErlProject project) {
        super(site);
        this.getOutput = getOutput;
        this.project = project;
        setText(getString(bundle, prefix + "label")); //$NON-NLS-1$
        setToolTipText(getString(bundle, prefix + "tooltip")); //$NON-NLS-1$
        setDescription(getString(bundle, prefix + "description")); //$NON-NLS-1$
        this.editor = editor;
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
