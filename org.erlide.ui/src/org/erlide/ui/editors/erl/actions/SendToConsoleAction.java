package org.erlide.ui.editors.erl.actions;

import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jdt.annotation.NonNull;
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
import org.erlide.backend.api.IBackend;
import org.erlide.backend.api.IBackendManager;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.runtime.shell.BackendShellEvent;
import org.erlide.runtime.shell.BackendShellListener;
import org.erlide.runtime.shell.IBackendShell;
import org.erlide.ui.actions.SelectionDispatchAction;
import org.erlide.ui.console.ErlConsoleManager;
import org.erlide.ui.console.IErlangConsole;
import org.erlide.ui.console.IErlangConsolePage;
import org.erlide.ui.handlers.ErlangAbstractHandler;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.util.ErlLogger;

public class SendToConsoleAction extends SelectionDispatchAction {

    private final class ConsoleBackendShellListener implements BackendShellListener {

        private final int offset;
        private int counter;
        private final IBackendShell shell;

        public ConsoleBackendShellListener(final IBackendShell shell, final int offset) {
            this.shell = shell;
            this.offset = offset;
            counter = 0;
        }

        @Override
        public void changed(final BackendShellEvent event) {
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
    private ConsoleBackendShellListener consoleBackendShellListener;
    IErlProject project;

    @Override
    public void run(final ITextSelection selection0) {
        ITextSelection selection = selection0;
        IErlangConsole console = null;
        final IProject workspaceProject = project.getWorkspaceProject();
        if (workspaceProject == null) {
            return;
        }
        console = getConsole(workspaceProject);
        if (console == null) {
            final String message = "There is no runtime launched for this backend. Please start a runtime to send commands to.";
            final Exception x = new Exception("No runtime started");
            ErrorDialog.openError(getShell(), "No runtime", message, new Status(
                    IStatus.ERROR, ErlideUIPlugin.PLUGIN_ID, 0, x.getMessage(), x));
            return;
        }
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
            consoleBackendShellListener = new ConsoleBackendShellListener(
                    console.getShell(), getLineSelection(selection, true).getOffset());
            console.getShell().addListener(consoleBackendShellListener);
        }

        final IErlangConsolePage consolePage = ErlideUIPlugin.getDefault()
                .getErlConsoleManager().getPage(console);
        consolePage.input(text);
        super.run(selection);
    }

    private IErlangConsole getConsole(final @NonNull IProject aproject) {
        final IBackendManager backendManager = BackendCore.getBackendManager();
        final Set<IBackend> executionBackends = backendManager
                .getExecutionBackends(aproject);
        final ErlConsoleManager erlConsoleManager = ErlideUIPlugin.getDefault()
                .getErlConsoleManager();
        IErlangConsole result = null;
        for (final IBackend backend : executionBackends) {
            result = erlConsoleManager.getConsole(backend);
            if (result != null) {
                break;
            }
        }
        return result;
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
                    final String[] delimiters = document.getLegalLineDelimiters();
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

    protected ITextSelection getLineSelection(final ITextSelection selection0,
            final boolean beginningOfNextLine) {
        ITextSelection selection = selection0;
        final IDocument document = editor.getDocumentProvider().getDocument(
                editor.getEditorInput());
        if (selection.getLength() == 0) { // don't use isEmpty()!
            selection = ErlangAbstractHandler.extendSelectionToWholeLines(document,
                    selection);
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

    public SendToConsoleAction(final IWorkbenchSite site, final ResourceBundle bundle,
            final String prefix, final ITextEditor editor, final boolean getOutput,
            final IErlProject project) {
        super(site);
        this.getOutput = getOutput;
        this.project = project;
        setText(getString(bundle, prefix + "label")); //$NON-NLS-1$
        setToolTipText(getString(bundle, prefix + "tooltip")); //$NON-NLS-1$
        setDescription(getString(bundle, prefix + "description")); //$NON-NLS-1$
        this.editor = editor;
    }

    protected static String getString(final ResourceBundle bundle, final String key) {
        try {
            return bundle.getString(key);
        } catch (final MissingResourceException x) {
        }
        return key;
    }

}
