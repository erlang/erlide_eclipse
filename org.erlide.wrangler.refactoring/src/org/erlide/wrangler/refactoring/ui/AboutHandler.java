package org.erlide.wrangler.refactoring.ui;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

public class AboutHandler extends AbstractHandler {

    protected static class MyMessageDialog extends Dialog {

        String title, message;

        protected MyMessageDialog(final IShellProvider parentShell) {
            super(parentShell);
        }

        public MyMessageDialog(final Shell shell, final String title,
                final String message) {
            super(shell);
            this.title = title;
            this.message = message;

        }

        @Override
        protected void createButtonsForButtonBar(final Composite parent) {
            // create OK and Cancel buttons by default
            createButton(parent, IDialogConstants.OK_ID,
                    IDialogConstants.OK_LABEL, true);
        }

        @Override
        protected void configureShell(final Shell shell) {
            super.configureShell(shell);
            shell.setText(title);
        }

        @Override
        protected Control createDialogArea(final Composite parent) {
            final Composite composite = (Composite) super
                    .createDialogArea(parent);

            final Link url1 = new Link(composite, SWT.BORDER);
            url1.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                    | GridData.HORIZONTAL_ALIGN_FILL));

            url1.setText(message);

            return composite;
        }
    }

    @Override
    public Object execute(final ExecutionEvent event)
            throws org.eclipse.core.commands.ExecutionException {

        final MyMessageDialog m = new MyMessageDialog(
                PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                "Missing Graphviz library",
                "For using Wrangler code inspection functionalities you must first install the Eclipse GraphViz plugin (and also the original graphviz binaries).\nUpdate site: <a src=\"http://oroszgy.github.com/eclipsegraphviz/update/\">http://oroszgy.github.com/eclipsegraphviz/update/</a>");

        m.open();

        // MyMessageDialog
        // .openMessage(
        // PlatformUI.getWorkbench().getActiveWorkbenchWindow()
        // .getShell(),
        // "Missing Graphviz plugin",
        // "For using Wrangler code inspection functionalities you must first install the Eclipse GraphViz plugin (and also the original graphviz binaries).");

        return null;
    }
}
