package org.erlide.cover.ui.views.util;

import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

/**
 * Dialog for HTML browser
 * 
 * @author Aleksandra Lipiec <sleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class BrowserDialog extends Dialog {

    private Browser browser;
    private String url;

    public BrowserDialog(final Shell parent) {
        this(parent, SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL);
    }

    public BrowserDialog(final Shell parent, final int style) {
        super(parent, style);

    }

    public void open() {
        final Shell shell = new Shell(getParent(), getStyle());
        shell.setText("Html report");
        createContent(shell);
        shell.open();
        final Display display = getParent().getDisplay();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }
    }

    public void setBrowserText(final String html) {
        browser.setText(html);
    }

    public void setFilePath(final String path) {
        url = "file://" + path;
    }

    private void createContent(final Shell shell) {

        shell.setLayout(new FillLayout());

        final GridData gData = new GridData();
        gData.horizontalAlignment = GridData.FILL;

        browser = new Browser(shell, SWT.NONE);
        browser.setUrl(url);

    }

}
