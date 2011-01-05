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

    public BrowserDialog(Shell parent) {
        this(parent, SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL);
    }
    
    public BrowserDialog(Shell parent, int style) {
        super(parent, style);
        
    }

    public void open() {
        Shell shell = new Shell(getParent(), getStyle());
        shell.setText("Html report");
        createContent(shell);
        shell.open();
        Display display = getParent().getDisplay();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }
    }

    public void setBrowserText(String html) {
        browser.setText(html);
    }
    
    public void setFilePath(String path) {
        this.url = "file://" + path;
    }

    private void createContent(Shell shell) {
    
        shell.setLayout(new FillLayout());

        GridData gData = new GridData();
        gData.horizontalAlignment = GridData.FILL;

        browser = new Browser(shell, SWT.NONE);
        browser.setUrl(url);

    }

}
