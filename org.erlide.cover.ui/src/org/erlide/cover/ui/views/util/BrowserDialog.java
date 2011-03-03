package org.erlide.cover.ui.views.util;

import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;

/**
 * Dialog for HTML browser
 * 
 * @author Aleksandra Lipiec <sleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class BrowserDialog extends Dialog {

    private Shell dialogShell;
    private Label titleLabel;
    private Composite comp;
    private Button home;
    private Button up;
    private Button next;
    private Button prev;
    private Browser browser;

    private String url;

    public BrowserDialog(final Shell parent) {
        this(parent, SWT.NULL);
    }

    public BrowserDialog(final Shell parent, final int style) {
        super(parent, style);

    }

    public void open() {
        createContent();
        dialogShell.open();
        Display display = dialogShell.getDisplay();
        while (!dialogShell.isDisposed()) {
            if (!display.readAndDispatch())
                display.sleep();
        }
    }

    public void setBrowserText(final String html) {
        browser.setText(html);
    }

    public void setFilePath(final String path) {
        url = "file://" + path;
    }

    private void createContent() {

        Shell parent = getParent();
        dialogShell = new Shell(parent, SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL);

        GridLayout dialogShellLayout = new GridLayout();
        dialogShellLayout.makeColumnsEqualWidth = true;
        dialogShell.setText("Html Report browser");
        dialogShell.setLayout(dialogShellLayout);
        dialogShell.layout();
        dialogShell.pack();
        dialogShell.setSize(800, 600);

        titleLabel = new Label(dialogShell, SWT.NONE);
        GridData titleLabelLData = new GridData();
        titleLabelLData.widthHint = 784;
        titleLabelLData.heightHint = 17;
        titleLabel.setLayoutData(titleLabelLData);
        titleLabel.setText(url);
        titleLabel.setAlignment(SWT.CENTER);

        browser = new Browser(dialogShell, SWT.NONE | SWT.BORDER);
        GridData browserLData = new GridData();
        browserLData.widthHint = 784;
        browserLData.heightHint = 500;
        browser.setLayoutData(browserLData);
        browser.setUrl(url);

        comp = new Composite(dialogShell, SWT.NONE);
        GridLayout compLayout = new GridLayout();
        compLayout.makeColumnsEqualWidth = true;
        compLayout.numColumns = 4;
        compLayout.marginLeft = 300;
        compLayout.marginRight = 300;
        GridData compLData = new GridData();
        compLData.widthHint = 521;
        compLData.heightHint = 33;
        comp.setLayoutData(compLData);
        comp.setLayout(compLayout);

        prev = new Button(comp, SWT.PUSH | SWT.CENTER | SWT.FILL);
        GridData prevLData = new GridData();
        prev.setLayoutData(prevLData);
        prev.setText("prev");

        next = new Button(comp, SWT.PUSH | SWT.CENTER | SWT.FILL);
        GridData nextLData = new GridData();
        next.setLayoutData(nextLData);
        next.setText("next");

        up = new Button(comp, SWT.PUSH | SWT.CENTER | SWT.FILL);
        GridData upLData = new GridData();
        up.setLayoutData(upLData);
        up.setText("up");

        home = new Button(comp, SWT.PUSH | SWT.CENTER | SWT.FILL);
        GridData homeLData = new GridData();
        home.setLayoutData(homeLData);
        home.setText("down");

        dialogShell.setLocation(getParent().toDisplay(100, 100));

    }
    
}
