package org.erlide.cover.ui.views.util;

import java.io.File;

import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.browser.LocationEvent;
import org.eclipse.swt.browser.LocationListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.erlide.cover.core.Logger;
import org.erlide.cover.ui.Activator;
import org.erlide.cover.ui.Images;
import org.erlide.cover.views.model.ICoverageObject;
import org.erlide.cover.views.model.StatsTreeModel;

/**
 * Dialog for HTML browser
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class BrowserDialog extends Dialog {

    private Shell dialogShell;
    private Composite comp;
    private Button home;
    private Button up;
    private Button next;
    private Button prev;
    private Browser browser;
    private ICoverageObject object;

    private String url;
    private Logger log; // logger

    public BrowserDialog(final Shell parent) {
        this(parent, SWT.NULL);
    }

    public BrowserDialog(final Shell parent, final int style) {
        super(parent, style);
        log = Activator.getDefault();
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

    /**
     * Sets object connected with current path
     * 
     * @param path
     */
    public void setObject(final ICoverageObject obj) {
        object = obj;
        url = "file://" + obj.getHtmlPath();
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

        browser = new Browser(dialogShell, SWT.NONE | SWT.BORDER);
        GridData browserLData = new GridData();
        browserLData.widthHint = 784;
        browserLData.heightHint = 500;
        browser.setLayoutData(browserLData);
        browser.setUrl(object.getHtmlPath());
        browser.addLocationListener(locationListener);

        comp = new Composite(dialogShell, SWT.NONE);
        GridLayout compLayout = new GridLayout();
        compLayout.makeColumnsEqualWidth = true;
        compLayout.numColumns = 4;
        compLayout.marginLeft = 330;
        compLayout.marginRight = 330;
        GridData compLData = new GridData();
        compLData.widthHint = 521;
        compLData.heightHint = 33;
        comp.setLayoutData(compLData);
        comp.setLayout(compLayout);

        prev = new Button(comp, SWT.PUSH | SWT.CENTER | SWT.FILL);
        GridData prevLData = new GridData();
        prev.setLayoutData(prevLData);
        // prev.setText("prev");
        prev.addSelectionListener(prevListener);
        prev.setImage(Activator.getImageDescriptor(Images.PREV).createImage());

        next = new Button(comp, SWT.PUSH | SWT.CENTER | SWT.FILL);
        GridData nextLData = new GridData();
        next.setLayoutData(nextLData);
        // next.setText("next");
        next.addSelectionListener(nextListener);
        next.setImage(Activator.getImageDescriptor(Images.NEXT).createImage());

        if (object.getParent() != null) {
            if (object.getParent().getNextSiblingTo(object.getLabel()) == null)
                next.setEnabled(false);
            if (object.getParent().getPrevSiblingTo(object.getLabel()) == null)
                prev.setEnabled(false);
        } else {
            next.setEnabled(false);
            prev.setEnabled(false);
        }

        home = new Button(comp, SWT.PUSH | SWT.CENTER | SWT.FILL);
        GridData homeLData = new GridData();
        home.setLayoutData(homeLData);
        // home.setText("down");
        home.addSelectionListener(homeListener);
        home.setImage(Activator.getImageDescriptor(Images.HOME).createImage());

        up = new Button(comp, SWT.PUSH | SWT.CENTER | SWT.FILL);
        GridData upLData = new GridData();
        up.setLayoutData(upLData);
        // up.setText("up");
        up.addSelectionListener(upListener);
        up.setImage(Activator.getImageDescriptor(Images.UP).createImage());

        dialogShell.setLocation(getParent().toDisplay(100, 100));

    }

    private SelectionListener prevListener = new SelectionListener() {

        public void widgetSelected(SelectionEvent e) {
            if (object.getParent() != null) {
                ICoverageObject sib = object.getParent().getPrevSiblingTo(
                        object.getLabel());
                if (sib == null) {
                    // CoverageHelper.reportInfo("No previous pages");
                    return;
                }
                setObject(sib);
                browser.setUrl(url);
                // disable buttons
                next.setEnabled(true);
                if (object.getParent().getPrevSiblingTo(object.getLabel()) == null)
                    prev.setEnabled(false);
            }
        }

        public void widgetDefaultSelected(SelectionEvent e) {
            // TODO Auto-generated method stub
        }

    };

    private SelectionListener nextListener = new SelectionListener() {

        public void widgetSelected(SelectionEvent e) {
            if (object.getParent() != null) {
                ICoverageObject sib = object.getParent().getNextSiblingTo(
                        object.getLabel());
                if (sib == null) {
                    // CoverageHelper.reportInfo("No further pages");
                    return;
                }
                setObject(sib);
                browser.setUrl(url);
                // disable buttons
                prev.setEnabled(true);
                if (object.getParent().getNextSiblingTo(object.getLabel()) == null)
                    next.setEnabled(false);
            }
        }

        public void widgetDefaultSelected(SelectionEvent e) {
            // TODO Auto-generated method stub
        }

    };

    private SelectionListener upListener = new SelectionListener() {

        public void widgetSelected(SelectionEvent e) {
            if (object.getParent() != null) {
                setObject(object.getParent());
                browser.setUrl(url);

                if (object.getParent().getNextSiblingTo(object.getLabel()) == null)
                    next.setEnabled(false);
                if (object.getParent().getPrevSiblingTo(object.getLabel()) == null)
                    prev.setEnabled(false);

            }
        }

        public void widgetDefaultSelected(SelectionEvent e) {
            // TODO Auto-generated method stub
        }

    };

    private SelectionListener homeListener = new SelectionListener() {

        public void widgetSelected(SelectionEvent e) {
            setObject(StatsTreeModel.getInstance().getRoot());
            browser.setUrl(url);
            next.setEnabled(false);
            prev.setEnabled(false);
        }

        public void widgetDefaultSelected(SelectionEvent e) {
            // TODO Auto-generated method stub
        }

    };

    private LocationListener locationListener = new LocationListener() {

        public void changing(LocationEvent event) {
            // TODO Auto-generated method stub

        }

        public void changed(LocationEvent event) {
            if (object == null)
                return;
            log.info(event.getSource());
            String newUrl = browser.getUrl();
            String name = newUrl.substring(newUrl.lastIndexOf(File.separator) + 1,
                    newUrl.length() - 5);
            log.info(name);
            if (name.startsWith("mod_"))
                name = name.substring(4);

            ICoverageObject newObj;
            if ((newObj = object.treeSearch(name)) != null) { // TODO ->
                                                              // findChild
                setObject(newObj);
                if (object.getParent() == null) {
                    up.setEnabled(false);
                    next.setEnabled(false);
                    prev.setEnabled(false);
                } else {
                    up.setEnabled(true);
                    if (object.getParent().getNextSiblingTo(object.getLabel()) == null)
                        next.setEnabled(false);
                    else
                        next.setEnabled(true);
                    if (object.getParent().getPrevSiblingTo(object.getLabel()) == null)
                        prev.setEnabled(false);
                    else
                        prev.setEnabled(true);
                }
            }

        }

    };

}
