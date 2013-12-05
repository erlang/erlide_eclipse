package org.erlide.ui.prefs;

import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.erlide.util.SystemConfiguration;

public class InternalSettingsPreferencePage extends PreferencePage implements
        IWorkbenchPreferencePage {

    private SystemConfiguration su;
    private Button btnTestMode;
    private Button btnDeveloperMode;
    private Button btnMakeAvailableClearcache;
    private Label lblNewLabel;
    private Label warnProcessLimitText;
    private Label lblNewLabel_1;
    private Label killProcessLimitText;

    public InternalSettingsPreferencePage() {
    }

    @Override
    public void init(final IWorkbench workbench) {
        su = SystemConfiguration.getInstance();
    }

    @Override
    protected Control createContents(final Composite parent) {
        final Composite panel = new Composite(parent, SWT.NONE);
        panel.setLayout(new GridLayout(2, false));

        final Label lblPleaseDoNot = new Label(panel, SWT.NONE);
        lblPleaseDoNot.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 2,
                1));
        lblPleaseDoNot
                .setText("Please do not change these values unless instructed by an erlide developer!");

        final Label lblStrangeThingsMight = new Label(panel, SWT.NONE);
        lblStrangeThingsMight.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false,
                false, 1, 1));
        lblStrangeThingsMight.setAlignment(SWT.RIGHT);
        lblStrangeThingsMight.setText("Strange things might happen...");
        new Label(panel, SWT.NONE);

        btnDeveloperMode = new Button(panel, SWT.CHECK);
        btnDeveloperMode.setText("Developer mode");
        btnDeveloperMode.setSelection(su.isDeveloper());
        new Label(panel, SWT.NONE);

        btnTestMode = new Button(panel, SWT.CHECK);
        btnTestMode.setText("Test mode");
        btnTestMode.setSelection(su.isTest());
        new Label(panel, SWT.NONE);

        btnMakeAvailableClearcache = new Button(panel, SWT.CHECK);
        btnMakeAvailableClearcache.setText("Make available ClearCache command");
        btnMakeAvailableClearcache.setSelection(su.isClearCacheAvailable());
        new Label(panel, SWT.NONE);

        new Label(panel, SWT.NONE);
        new Label(panel, SWT.NONE);

        lblNewLabel = new Label(panel, SWT.NONE);
        final GridData gd_lblNewLabel = new GridData(SWT.LEFT, SWT.CENTER, false, false,
                1, 1);
        gd_lblNewLabel.widthHint = 300;
        lblNewLabel.setLayoutData(gd_lblNewLabel);
        lblNewLabel.setText("-Derlide.process.heap.warn.limit (MB)");

        warnProcessLimitText = new Label(panel, SWT.BORDER);
        warnProcessLimitText.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true,
                false, 1, 1));
        warnProcessLimitText.setText(Integer.toString(su.getWarnProcessSizeLimitMB()));

        lblNewLabel_1 = new Label(panel, SWT.NONE);
        lblNewLabel_1.setText("-Derlide.process.heap.kill.limit (MB)");

        killProcessLimitText = new Label(panel, SWT.BORDER);
        killProcessLimitText.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true,
                false, 1, 1));
        killProcessLimitText.setText(Integer.toString(su.getKillProcessSizeLimitMB()));

        return panel;
    }

    @Override
    public boolean performOk() {
        su.setDeveloper(btnDeveloperMode.getSelection());

        su.setTest(btnTestMode.getSelection());

        su.setClearCacheAvailable(btnMakeAvailableClearcache.getSelection());

        return true;
    }
}
