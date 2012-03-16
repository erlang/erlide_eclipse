package org.erlide.ui.prefs;

import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.erlide.backend.BackendCore;
import org.erlide.utils.SystemUtils;

public class InternalSettingsPreferencePage extends PreferencePage implements
        IWorkbenchPreferencePage {

    private SystemUtils su;
    private Button btnMonitorIdeBackend;
    private Button btnTestMode;
    private Button btnDeveloperMode;
    private Button btnMakeAvailableClearcache;
    private Text text;
    private Label lblNewLabel;

    public InternalSettingsPreferencePage() {
    }

    @Override
    public void init(final IWorkbench workbench) {
        su = SystemUtils.getInstance();
    }

    @Override
    protected Control createContents(final Composite parent) {
        final Composite panel = new Composite(parent, SWT.NONE);
        panel.setLayout(new GridLayout(2, false));

        final Label lblPleaseDoNot = new Label(panel, SWT.NONE);
        lblPleaseDoNot.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false,
                false, 2, 1));
        lblPleaseDoNot
                .setText("Please do not change these values unless instructed by an erlide developer!");

        final Label lblStrangeThingsMight = new Label(panel, SWT.NONE);
        lblStrangeThingsMight.setLayoutData(new GridData(SWT.FILL, SWT.CENTER,
                false, false, 1, 1));
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

        btnMonitorIdeBackend = new Button(panel, SWT.CHECK);
        btnMonitorIdeBackend.setText("Monitor IDE backend");
        btnMonitorIdeBackend.setSelection(su.isMonitoringIdeBackend());
        new Label(panel, SWT.NONE);

        lblNewLabel = new Label(panel, SWT.NONE);
        lblNewLabel.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false,
                false, 1, 1));
        lblNewLabel.setText("Monitoring poll interval (seconds)");

        text = new Text(panel, SWT.BORDER);
        final GridData gd_text = new GridData(SWT.LEFT, SWT.CENTER, true,
                false, 1, 1);
        gd_text.widthHint = 79;
        text.setLayoutData(gd_text);
        text.setText(Integer.toString(su.getMonitoringInterval()));

        return panel;
    }

    @Override
    public boolean performOk() {
        su.setDeveloper(btnDeveloperMode.getSelection());

        su.setTest(btnTestMode.getSelection());

        su.setClearCacheAvailable(btnMakeAvailableClearcache.getSelection());

        su.setMonitoringIdeBackend(btnMonitorIdeBackend.getSelection());
        BackendCore.getBackendManager().getIdeBackend()
                .setMonitoring(su.isMonitoringIdeBackend());
        su.setMonitoringInterval(Integer.parseInt(text.getText()));
        BackendCore.getBackendManager().getIdeBackend()
                .setMonitoringInterval(su.getMonitoringInterval());

        return true;
    }
}
