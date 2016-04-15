package org.erlide.ui.prefs;

import java.util.List;

import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.program.Program;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.xtext.xbase.lib.Functions;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.erlide.runtime.api.RuntimeCore;
import org.erlide.ui.util.ColorManager;
import org.erlide.util.ErlangHostnameRetriever;
import org.erlide.util.HostnameChecker;

public class TroubleshootingPreferencePage extends PreferencePage
        implements IWorkbenchPreferencePage {
    private Text fileLongText;
    private Text fileShortText;
    private Text erlLongText;
    private Text javaLongText;
    private Text fallbackLongText;
    private Text erlShortText;
    private Text javaShortText;
    private Text fallbackShortText;
    private Label longLabel;
    private Label shortLabel;

    final private String runtime = RuntimeCore.getRuntimeInfoCatalog().getErlideRuntime()
            .getOtpHome();
    final private ErlangHostnameRetriever retriever = new ErlangHostnameRetriever(
            runtime);
    final private ColorManager cm = new ColorManager();
    final private RGB lightRed = new RGB(255, 220, 220);
    final private RGB lightGreen = new RGB(220, 255, 220);
    final private RGB darkRed = new RGB(145, 0, 0);
    final private RGB darkGreen = new RGB(0, 145, 0);

    public TroubleshootingPreferencePage() {
        super();
        noDefaultAndApplyButton();
    }

    @Override
    public void init(final IWorkbench workbench) {
    }

    @Override
    protected Control createContents(final Composite parent) {
        initializeDialogUnits(parent);

        final GridLayout layout = new GridLayout();
        layout.numColumns = 1;
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        parent.setLayout(layout);

        final Control ctrl = createMyControl(parent);
        final GridData data = new GridData(GridData.FILL_BOTH);
        data.horizontalSpan = 1;
        ctrl.setLayoutData(data);

        applyDialogFont(parent);
        return parent;
    }

    private Control createMyControl(final Composite ancestor) {
        final CTabFolder folder = new CTabFolder(ancestor, SWT.FLAT);
        folder.setSimple(false);
        final CTabItem item = new CTabItem(folder, SWT.NONE);
        item.setText("Connection");
        folder.setSelection(item);

        final Composite parent = new Composite(folder, SWT.NULL);
        item.setControl(parent);
        parent.setLayout(new GridLayout(1, false));

        final Label lblNewLabel = new Label(parent, SWT.WRAP);
        final GridData gd_lblNewLabel = new GridData(SWT.LEFT, SWT.CENTER, false, false,
                1, 1);
        gd_lblNewLabel.widthHint = 479;
        lblNewLabel.setLayoutData(gd_lblNewLabel);
        lblNewLabel.setText(
                "Erlide can't connect to the backend if the network (host names) is not configured properly.\n\n"
                        + "Usually it is enough to add the names under \"from Erlang\" to /etc/hosts (or similar).\n\n"
                        + "You can also try to edit the values in ~/.erlide.hosts (here or directly in the file).  Green color on the fields means that the value works.\n\n"
                        + "WARNING! Wrong values may make erlide unusable! Delete ~/.erlide.hosts to force redetection.");

        final Composite composite = new Composite(parent, SWT.NONE);
        composite.setLayout(new GridLayout(4, false));
        final GridData gd_composite = new GridData(SWT.FILL, SWT.CENTER, false, false, 1,
                1);
        gd_composite.heightHint = 139;
        gd_composite.widthHint = 465;
        composite.setLayoutData(gd_composite);
        new Label(composite, SWT.NONE);

        longLabel = new Label(composite, SWT.NONE);
        longLabel.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, false, false, 1, 1));
        longLabel.setText("-name");

        shortLabel = new Label(composite, SWT.NONE);
        shortLabel
                .setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, false, false, 1, 1));
        shortLabel.setText("-sname");
        new Label(composite, SWT.NONE);

        final Label lblNewLabel_2 = new Label(composite, SWT.NONE);
        lblNewLabel_2
                .setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, true, 1, 1));
        lblNewLabel_2.setText("~/.erlide.hosts");

        fileLongText = new Text(composite, SWT.BORDER);
        final GridData gd_fileLongText = new GridData(SWT.FILL, SWT.CENTER, true, false,
                1, 1);
        gd_fileLongText.widthHint = 120;
        fileLongText.setLayoutData(gd_fileLongText);

        fileShortText = new Text(composite, SWT.BORDER);
        fileShortText
                .setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));

        final Button btnSave = new Button(composite, SWT.NONE);
        btnSave.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                boolean ok = true;
                if (retriever.canConnect(fileLongText.getText(), true)) {
                    fileLongText.setBackground(cm.getColor(lightGreen));
                } else {
                    fileLongText.setBackground(cm.getColor(lightRed));
                    ok = false;
                }
                if (retriever.canConnect(fileShortText.getText(), false)) {
                    fileShortText.setBackground(cm.getColor(lightGreen));
                } else {
                    fileShortText.setBackground(cm.getColor(lightRed));
                    ok = false;
                }

                if (ok) {
                    HostnameChecker.getInstance().saveErlideHosts(fileLongText.getText(),
                            fileShortText.getText());
                }
            }
        });
        btnSave.setText("Save");

        final Label lblNewLabel_3 = new Label(composite, SWT.NONE);
        lblNewLabel_3
                .setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
        lblNewLabel_3.setText("from Erlang");

        erlLongText = new Text(composite, SWT.BORDER);
        erlLongText.setEditable(false);
        erlLongText.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));

        erlShortText = new Text(composite, SWT.BORDER);
        erlShortText.setEditable(false);
        erlShortText.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
        new Label(composite, SWT.NONE);

        final Label lblNewLabel_4 = new Label(composite, SWT.NONE);
        lblNewLabel_4
                .setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
        lblNewLabel_4.setText("from Java");

        javaLongText = new Text(composite, SWT.BORDER);
        javaLongText.setEditable(false);
        javaLongText.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));

        javaShortText = new Text(composite, SWT.BORDER);
        javaShortText.setEditable(false);
        javaShortText
                .setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
        new Label(composite, SWT.NONE);

        final Label lblNewLabel_5 = new Label(composite, SWT.NONE);
        lblNewLabel_5
                .setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
        lblNewLabel_5.setText("fallback");

        fallbackLongText = new Text(composite, SWT.BORDER);
        fallbackLongText.setEditable(false);
        fallbackLongText
                .setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));

        fallbackShortText = new Text(composite, SWT.BORDER);
        fallbackShortText.setEditable(false);
        fallbackShortText
                .setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
        new Label(composite, SWT.NONE);
        new Label(composite, SWT.NONE);
        new Label(composite, SWT.NONE);
        new Label(composite, SWT.NONE);
        new Label(composite, SWT.NONE);

        final Label lblNewLabel_1 = new Label(parent, SWT.WRAP);
        final GridData gd_lblNewLabel_1 = new GridData(SWT.LEFT, SWT.CENTER, false, false,
                1, 1);
        gd_lblNewLabel_1.widthHint = 459;
        lblNewLabel_1.setLayoutData(gd_lblNewLabel_1);
        lblNewLabel_1.setText(
                "When using \"127.0.0.1\" or \"localhost\", you will not be able to debug distributed remote nodes.\r\n");

        final Link link = new Link(parent, SWT.NONE);
        link.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
        link.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                // TODO open eclise help instead
                Program.launch(link.getToolTipText());
            }
        });
        link.setToolTipText("https://github.com/erlide/erlide/wiki/Troubleshooting");
        link.setText("<a>More detailed troubleshooting information</a>");

        updateHostNames();

        return parent;
    }

    private void updateHostNames() {
        final List<List<Functions.Function0<? extends String>>> cfg = HostnameChecker
                .getInstance().getAllHostNameValues(runtime);
        set_async(fileLongText, cfg.get(0).get(0), retriever, true);
        set_async(fileShortText, cfg.get(1).get(0), retriever, false);
        set_async(fallbackLongText, cfg.get(0).get(3), retriever, true);
        set_async(fallbackShortText, cfg.get(1).get(3), retriever, false);
        set_async(erlLongText, cfg.get(0).get(1), retriever, true);
        set_async(erlShortText, cfg.get(1).get(1), retriever, false);
        set_async(javaShortText, cfg.get(1).get(2), retriever, false);
        set_async(javaLongText, cfg.get(0).get(2), retriever, true);

        if (HostnameChecker.getInstance().canUseLongNames()) {
            longLabel.setForeground(cm.getColor(darkGreen));
        } else {
            longLabel.setForeground(cm.getColor(darkRed));
        }
        if (HostnameChecker.getInstance().canUseShortNames()) {
            shortLabel.setForeground(cm.getColor(darkGreen));
        } else {
            shortLabel.setForeground(cm.getColor(darkRed));
        }

        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                if (retriever.canConnect(fileLongText.getText(), true)) {
                    fileLongText.setBackground(cm.getColor(lightGreen));
                } else {
                    fileLongText.setBackground(cm.getColor(lightRed));
                }
                if (retriever.canConnect(fileShortText.getText(), false)) {
                    fileShortText.setBackground(cm.getColor(lightGreen));
                } else {
                    fileShortText.setBackground(cm.getColor(lightRed));
                }
            }
        });
    }

    private void set_async(final Text text, final Function0<? extends String> function,
            final ErlangHostnameRetriever r, final boolean isLong) {
        text.setText("...wait...");
        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                final String name = fix(function.apply());
                text.setText(fix(name));
            }
        });
    }

    private String fix(final String string) {
        if (string == null) {
            return "";
        }
        return string;
    }

    @Override
    public void dispose() {
        cm.dispose();
        super.dispose();
    }
}
