package org.erlide.ui.prefs;

import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.wb.swt.SWTResourceManager;
import org.erlide.backend.BackendUtils;

public class NetworkPreferencePage extends PreferencePage implements
        IWorkbenchPreferencePage {
    private Text shortNameText;
    private Text longNameText;
    private Text javaShortNameText;
    private Text javaLongNameText;

    public NetworkPreferencePage() {
        super();
        noDefaultAndApplyButton();
    }

    @Override
    public void init(final IWorkbench workbench) {
        // TODO Auto-generated method stub

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

        // addSelectionChangedListener(new ISelectionChangedListener() {
        //
        // @Override
        // public void selectionChanged(final SelectionChangedEvent event) {
        // checkValid();
        // }
        // });
        //
        // checkValid();

        applyDialogFont(parent);
        return parent;
    }

    private Control createMyControl(final Composite ancestor) {
        final Composite parent = new Composite(ancestor, SWT.NULL);
        final GridLayout layout = new GridLayout();
        layout.numColumns = 2;
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        parent.setLayout(layout);
        final Font font = ancestor.getFont();
        parent.setFont(font);

        final Label lblNewLabel = new Label(parent, SWT.NONE);
        lblNewLabel.setFont(SWTResourceManager.getFont("Sans", 10, SWT.NORMAL));
        final GridData gd_lblNewLabel = new GridData(SWT.FILL, SWT.CENTER,
                false, false, 2, 1);
        gd_lblNewLabel.widthHint = 69;
        lblNewLabel.setLayoutData(gd_lblNewLabel);
        lblNewLabel.setText("Host names used by the internal Erlang backends");

        final Label lblNewLabel_8 = new Label(parent, SWT.NONE);
        lblNewLabel_8.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false,
                false, 2, 1));
        lblNewLabel_8.setText("This is just informational, to aid debugging.");

        final Label lblNewLabel_1 = new Label(parent, SWT.NONE);
        lblNewLabel_1.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false,
                false, 1, 1));
        lblNewLabel_1.setText("Short name (-sname)");

        shortNameText = new Text(parent, SWT.BORDER);
        shortNameText.setEditable(false);
        shortNameText.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true,
                false, 1, 1));

        final Label lblNewLabel_2 = new Label(parent, SWT.NONE);
        lblNewLabel_2.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false,
                false, 1, 1));
        lblNewLabel_2.setText("Long name (-name)");

        longNameText = new Text(parent, SWT.BORDER);
        longNameText.setEditable(false);
        longNameText.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true,
                false, 1, 1));
        new Label(parent, SWT.NONE);

        final Button btnNewButton = new Button(parent, SWT.NONE);
        btnNewButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                BackendUtils.detectHostNames();
                updateHostNames();
            }

        });
        btnNewButton.setText("Detect Erlang host names");
        new Label(parent, SWT.NONE);

        final Label lblNewLabel_3 = new Label(parent, SWT.WRAP);
        final GridData gd_lblNewLabel_3 = new GridData(SWT.FILL, SWT.CENTER,
                false, false, 1, 1);
        gd_lblNewLabel_3.widthHint = 234;
        lblNewLabel_3.setLayoutData(gd_lblNewLabel_3);
        lblNewLabel_3
                .setText("This is needed if you change network or network settings.\n\nIf you see \"127.0.0.1\" and \"localhost\" above, you will not be able to debug distributed remote nodes.");

        final Label lblNewLabel_7 = new Label(parent, SWT.NONE);
        new Label(parent, SWT.NONE);
        new Label(parent, SWT.NONE);

        final Label lblNewLabel_4 = new Label(parent, SWT.NONE);
        lblNewLabel_4.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false,
                false, 1, 1));
        lblNewLabel_4.setText("For reference, Java sees the following values:");

        final Label lblNewLabel_5 = new Label(parent, SWT.NONE);
        lblNewLabel_5.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false,
                false, 1, 1));
        lblNewLabel_5.setText("short");

        javaShortNameText = new Text(parent, SWT.BORDER);
        javaShortNameText.setEnabled(false);
        javaShortNameText.setEditable(false);
        javaShortNameText.setLayoutData(new GridData(SWT.FILL, SWT.CENTER,
                true, false, 1, 1));

        final Label lblNewLabel_6 = new Label(parent, SWT.NONE);
        lblNewLabel_6.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false,
                false, 1, 1));
        lblNewLabel_6.setText("long");

        javaLongNameText = new Text(parent, SWT.BORDER);
        javaLongNameText.setEnabled(false);
        javaLongNameText.setEditable(false);
        javaLongNameText.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true,
                false, 1, 1));

        updateHostNames();

        return parent;
    }

    private void updateHostNames() {
        shortNameText.setText(BackendUtils.getErlangShortHostName());
        longNameText.setText(BackendUtils.getErlangLongHostName());
        javaShortNameText.setText(BackendUtils.getJavaShortHostName());
        javaLongNameText.setText(BackendUtils.getJavaLongHostName());
    }
}
