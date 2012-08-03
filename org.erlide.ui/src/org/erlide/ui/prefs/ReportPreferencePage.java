/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.prefs;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.progress.UIJob;
import org.eclipse.wb.swt.SWTResourceManager;
import org.erlide.backend.BackendCore;
import org.erlide.backend.BackendHelper;
import org.erlide.backend.IBackend;
import org.erlide.jinterface.ErlLogger;
import org.erlide.utils.LogUtil;

public class ReportPreferencePage extends PreferencePage implements
        IWorkbenchPreferencePage {
    public ReportPreferencePage() {
    }

    private Button sendButton;
    private Label responseLabel_1;
    private Text locationLabel;
    Text fcontact;
    Text fbody;
    Text ftitle;

    private Label responseLabel;
    Button attachTechnicalDataButton;
    private Label lblDescription;

    @Override
    protected Control createContents(final Composite parent) {
        final Composite panel = new Composite(parent, SWT.NONE);
        panel.setLayout(new GridLayout(2, false));
        final Label titleLabel = new Label(panel, SWT.NONE);
        titleLabel.setAlignment(SWT.RIGHT);
        {
            final GridData gridData = new GridData(SWT.RIGHT, SWT.CENTER,
                    false, false, 1, 1);
            gridData.widthHint = 60;
            titleLabel.setLayoutData(gridData);
        }
        titleLabel.setText("Title:");
        ftitle = new Text(panel, SWT.BORDER);
        ftitle.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1,
                1));

        lblDescription = new Label(panel, SWT.NONE);
        lblDescription.setAlignment(SWT.RIGHT);
        lblDescription.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false,
                false, 1, 1));
        lblDescription.setText("Description:");

        fbody = new Text(panel, SWT.V_SCROLL | SWT.MULTI | SWT.BORDER
                | SWT.WRAP);
        {
            final GridData gridData = new GridData(SWT.FILL, SWT.FILL, true,
                    false, 1, 1);
            gridData.heightHint = 150;
            fbody.setLayoutData(gridData);
        }
        fbody.setText("(enter error description here, paste any relevant code too)");
        final Label contactoptionalLabel = new Label(panel, SWT.NONE);
        contactoptionalLabel.setAlignment(SWT.RIGHT);
        contactoptionalLabel.setText("Contact (email):");
        fcontact = new Text(panel, SWT.BORDER);
        fcontact.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false,
                1, 1));
        fcontact.setText(System.getProperty("user.name"));
        attachTechnicalDataButton = new Button(panel, SWT.CHECK);
        attachTechnicalDataButton.setLayoutData(new GridData(SWT.CENTER,
                SWT.CENTER, false, false, 2, 1));
        attachTechnicalDataButton.setSelection(true);
        attachTechnicalDataButton
                .setText("   Attach technical data (eclipse and erlide logs)   ");
        sendButton = new Button(panel, SWT.NONE);
        {
            final GridData gridData = new GridData(SWT.CENTER, SWT.CENTER,
                    false, false, 2, 1);
            gridData.widthHint = 243;
            sendButton.setLayoutData(gridData);
        }
        sendButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                postReport();
            }
        });
        sendButton.setText("Create report");
        responseLabel = new Label(panel, SWT.CENTER);
        responseLabel.setVisible(false);
        responseLabel.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, false,
                false, 2, 1));
        responseLabel
                .setText("The report was saved in the location below, you can now close this window.");
        locationLabel = new Text(panel, SWT.CENTER | SWT.WRAP | SWT.READ_ONLY);
        locationLabel.setVisible(false);
        locationLabel.setForeground(SWTResourceManager
                .getColor(SWT.COLOR_DARK_BLUE));
        locationLabel.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false,
                false, 2, 1));
        locationLabel.setBackground(Display.getCurrent().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));
        locationLabel.setText("location");
        responseLabel_1 = new Label(panel, SWT.CENTER);
        responseLabel_1.setVisible(false);
        responseLabel_1.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false,
                false, 2, 1));
        responseLabel_1
                .setText("Please attach the report if you are writing a trouble ticket.");

        noDefaultAndApplyButton();

        return panel;
    }

    protected void postReport() {

        sendButton.setText("Generating report, please wait...");
        sendButton.setEnabled(false);
        sendButton.update();

        final boolean attach = attachTechnicalDataButton.getSelection();
        final String title = ftitle.getText();
        final String contact = fcontact.getText();
        final String body = fbody.getText();

        final Job j = new Job("send error report") {
            @Override
            public IStatus run(final IProgressMonitor monitor) {
                final String location = LogUtil.getReportFile();

                final ProblemData data = gatherProblemData(attach, title,
                        contact, body);
                sendToDisk(location, data);

                final Job inner = new UIJob("update report ui") {
                    @Override
                    public IStatus runInUIThread(final IProgressMonitor amonitor) {
                        sendButton.setText("Done!");
                        responseLabel.setVisible(true);
                        locationLabel.setText(location);
                        locationLabel.setVisible(true);
                        responseLabel_1.setVisible(true);
                        return Status.OK_STATUS;
                    }
                };
                inner.setPriority(Job.INTERACTIVE);
                inner.setSystem(true);
                inner.schedule();

                return Status.OK_STATUS;
            }
        };
        j.setPriority(Job.SHORT);
        j.setSystem(true);
        j.schedule();
    }

    private static void fetchErlangSystemInfo() {
        final IBackend ideBackend = BackendCore.getBackendManager()
                .getIdeBackend();
        final String info = BackendHelper.getSystemInfo(ideBackend);
        ErlLogger.info("\n++++++++++++++++++++++\n" + info);
    }

    void sendToDisk(final String location, final ProblemData data) {
        final File report = new File(location);
        try {
            report.createNewFile();
            final OutputStream out = new FileOutputStream(report);
            final PrintWriter pw = new PrintWriter(out);
            try {
                pw.println(data.summary);
                pw.println(data.reporter);
                pw.println(data.description);
                pw.println("\n==================================\n");
                pw.println(data.platformLog);
                pw.println("\n==================================\n");
                pw.println(data.erlideLog);
            } finally {
                pw.flush();
                pw.close();
                out.close();
            }
        } catch (final IOException e) {
            ErlLogger.warn(e);
        }
    }

    @Override
    public void init(final IWorkbench workbench) {
    }

    public static ProblemData gatherProblemData(final boolean attach,
            final String title, final String contact, final String body) {
        fetchErlangSystemInfo();

        String plog = "N/A";
        String elog = "N/A";
        if (attach) {
            plog = LogUtil.fetchPlatformLog();
            elog = LogUtil.fetchErlideLog();
        }
        final ProblemData data = new ProblemData(title, contact, body, plog,
                elog);
        return data;
    }
}
