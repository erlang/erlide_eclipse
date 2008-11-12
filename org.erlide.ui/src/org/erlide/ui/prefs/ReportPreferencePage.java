/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.prefs;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.erlide.ui.prefs.tickets.AssemblaHandler;
import org.erlide.ui.prefs.tickets.TicketInfo;

public class ReportPreferencePage extends PreferencePage implements
		IWorkbenchPreferencePage {

	private Button sendButton;
	private Label responseLabel_1;
	private Text locationLabel;
	Text fcontact;
	Text fbody;
	Text ftitle;

	private Label responseLabel;
	Button attachTechnicalDataButton;

	@Override
	protected Control createContents(Composite parent) {
		Composite panel = new Composite(parent, SWT.NONE);

		final Label titleLabel = new Label(panel, SWT.NONE);
		titleLabel.setText("Title");
		titleLabel.setBounds(0, 5, 25, 15);

		this.ftitle = new Text(panel, SWT.BORDER);
		this.ftitle.setBounds(46, 0, 416, 25);

		this.fbody = new Text(panel, SWT.V_SCROLL | SWT.MULTI | SWT.BORDER
				| SWT.WRAP);
		this.fbody.setBounds(46, 31, 416, 188);

		this.fbody
				.setText("(enter error description here, paste any relevant code too)");

		this.fcontact = new Text(panel, SWT.BORDER);
		fcontact.setText(System.getProperty("user.name"));
		this.fcontact.setBounds(152, 225, 310, 25);

		attachTechnicalDataButton = new Button(panel, SWT.CHECK);
		attachTechnicalDataButton.setSelection(true);
		attachTechnicalDataButton
				.setText("Attach technical data (eclipse and erlide logs)");
		attachTechnicalDataButton.setBounds(46, 256, 260, 20);

		sendButton = new Button(panel, SWT.NONE);
		sendButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				postReport();
			}
		});
		sendButton.setText("Send!");
		sendButton.setBounds(217, 284, 45, 25);

		final Label contactoptionalLabel = new Label(panel, SWT.NONE);
		contactoptionalLabel.setText("Contact email (optional)");
		contactoptionalLabel.setBounds(46, 228, 100, 15);

		responseLabel = new Label(panel, SWT.CENTER);
		responseLabel.setVisible(false);
		responseLabel
				.setText("The report was saved in the location below, you can now close this window.");
		responseLabel.setBounds(47, 315, 415, 15);

		locationLabel = new Text(panel, SWT.CENTER | SWT.WRAP | SWT.READ_ONLY);
		locationLabel.setBackground(Display.getCurrent().getSystemColor(
				SWT.COLOR_WIDGET_BACKGROUND));
		locationLabel.setText("location");
		locationLabel.setBounds(46, 336, 415, 23);
		locationLabel.setVisible(false);

		responseLabel_1 = new Label(panel, SWT.CENTER);
		responseLabel_1.setBounds(47, 364, 415, 15);
		responseLabel_1.setVisible(false);
		responseLabel_1
				.setText("Please attach the report if you are writing a trouble ticket.");

		noDefaultAndApplyButton();

		return panel;
	}

	protected void postReport() {
		final String location = getLocation();
		final boolean attach = attachTechnicalDataButton.getSelection();
		final String title = ftitle.getText();
		final String contact = fcontact.getText();
		final String body = fbody.getText();

		Job j = new Job("send error report") {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				String plog = "N/A";
				String elog = "N/A";
				if (attach) {
					plog = fetchPlatformLog();
					elog = fetchErlideLog();
				}
				TicketInfo data = new TicketInfo(title, contact, body, plog,
						elog);
				sendToDisk(location, data);
				new AssemblaHandler().send(data);
				return Status.OK_STATUS;
			}
		};
		j.setPriority(Job.SHORT);
		j.setSystem(true);
		j.schedule();

		responseLabel.setVisible(true);
		locationLabel.setText(location);
		locationLabel.setVisible(true);
		responseLabel_1.setVisible(true);
		sendButton.setEnabled(false);
	}

	String getLocation() {
		String s;
		if (System.getProperty("os.name").toLowerCase().contains("windows")) {
			s = "\\\\projhost\\tecsas\\shade\\erlide\\reports";
		} else {
			s = "/proj/tecsas/SHADE/erlide/reports";
		}
		File dir = new File(s);
		if (!dir.exists()) {
			s = System.getProperty("user.home");
		}
		String tstamp = new SimpleDateFormat("yyyyMMdd_HHmmss")
				.format(new Date());
		return s + "/" + System.getProperty("user.name") + "_" + tstamp
				+ ".txt";
	}

	void sendToDisk(String location, TicketInfo data) {
		File report = new File(location);
		try {
			report.createNewFile();
			OutputStream out = new FileOutputStream(report);
			PrintWriter pw = new PrintWriter(out);
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
				out.close();
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	public void init(IWorkbench workbench) {
	}

	String fetchPlatformLog() {
		List<String> result = new ArrayList<String>();
		File log = Platform.getLogFileLocation().toFile();
		int i = 0;
		int last = 0;
		try {
			BufferedReader reader = new BufferedReader(new InputStreamReader(
					new FileInputStream(log), "UTF-8"));
			for (;;) {
				String line = reader.readLine();
				if (line == null) {
					break;
				}
				line = line.trim();
				if (line.length() == 0) {
					continue;
				}
				result.add(line);
				if (line.startsWith("!SESSION ")) {
					last = i;
				}
				i++;
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		for (int j = 0; j < last; j++) {
			result.remove(0);
		}
		StringBuffer buf = new StringBuffer();
		for (String s : result) {
			buf.append(s).append("\n");
		}
		return buf.toString();
	}

	String fetchErlideLog() {
		StringBuffer result = new StringBuffer();
		String dir = ResourcesPlugin.getWorkspace().getRoot().getLocation()
				.toPortableString();
		dir = (dir == null) ? "c:/" : dir;
		File log = new File(dir + "_erlide.log");

		try {
			BufferedReader reader = new BufferedReader(new InputStreamReader(
					new FileInputStream(log), "UTF-8"));
			for (;;) {
				String line = reader.readLine();
				if (line == null) {
					break;
				}
				line = line.trim();
				if (line.length() == 0) {
					continue;
				}
				result.append(line).append('\n');
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return result.toString();
	}

}
