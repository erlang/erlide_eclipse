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
import java.io.UnsupportedEncodingException;
import java.net.InetSocketAddress;
import java.net.MalformedURLException;
import java.net.Proxy;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;
import java.text.SimpleDateFormat;
import java.util.Date;

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
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

public class ReportPreferencePage extends PreferencePage implements
		IWorkbenchPreferencePage {

	private Label locationLabel;
	Text fcontact;
	Text fbody;
	Text ftitle;

	class ReportData {
		String title;
		String contact;
		String body;
		String plog;
		String elog;

		public ReportData(String title, String body, String contact,
				boolean attach) {
			this.title = title;
			this.contact = contact;
			this.body = body;
			if (attach) {
				this.plog = fetchPlatformLog();
				this.elog = fetchErlideLog();
			}
		}
	}

	final private String URL = "https://shibumi.fogbugz.com/ScoutSubmit.asp?"
			+ "ScoutUserName=field_tester&ScoutProject=erlide&ScoutArea=Misc&"
			+ "Extra=%s&Description=%s&Email=%s";
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

		final Button sendButton = new Button(panel, SWT.NONE);
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
		responseLabel.setText("The report was saved in the location below,"
				+ " you can close this window.");
		responseLabel.setBounds(47, 315, 415, 15);

		locationLabel = new Label(panel, SWT.CENTER | SWT.WRAP);
		locationLabel.setText("location");
		locationLabel.setBounds(46, 336, 415, 38);
		locationLabel.setVisible(false);

		noDefaultAndApplyButton();

		return panel;
	}

	protected void postReport() {
		final String location = getLocation();
		Job j = new Job("send error report") {
			ReportData data = new ReportData(ftitle.getText(), fcontact
					.getText(), fbody.getText(), attachTechnicalDataButton
					.getSelection());

			@Override
			protected IStatus run(IProgressMonitor monitor) {
				sendToDisk(location, data);
				return Status.OK_STATUS;
			}
		};
		j.setPriority(Job.SHORT);
		j.setSystem(true);
		j.schedule();

		responseLabel.setVisible(true);
		locationLabel.setText(location);
		locationLabel.setVisible(true);
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

	void sendToDisk(String location, ReportData data) {
		File report = new File(location);
		try {
			report.createNewFile();
			OutputStream out = new FileOutputStream(report);
			PrintWriter pw = new PrintWriter(out);
			try {
				pw.println(data.title);
				pw.println(data.contact);
				pw.println(data.body);
				pw.println("\n==================================\n");
				pw.println(data.plog);
				pw.println("\n==================================\n");
				pw.println(data.elog);
			} finally {
				pw.flush();
				out.close();
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	@SuppressWarnings("unused")
	private void sendToFogBugz() {
		String extra;
		String descr;
		String email;
		try {
			extra = URLEncoder.encode(fbody.getText(), "UTF-8");
			descr = URLEncoder.encode(ftitle.getText(), "UTF-8");
			email = URLEncoder.encode(fcontact.getText(), "UTF-8");
		} catch (UnsupportedEncodingException e1) {
			e1.printStackTrace();
			extra = "";
			descr = "";
			email = "";
		}
		final String fe = extra;
		final String fd = descr;
		final String fm = email;

		Job j = new Job("send error report") {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				try {
					Proxy proxy = Proxy.NO_PROXY;
					if ("true".equals(System.getProperty("proxySet"))) {
						proxy = new Proxy(Proxy.Type.HTTP,
								new InetSocketAddress(System
										.getProperty("proxyHost"), Integer
										.parseInt(System
												.getProperty("proxyPort"))));
					}
					URLConnection c = new URL(String.format(URL, fe, fd, fm))
							.openConnection(proxy);

					// TODO use POST!

					BufferedReader dis = new BufferedReader(
							new InputStreamReader(c.getInputStream()));
					String inputLine;

					while ((inputLine = dis.readLine()) != null) {
						System.out.println(inputLine);
					}
					dis.close();
				} catch (MalformedURLException e) {
					e.printStackTrace();
				} catch (IOException e) {
					e.printStackTrace();
				}
				return Status.OK_STATUS;
			}
		};
		j.setPriority(Job.SHORT);
		j.setSystem(true);
		// j.schedule();
	}

	public void init(IWorkbench workbench) {
	}

	String fetchPlatformLog() {
		StringBuffer result = new StringBuffer();
		File log = Platform.getLogFileLocation().toFile();
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
