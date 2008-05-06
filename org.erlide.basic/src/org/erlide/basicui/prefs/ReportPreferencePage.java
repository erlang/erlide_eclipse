/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.basicui.prefs;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.InetSocketAddress;
import java.net.MalformedURLException;
import java.net.Proxy;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;

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

	Text contact;
	Text description;
	Text title;

	final private String URL = "https://shibumi.fogbugz.com/ScoutSubmit.asp?"
			+ "ScoutUserName=field_tester&ScoutProject=erlide&ScoutArea=Misc&"
			+ "Extra=%s&Description=%s&Email=%s";
	private Label responseLabel;

	@Override
	protected Control createContents(Composite parent) {
		Composite panel = new Composite(parent, SWT.NONE);
		panel.setEnabled(false);

		final Label titleLabel = new Label(panel, SWT.NONE);
		titleLabel.setText("Title");
		titleLabel.setBounds(0, 5, 25, 15);

		this.title = new Text(panel, SWT.BORDER);
		this.title.setBounds(46, 0, 416, 25);

		this.description = new Text(panel, SWT.V_SCROLL | SWT.MULTI
				| SWT.BORDER | SWT.WRAP);
		this.description.setBounds(46, 31, 416, 188);

		String plog = fetchPlatformLog();
		String elog = fetchErlideLog();
		this.description.setText("(enter error description here)\n"
				+ "\n\n------------------------------\n" + plog
				+ "\n\n------------------------------\n" + elog);

		this.contact = new Text(panel, SWT.BORDER);
		this.contact.setBounds(152, 225, 310, 25);

		final Button attachTechnicalDataButton = new Button(panel, SWT.CHECK);
		attachTechnicalDataButton.setText("Attach technical data");
		attachTechnicalDataButton.setBounds(46, 256, 135, 20);

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
		contactoptionalLabel.setText("Contact (optional)");
		contactoptionalLabel.setBounds(46, 228, 100, 15);

		responseLabel = new Label(panel, SWT.CENTER);
		responseLabel.setVisible(false);
		responseLabel.setText("The report is being sent now, you can close this window.");
		responseLabel.setBounds(47, 315, 415, 20);
		noDefaultAndApplyButton();

		return panel;
	}

	protected void postReport() {
		String extra;
		String descr;
		String email;
		try {
			extra = URLEncoder.encode(description.getText(),
					"UTF-8");
			descr = URLEncoder.encode(title.getText(), "UTF-8");
			email = URLEncoder
					.encode(contact.getText(), "UTF-8");
		} catch (UnsupportedEncodingException e1) {
			e1.printStackTrace();
			extra="";
			descr="";
			email="";
		}
		final String fe=extra;
		final String fd=descr;
		final String fm=email;

		Job j = new Job("send error report") {;
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				try {
					Proxy proxy = Proxy.NO_PROXY;
						if("true".equals(System.getProperty("proxySet"))){
							proxy = new Proxy(Proxy.Type.HTTP, 
									new InetSocketAddress(System.getProperty("proxyHost"),
											Integer.parseInt(System.getProperty("proxyPort"))));
						}
					URLConnection c = new URL(String.format(URL, fe, fd, fm)).openConnection(proxy);
					
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
		responseLabel.setVisible(true);
	}

	public void init(IWorkbench workbench) {
	}

	private String fetchPlatformLog() {
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

	private String fetchErlideLog() {
		StringBuffer result = new StringBuffer();
		String dir = ResourcesPlugin.getWorkspace().getRoot().getLocation()
				.toPortableString();
		dir = dir == null ? "c:/" : dir;
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
