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

import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

public class ReportPreferencePage extends PreferencePage implements
		IWorkbenchPreferencePage {

	private Text contact;
	private Text description;
	private Text title;

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
		this.description.setBounds(46, 31, 416, 86);

		this.contact = new Text(panel, SWT.BORDER);
		this.contact.setBounds(152, 123, 310, 25);

		final Button attachTechnicalDataButton = new Button(panel, SWT.CHECK);
		attachTechnicalDataButton.setText("Attach technical data");
		attachTechnicalDataButton.setBounds(46, 154, 135, 20);

		final Button attachFilesButton = new Button(panel, SWT.NONE);
		attachFilesButton.setText("Attach files");
		attachFilesButton.setBounds(56, 176, 75, 25);

		final Button viewAttachmentsButton = new Button(panel, SWT.NONE);
		viewAttachmentsButton.setText("View attachments");
		viewAttachmentsButton.setBounds(142, 176, 110, 25);

		final Button sendButton = new Button(panel, SWT.NONE);
		sendButton.setText("Send!");
		sendButton.setBounds(414, 176, 45, 25);

		final Label contactoptionalLabel = new Label(panel, SWT.NONE);
		contactoptionalLabel.setText("Contact (optional)");
		contactoptionalLabel.setBounds(46, 126, 100, 15);
		noDefaultAndApplyButton();

		return panel;
	}

	public void init(IWorkbench workbench) {
	}

}
