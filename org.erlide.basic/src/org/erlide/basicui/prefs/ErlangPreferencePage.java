/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.basicui.prefs;

import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.resource.DeviceResourceException;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.program.Program;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.erlide.basicui.ErlideBasicUIPlugin;
import org.erlide.basicui.IErlideBasicUIConstants;
import org.erlide.basicui.util.SFProjectSupport;

public class ErlangPreferencePage extends PreferencePage implements
		IWorkbenchPreferencePage {

	@Override
	protected Control createContents(Composite parent) {
		noDefaultAndApplyButton();
		final Composite panel = new Composite(parent, SWT.NONE);
		final GridLayout layout = new GridLayout();
		layout.numColumns = 2;
		panel.setLayout(layout);

		final Label img = new Label(panel, SWT.NONE);
		try {
			final ImageDescriptor d = ErlideBasicUIPlugin
					.getDefault()
					.getImageDescriptor(IErlideBasicUIConstants.IMG_ERLANG_LOGO);
			if (d != null) {
				img.setImage((Image) d.createResource(parent.getDisplay()));
			} else {
				img.setText("Erlang");
			}
		} catch (final DeviceResourceException e) {
			img.setText("<Erlang>");
		}

		Label text = new Label(panel, SWT.NONE);
		text.setText("Erlang IDE plugin, by the Erlide team (c) 2004,2005");

		final SelectionListener linkListener = new SelectionListener() {

			public void widgetSelected(SelectionEvent e) {
				Program.launch(e.text);
			}

			public void widgetDefaultSelected(SelectionEvent e) {
			}
		};

		final Composite panel2 = new Composite(panel, SWT.NONE);
		final GridLayout layout2 = new GridLayout();
		layout2.numColumns = 1;
		panel2.setLayout(layout2);

		text = new Label(panel2, SWT.NONE);
		text.setText("Here you can request help from the Erlide team:");

		Link l;

		l = new Link(panel2, SWT.NONE | SWT.WRAP);
		l.setSize(panel2.getSize().x, 20);
		l.setText("   Visit the project's <a href=\""
				+ SFProjectSupport.HOME_URL + "\">home page</a>.");
		l.addSelectionListener(linkListener);

		l = new Link(panel2, SWT.NONE | SWT.WRAP);
		l.setSize(panel2.getSize().x, 20);
		l.setText("   Post a <a href=\"" + SFProjectSupport.BUGS_URL
				+ "\">bug report</a>.");
		l.addSelectionListener(linkListener);

		l = new Link(panel2, SWT.NONE);
		l.setText("   Post a <a href=\"" + SFProjectSupport.SUPPORT_URL
				+ "\">support request</a>.");
		l.addSelectionListener(linkListener);

		l = new Link(panel2, SWT.NONE);
		l.setText("   Post a <a href=\"" + SFProjectSupport.FEATURES_URL
				+ "\">feature request</a>.");
		l.addSelectionListener(linkListener);

		return panel;
	}

	public void init(IWorkbench workbench) {
	}

}
