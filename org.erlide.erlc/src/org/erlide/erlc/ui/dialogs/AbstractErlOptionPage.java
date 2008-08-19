/*******************************************************************************
 * Copyright (c) 2003, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     QNX Software Systems - Initial API and implementation
 *******************************************************************************/
package org.erlide.erlc.ui.dialogs;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.DialogPage;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.erlide.ui.util.SWTUtil;

public abstract class AbstractErlOptionPage extends DialogPage implements
		IErlOptionPage {

	private boolean bIsValid = true;

	private IErlOptionContainer fContainer;

	protected Button createPushButton(final Composite parent,
			final String label, final Image image) {
		final Button button = new Button(parent, SWT.PUSH);
		button.setFont(parent.getFont());
		if (image != null) {
			button.setImage(image);
		}
		if (label != null) {
			button.setText(label);
		}
		final GridData gd = new GridData();
		button.setLayoutData(gd);
		SWTUtil.setButtonDimensionHint(button);
		return button;
	}

	protected Button createRadioButton(final Composite parent,
			final String label) {
		final Button button = new Button(parent, SWT.RADIO);
		button.setFont(parent.getFont());
		if (label != null) {
			button.setText(label);
		}
		final GridData gd = new GridData();
		button.setLayoutData(gd);
		SWTUtil.setButtonDimensionHint(button);
		return button;
	}

	protected AbstractErlOptionPage() {
		super();
	}

	protected AbstractErlOptionPage(final String title) {
		super(title);
	}

	protected AbstractErlOptionPage(final String title,
			final ImageDescriptor image) {
		super(title, image);
	}

	public void setContainer(final IErlOptionContainer container) {
		fContainer = container;
	}

	protected IErlOptionContainer getContainer() {
		return fContainer;
	}

	protected void setValid(final boolean isValid) {
		bIsValid = isValid;
	}

	public boolean isValid() {
		return bIsValid;
	}

	public abstract void performApply(IProgressMonitor monitor)
			throws CoreException;

	public abstract void performDefaults();

	public abstract void createControl(Composite parent);

}
