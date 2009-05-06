/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.internal.util;

import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CLabel;
import org.eclipse.swt.custom.ViewForm;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.ToolBar;

/**
 * A <code>ViewerPane</code> is a convenience class which installs a
 * <code>CLabel</code> and a <code>Toolbar</code> in a <code>ViewForm</code>.
 * <P>
 */
public class ViewerPane extends ViewForm {

	private final ToolBarManager fToolBarManager;

	public ViewerPane(final Composite parent, final int style) {
		super(parent, style);

		marginWidth = 0;
		marginHeight = 0;

		final CLabel label = new CLabel(this, SWT.NONE);
		setTopLeft(label);

		final ToolBar tb = new ToolBar(this, SWT.FLAT);
		setTopCenter(tb);
		fToolBarManager = new ToolBarManager(tb);
	}

	/**
	 * Sets the receiver's title text.
	 */
	public void setText(final String label) {
		final CLabel cl = (CLabel) getTopLeft();
		cl.setText(label);
	}

	public String getText() {
		final CLabel cl = (CLabel) getTopLeft();
		return cl.getText();
	}

	/**
	 * Sets the receiver's title image.
	 */
	public void setImage(final Image image) {
		final CLabel cl = (CLabel) getTopLeft();
		cl.setImage(image);
	}

	public Image getImage() {
		final CLabel cl = (CLabel) getTopLeft();
		return cl.getImage();
	}

	public ToolBarManager getToolBarManager() {
		return fToolBarManager;
	}
}
