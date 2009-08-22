/*******************************************************************************
 * Copyright (c) 2009 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.views.console;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.console.AbstractConsole;
import org.eclipse.ui.console.IConsoleConstants;
import org.eclipse.ui.console.IConsoleView;
import org.eclipse.ui.part.IPageBookViewPage;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendShell;
import org.erlide.runtime.backend.ErlideBackend;

public class ErlangConsole extends AbstractConsole {

	private Font fFont;
	private Color fBackground;
	private final BackendShell shell;

	public ErlangConsole(ErlideBackend backend) {
		super(backend.getName(), null);
		shell = backend.getShell("main");
	}

	public IPageBookViewPage createPage(IConsoleView view) {
		return new ErlangConsolePage(view, this);
	}

	public Backend getBackend() {
		return shell.getBackend();
	}

	public BackendShell getShell() {
		return shell;
	}

	@Override
	public ImageDescriptor getImageDescriptor() {
		return null;
	}

	@Override
	public String getName() {
		return "Erlang: " + shell.getBackend().getInfo().toString() + " "
				+ shell.hashCode();
	}

	@Override
	public String getType() {
		return null;
	}

	@Override
	public void addPropertyChangeListener(IPropertyChangeListener listener) {
	}

	@Override
	public void removePropertyChangeListener(IPropertyChangeListener listener) {
	}

	public void show() {
		IWorkbenchPage page = PlatformUI.getWorkbench()
				.getActiveWorkbenchWindow().getActivePage();
		String id = IConsoleConstants.ID_CONSOLE_VIEW;
		IConsoleView view;
		try {
			view = (IConsoleView) page.showView(id);
			view.display(this);
		} catch (PartInitException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/**
	 * Returns the font used by this console. Must be called in the UI thread.
	 * 
	 * @return font used by this console
	 */
	public Font getFont() {
		if (fFont == null) {
			fFont = getDefaultFont();
		}
		return fFont;
	}

	/**
	 * Sets the font used by this console. Specify <code>null</code> to use the
	 * default text font.
	 * 
	 * @param newFont
	 *            font, or <code>null</code> to indicate the default font
	 */
	public void setFont(Font newFont) {
		// ensure font is initialized
		getFont();
		// translate null to default font
		if (newFont == null) {
			newFont = getDefaultFont();
		}
		// fire property change if required
		if (!fFont.equals(newFont)) {
			Font old = fFont;
			fFont = newFont;
			firePropertyChange(this, IConsoleConstants.P_FONT, old, fFont);
		}
	}

	/**
	 * Returns the default text font.
	 * 
	 * @return the default text font
	 */
	private Font getDefaultFont() {
		return JFaceResources.getFont(JFaceResources.TEXT_FONT);
	}

	/**
	 * Sets the background color used by this console. Specify <code>null</code>
	 * to use the default background color.
	 * 
	 * @param background
	 *            background color or <code>null</code> for default
	 * @since 3.3
	 */
	public void setBackground(Color background) {
		if (fBackground == null) {
			if (background == null) {
				return;
			}
		} else if (fBackground.equals(background)) {
			return;
		}
		Color old = fBackground;
		fBackground = background;
		firePropertyChange(this, IConsoleConstants.P_BACKGROUND_COLOR, old,
				fBackground);
	}

	/**
	 * Returns the background color to use for this console or <code>null</code>
	 * for the default background color.
	 * 
	 * @return background color or <code>null</code> for default
	 * @since 3.3
	 */
	public Color getBackground() {
		return fBackground;
	}

}
