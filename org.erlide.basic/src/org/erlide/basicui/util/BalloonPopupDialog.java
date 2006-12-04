/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.basicui.util;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.progress.UIJob;
import org.erlide.basicui.ErlideBasicUIPlugin;

import com.novocode.naf.swt.custom.BalloonWindow;

public class BalloonPopupDialog {

	private BalloonPopupDialog() {
	}

	public static void show(String title, String message, int delay) {
		final IWorkbench workbench = PlatformUI.getWorkbench();
		final IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();
		final Shell shell = window.getShell();

		final Display display = shell.getDisplay();
		final BalloonWindow win = new BalloonWindow(display, SWT.ON_TOP
				| SWT.TITLE | SWT.TOOL);
		win.setText(title);

		final Layout layout = new FillLayout();
		win.getContents().setLayout(layout);

		final Composite ct = win.getContents();
		final Label l = new Label(ct, SWT.LEFT | SWT.WRAP);
		l.setBackground(ct.getBackground());
		l.setForeground(ct.getForeground());
		l.setText(message);
		// l.setBounds(0, 0, 60, 60);
		l.setBounds(ct.getClientArea());
		l.pack();
		ct.pack();
		win.addSelectionControl(l);

		final Rectangle r = shell.getBounds();
		win.setLocation(r.x + r.width - 10, r.y + r.height - 10);
		win.setAnchor(SWT.RIGHT | SWT.BOTTOM);
		win.setVisible(true);
		final BalloonWindow mywin = win;

		final UIJob job = new UIJob("close balloon") {

			@Override
			public IStatus runInUIThread(IProgressMonitor monitor) {
				mywin.close();

				return new Status(IStatus.OK, ErlideBasicUIPlugin.PLUGIN_ID,
						IStatus.OK, "", null); //$NON-NLS-1$
			}
		};
		job.schedule(delay);
	}

}
