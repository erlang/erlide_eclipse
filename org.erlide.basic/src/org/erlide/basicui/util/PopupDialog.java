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
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.internal.Workbench;
import org.eclipse.ui.progress.UIJob;
import org.erlide.basicui.ErlideBasicUIPlugin;

import com.novocode.naf.swt.custom.BalloonWindow;

public class PopupDialog {

	private PopupDialog() {
	}

	static BalloonWindow win;
	
	public static void show(final String title, final String message,
			final int delay, final int anchor, final boolean centered) {
//		if (Workbench.getInstance()==null)
//			return;
		final IWorkbench workbench = PlatformUI.getWorkbench();
		IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();
		final Display display = window == null ? workbench.getDisplay()
				: window.getShell().getDisplay();
//		final Display display = PlatformUI.createDisplay();
		display.syncExec(new Runnable() {
			public void run() {
				win = new BalloonWindow(display, SWT.ON_TOP
						| SWT.TITLE | SWT.TOOL);
				win.setText(title);

				final Layout layout = new FillLayout();
				final Composite ct = win.getContents();
				ct.setLayout(layout);

				final Label l = new Label(ct, SWT.LEFT | SWT.WRAP);
				l.setBackground(ct.getBackground());
				l.setForeground(ct.getForeground());
				l.setText(message);
				// l.setBounds(0, 0, 60, 60);
				l.setBounds(ct.getClientArea());
				l.pack();
				ct.pack();
				win.addSelectionControl(l);

				final Rectangle r = display.getBounds();
				Rectangle rr = win.getContents().getBounds();
				if (centered) {
					win.setLocation(r.x + r.width / 2 - rr.width / 2, r.y
							+ r.height / 2 - rr.height / 2);
				} else {
					win.setLocation(r.x + r.width - 10, r.y + r.height - 10);
				}
				win.setAnchor(anchor);
				win.setVisible(true);

			}
		});
		final UIJob job = new UIJob("close balloon") {

			@Override
			public IStatus runInUIThread(IProgressMonitor monitor) {
				 if (!win.getContents().isDisposed())
					 win.getContents().getShell().close();
				// display.dispose();

				return new Status(IStatus.OK,
						ErlideBasicUIPlugin.PLUGIN_ID, IStatus.OK, "",
						null);
			}
		};
		job.schedule(delay);

	}

	public static void showBalloon(String title, String message, int delay) {
		show(title, message, delay, SWT.RIGHT | SWT.BOTTOM, false);
	}

	public static void showDialog(String title, String message, int delay) {
		show(title, message, delay, SWT.NONE, true);
	}

}
