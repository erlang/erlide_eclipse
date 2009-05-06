/*******************************************************************************
 * Copyright (c) 2000, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Stephan Michels, stephan@apache.org - 104944 [JUnit] Unnecessary code in JUnitProgressBar
 *******************************************************************************/
package org.erlide.gunit.internal.ui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;

/**
 * A progress bar with a red/green indication for success or failure.
 */
public class GUnitProgressBar extends Canvas {
	private static final int DEFAULT_WIDTH = 160;

	private static final int DEFAULT_HEIGHT = 18;

	private int fCurrentTickCount = 0;

	private int fMaxTickCount = 0;

	private int fColorBarWidth = 0;

	private final Color fOKColor;

	private final Color fFailureColor;

	private final Color fStoppedColor;

	private boolean fError;

	private boolean fStopped = false;

	public GUnitProgressBar(final Composite parent) {
		super(parent, SWT.NONE);

		addControlListener(new ControlAdapter() {
			@Override
			public void controlResized(final ControlEvent e) {
				GUnitProgressBar.this.fColorBarWidth = scale(GUnitProgressBar.this.fCurrentTickCount);
				redraw();
			}
		});
		addPaintListener(new PaintListener() {
			public void paintControl(final PaintEvent e) {
				paint(e);
			}
		});
		addDisposeListener(new DisposeListener() {
			public void widgetDisposed(final DisposeEvent e) {
				GUnitProgressBar.this.fFailureColor.dispose();
				GUnitProgressBar.this.fOKColor.dispose();
				GUnitProgressBar.this.fStoppedColor.dispose();
			}
		});
		final Display display = parent.getDisplay();
		this.fFailureColor = new Color(display, 159, 63, 63);
		this.fOKColor = new Color(display, 95, 191, 95);
		this.fStoppedColor = new Color(display, 120, 120, 120);
	}

	public void setMaximum(final int max) {
		this.fMaxTickCount = max;
	}

	public void reset() {
		this.fError = false;
		this.fStopped = false;
		this.fCurrentTickCount = 0;
		this.fMaxTickCount = 0;
		this.fColorBarWidth = 0;
		redraw();
	}

	public void reset(final boolean hasErrors, final boolean stopped, final int ticksDone,
			final int maximum) {
		final boolean noChange = this.fError == hasErrors && this.fStopped == stopped
		&& this.fCurrentTickCount == ticksDone
		&& this.fMaxTickCount == maximum;
		this.fError = hasErrors;
		this.fStopped = stopped;
		this.fCurrentTickCount = ticksDone;
		this.fMaxTickCount = maximum;
		this.fColorBarWidth = scale(ticksDone);
		if (!noChange) {
			redraw();
		}
	}

	private void paintStep(int startX, final int endX) {
		final GC gc = new GC(this);
		setStatusColor(gc);
		final Rectangle rect = getClientArea();
		startX = Math.max(1, startX);
		gc.fillRectangle(startX, 1, endX - startX, rect.height - 2);
		gc.dispose();
	}

	private void setStatusColor(final GC gc) {
		if (this.fStopped) {
			gc.setBackground(this.fStoppedColor);
		} else if (this.fError) {
			gc.setBackground(this.fFailureColor);
		} else {
			gc.setBackground(this.fOKColor);
		}
	}

	public void stopped() {
		this.fStopped = true;
		redraw();
	}

	private int scale(final int value) {
		if (this.fMaxTickCount > 0) {
			final Rectangle r = getClientArea();
			if (r.width != 0) {
				return Math.max(0, value * (r.width - 2) / this.fMaxTickCount);
			}
		}
		return value;
	}

	private void drawBevelRect(final GC gc, final int x, final int y, final int w, final int h,
			final Color topleft, final Color bottomright) {
		gc.setForeground(topleft);
		gc.drawLine(x, y, x + w - 1, y);
		gc.drawLine(x, y, x, y + h - 1);

		gc.setForeground(bottomright);
		gc.drawLine(x + w, y, x + w, y + h);
		gc.drawLine(x, y + h, x + w, y + h);
	}

	private void paint(final PaintEvent event) {
		final GC gc = event.gc;
		final Display disp = getDisplay();

		final Rectangle rect = getClientArea();
		gc.fillRectangle(rect);
		drawBevelRect(gc, rect.x, rect.y, rect.width - 1, rect.height - 1, disp
				.getSystemColor(SWT.COLOR_WIDGET_NORMAL_SHADOW), disp
				.getSystemColor(SWT.COLOR_WIDGET_HIGHLIGHT_SHADOW));

		setStatusColor(gc);
		this.fColorBarWidth = Math.min(rect.width - 2, this.fColorBarWidth);
		gc.fillRectangle(1, 1, this.fColorBarWidth, rect.height - 2);
	}

	@Override
	public Point computeSize(final int wHint, final int hHint, final boolean changed) {
		checkWidget();
		final Point size = new Point(DEFAULT_WIDTH, DEFAULT_HEIGHT);
		if (wHint != SWT.DEFAULT) {
			size.x = wHint;
		}
		if (hHint != SWT.DEFAULT) {
			size.y = hHint;
		}
		return size;
	}

	public void step(final int failures) {
		this.fCurrentTickCount++;
		int x = this.fColorBarWidth;

		this.fColorBarWidth = scale(this.fCurrentTickCount);

		if (!this.fError && failures > 0) {
			this.fError = true;
			x = 1;
		}
		if (this.fCurrentTickCount == this.fMaxTickCount) {
			this.fColorBarWidth = getClientArea().width - 1;
		}
		paintStep(x, this.fColorBarWidth);
	}

	public void refresh(final boolean hasErrors) {
		this.fError = hasErrors;
		redraw();
	}

}
