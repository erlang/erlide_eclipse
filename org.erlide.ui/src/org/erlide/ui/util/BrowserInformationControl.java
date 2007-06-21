/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

package org.erlide.ui.util;

import org.eclipse.core.runtime.ListenerList;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlExtension;
import org.eclipse.jface.text.IInformationControlExtension3;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTError;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;

/**
 * Displays textual information in a {@link org.eclipse.swt.browser.Browser}
 * widget.
 * 
 * <p>
 * This class may be instantiated; it is not intended to be subclassed.
 * </p>
 * <p>
 * Current problems: - setting a status field text automatically closes the
 * hover - the size computation is too small - focusLost event is not sent (see
 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=84532)
 * </p>
 * 
 * @since 3.1
 */
public class BrowserInformationControl implements IInformationControl,
		IInformationControlExtension, IInformationControlExtension3,
		DisposeListener {

	/** Right margin in pixels. */
	private static final int RIGHT_MARGIN = 3;

	/**
	 * Layout used to achieve the "tool tip" look, i.e., flat with a thin
	 * boarder.
	 */
	private static class BorderFillLayout extends Layout {

		/** The border widths. */
		final int fBorderSize;

		/**
		 * Creates a fill layout with a border.
		 * 
		 * @param borderSize
		 *            the size of the border
		 */
		public BorderFillLayout(int borderSize) {
			if (borderSize < 0) {
				throw new IllegalArgumentException();
			}
			fBorderSize = borderSize;
		}

		/**
		 * Returns the border size.
		 * 
		 * @return the border size
		 */
		public int getBorderSize() {
			return fBorderSize;
		}

		/*
		 * @see org.eclipse.swt.widgets.Layout#computeSize(org.eclipse.swt.widgets.Composite,
		 *      int, int, boolean)
		 */
		@Override
		protected Point computeSize(Composite composite, int wHint, int hHint,
				boolean flushCache) {

			final Control[] children = composite.getChildren();
			final Point minSize = new Point(0, 0);

			if (children != null) {
				for (Control element : children) {
					final Point size = element.computeSize(wHint, hHint,
							flushCache);
					minSize.x = Math.max(minSize.x, size.x);
					minSize.y = Math.max(minSize.y, size.y);
				}
			}

			minSize.x += fBorderSize * 2 + RIGHT_MARGIN;
			minSize.y += fBorderSize * 2;

			return minSize;
		}

		/*
		 * @see org.eclipse.swt.widgets.Layout#layout(org.eclipse.swt.widgets.Composite,
		 *      boolean)
		 */
		@Override
		protected void layout(Composite composite, boolean flushCache) {

			final Control[] children = composite.getChildren();
			final Point minSize = new Point(composite.getClientArea().width,
					composite.getClientArea().height);

			if (children != null) {
				for (final Control child : children) {
					child.setSize(minSize.x - fBorderSize * 2, minSize.y -
							fBorderSize * 2);
					child.setLocation(fBorderSize, fBorderSize);
				}
			}
		}
	}

	/**
	 * Tells whether the SWT Browser widget and hence this information control
	 * is available.
	 * 
	 * @param parent
	 *            the parent component used for checking
	 * @return <code>true</code> if this control is available
	 */
	public static boolean isAvailable(Composite parent) {
		if (!fgAvailabilityChecked) {
			try {
				final Browser browser = new Browser(parent, SWT.NONE);
				browser.dispose();
				fgIsAvailable = true;
			} catch (final SWTError er) {
				fgIsAvailable = false;
			} finally {
				fgAvailabilityChecked = true;
			}
		}

		return fgIsAvailable;
	}

	/** Border thickness in pixels. */
	private static final int BORDER = 1;

	/**
	 * Availability checking cache.
	 */
	private static boolean fgIsAvailable = false;

	private static boolean fgAvailabilityChecked = false;

	/** The control's shell */
	Shell fShell;

	/** The control's browser widget */
	private Browser fBrowser;

	/** Tells whether the browser has content */
	private boolean fBrowserHasContent;

	/** The control width constraint */
	private int fMaxWidth = -1;

	/** The control height constraint */
	private int fMaxHeight = -1;

	private Font fStatusTextFont;

	private boolean fHideScrollBars;

	private Listener fDeactivateListener;

	ListenerList fFocusListeners = new ListenerList();

	/**
	 * Creates a default information control with the given shell as parent. The
	 * given information presenter is used to process the information to be
	 * displayed. The given styles are applied to the created styled text
	 * widget.
	 * 
	 * @param parent
	 *            the parent shell
	 * @param shellStyle
	 *            the additional styles for the shell
	 * @param style
	 *            the additional styles for the styled text widget
	 */
	public BrowserInformationControl(Shell parent, int shellStyle, int style) {
		this(parent, shellStyle, style, null);
	}

	/**
	 * Creates a default information control with the given shell as parent. The
	 * given information presenter is used to process the information to be
	 * displayed. The given styles are applied to the created styled text
	 * widget.
	 * 
	 * @param parent
	 *            the parent shell
	 * @param shellStyle
	 *            the additional styles for the shell
	 * @param style
	 *            the additional styles for the styled text widget
	 * @param statusFieldText
	 *            the text to be used in the optional status field or
	 *            <code>null</code> if the status field should be hidden
	 */
	public BrowserInformationControl(Shell parent, int shellStyle, int style,
			String statusFieldText) {
		GridLayout layout;
		GridData gd;

		fShell = new Shell(parent, SWT.NO_FOCUS | SWT.ON_TOP | shellStyle);
		final Display display = fShell.getDisplay();
		fShell.setBackground(display.getSystemColor(SWT.COLOR_BLACK));

		final int border = ((shellStyle & SWT.NO_TRIM) == 0) ? 0 : BORDER;
		fShell.setLayout(new BorderFillLayout(border));

		Composite composite = fShell;
		layout = new GridLayout(1, false);
		layout.marginHeight = border;
		layout.marginWidth = border;
		composite.setLayout(layout);
		gd = new GridData(GridData.FILL_BOTH);
		composite.setLayoutData(gd);

		if (statusFieldText != null) {
			composite = new Composite(composite, SWT.NONE);
			layout = new GridLayout(1, false);
			layout.marginHeight = 0;
			layout.marginWidth = 0;
			composite.setLayout(layout);
			gd = new GridData(GridData.FILL_BOTH);
			composite.setLayoutData(gd);
			composite.setForeground(display
					.getSystemColor(SWT.COLOR_INFO_FOREGROUND));
			composite.setBackground(display
					.getSystemColor(SWT.COLOR_INFO_BACKGROUND));
		}

		// Browser field
		fBrowser = new Browser(fShell, SWT.NONE);
		fHideScrollBars = (style & SWT.V_SCROLL) == 0 &&
				(style & SWT.H_SCROLL) == 0;
		gd = new GridData(GridData.BEGINNING | GridData.FILL_BOTH);
		fBrowser.setLayoutData(gd);
		fBrowser.setForeground(display
				.getSystemColor(SWT.COLOR_INFO_FOREGROUND));
		fBrowser.setBackground(display
				.getSystemColor(SWT.COLOR_INFO_BACKGROUND));
		fBrowser.addKeyListener(new KeyListener() {

			public void keyPressed(KeyEvent e) {
				if (e.character == 0x1B) {
					fShell.dispose();
				}
			}

			public void keyReleased(KeyEvent e) {
			}
		});

		// Replace browser's built-in context menu with none
		fBrowser.setMenu(new Menu(fShell, SWT.NONE));

		// Status field
		if (statusFieldText != null) {

			// Horizontal separator line
			final Label separator = new Label(composite, SWT.SEPARATOR |
					SWT.HORIZONTAL | SWT.LINE_DOT);
			separator.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

			// Status field label
			final Label statusField = new Label(composite, SWT.RIGHT);
			statusField.setText(statusFieldText);
			final Font font = statusField.getFont();
			final FontData[] fontDatas = font.getFontData();
			for (FontData element : fontDatas) {
				element.setHeight(element.getHeight() * 9 / 10);
			}
			fStatusTextFont = new Font(statusField.getDisplay(), fontDatas);
			statusField.setFont(fStatusTextFont);
			gd = new GridData(GridData.FILL_HORIZONTAL |
					GridData.HORIZONTAL_ALIGN_BEGINNING |
					GridData.VERTICAL_ALIGN_BEGINNING);
			statusField.setLayoutData(gd);

			statusField.setForeground(display
					.getSystemColor(SWT.COLOR_WIDGET_DARK_SHADOW));

			statusField.setBackground(display
					.getSystemColor(SWT.COLOR_INFO_BACKGROUND));
		}

		addDisposeListener(this);
	}

	/**
	 * Creates a default information control with the given shell as parent. The
	 * given information presenter is used to process the information to be
	 * displayed. The given styles are applied to the created styled text
	 * widget.
	 * 
	 * @param parent
	 *            the parent shell
	 * @param style
	 *            the additional styles for the browser widget
	 */
	public BrowserInformationControl(Shell parent, int style) {
		this(parent, SWT.NO_TRIM, style);
	}

	/**
	 * Creates a default information control with the given shell as parent. No
	 * information presenter is used to process the information to be displayed.
	 * No additional styles are applied to the styled text widget.
	 * 
	 * @param parent
	 *            the parent shell
	 */
	public BrowserInformationControl(Shell parent) {
		this(parent, SWT.NONE);
	}

	/*
	 * @see IInformationControl#setInformation(String)
	 */
	public void setInformation(String content) {
		fBrowserHasContent = content != null && content.length() > 0;

		if (fBrowserHasContent) {
			final int shellStyle = fShell.getStyle();
			final boolean RTL = (shellStyle & SWT.RIGHT_TO_LEFT) != 0;

			String[] styles = null;
			if (RTL && !fHideScrollBars) {
				styles = new String[] { "direction:rtl" }; //$NON-NLS-1$
			} else if (RTL && fHideScrollBars) {
				styles = new String[] { "direction:rtl", "overflow:hidden" }; //$NON-NLS-1$ //$NON-NLS-2$
			} else if (fHideScrollBars) {
				styles = new String[] { "overflow:hidden" }; //$NON-NLS-1$
			}

			if (styles != null) {
				final StringBuffer buffer = new StringBuffer(content);
				insertStyles(buffer, styles);
				content = buffer.toString();
			}
		}

		fBrowser.setText(content);
		fBrowser.setSize(Math.min(200, fMaxWidth), Math.min(fMaxHeight, 50));
	}

	private void insertStyles(StringBuffer buffer, String[] styles) {
		if (styles == null || styles.length == 0) {
			return;
		}

		final StringBuffer styleBuf = new StringBuffer(10 * styles.length);
		for (String element : styles) {
			styleBuf.append(" style=\""); //$NON-NLS-1$
			styleBuf.append(element);
			styleBuf.append('"');
		}

		// Find insertion index
		final int index = buffer.indexOf("<body "); //$NON-NLS-1$
		if (index == -1) {
			return;
		}

		buffer.insert(index + 5, styleBuf);
	}

	/*
	 * @see IInformationControl#setVisible(boolean)
	 */
	public void setVisible(boolean visible) {
		fShell.setVisible(visible);
	}

	/*
	 * @see IInformationControl#dispose()
	 */
	public void dispose() {
		if (fShell != null && !fShell.isDisposed()) {
			fShell.dispose();
		} else {
			widgetDisposed(null);
		}
	}

	/*
	 * @see org.eclipse.swt.events.DisposeListener#widgetDisposed(org.eclipse.swt.events.DisposeEvent)
	 */
	public void widgetDisposed(DisposeEvent event) {
		if (fStatusTextFont != null && !fStatusTextFont.isDisposed()) {
			fStatusTextFont.dispose();
		}

		fShell = null;
		fBrowser = null;
		fStatusTextFont = null;
	}

	/*
	 * @see IInformationControl#setSize(int, int)
	 */
	public void setSize(int width, int height) {
		fShell
				.setSize(Math.min(width, fMaxWidth), Math.min(height,
						fMaxHeight));
	}

	/*
	 * @see IInformationControl#setLocation(Point)
	 */
	public void setLocation(Point location) {
		final Rectangle trim = fShell.computeTrim(0, 0, 0, 0);
		final Point textLocation = fBrowser.getLocation();
		location.x += trim.x - textLocation.x;
		location.y += trim.y - textLocation.y;
		fShell.setLocation(location);
	}

	/*
	 * @see IInformationControl#setSizeConstraints(int, int)
	 */
	public void setSizeConstraints(int maxWidth, int maxHeight) {
		fMaxWidth = maxWidth;
		fMaxHeight = maxHeight;
	}

	/*
	 * @see IInformationControl#computeSizeHint()
	 */
	public Point computeSizeHint() {
		return fShell.computeSize(SWT.DEFAULT, SWT.DEFAULT);
	}

	/*
	 * @see org.eclipse.jface.text.IInformationControlExtension3#computeTrim()
	 */
	public Rectangle computeTrim() {
		return fShell.computeTrim(0, 0, 0, 0);
	}

	/*
	 * @see org.eclipse.jface.text.IInformationControlExtension3#getBounds()
	 */
	public Rectangle getBounds() {
		return fShell.getBounds();
	}

	/*
	 * @see org.eclipse.jface.text.IInformationControlExtension3#restoresLocation()
	 */
	public boolean restoresLocation() {
		return false;
	}

	/*
	 * @see org.eclipse.jface.text.IInformationControlExtension3#restoresSize()
	 */
	public boolean restoresSize() {
		return false;
	}

	/*
	 * @see IInformationControl#addDisposeListener(DisposeListener)
	 */
	public void addDisposeListener(DisposeListener listener) {
		fShell.addDisposeListener(listener);
	}

	/*
	 * @see IInformationControl#removeDisposeListener(DisposeListener)
	 */
	public void removeDisposeListener(DisposeListener listener) {
		fShell.removeDisposeListener(listener);
	}

	/*
	 * @see IInformationControl#setForegroundColor(Color)
	 */
	public void setForegroundColor(Color foreground) {
		fBrowser.setForeground(foreground);
	}

	/*
	 * @see IInformationControl#setBackgroundColor(Color)
	 */
	public void setBackgroundColor(Color background) {
		fBrowser.setBackground(background);
	}

	/*
	 * @see IInformationControl#isFocusControl()
	 */
	public boolean isFocusControl() {
		return fBrowser.isFocusControl();
	}

	/*
	 * @see IInformationControl#setFocus()
	 */
	public void setFocus() {
		fShell.forceFocus();
		fBrowser.setFocus();
	}

	/*
	 * @see IInformationControl#addFocusListener(FocusListener)
	 */
	public void addFocusListener(final FocusListener listener) {
		fBrowser.addFocusListener(listener);

		/*
		 * FIXME: This is a workaround for bug
		 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=84532 (Browser widget
		 * does not send focusLost event)
		 */
		if (fFocusListeners.isEmpty()) {
			fDeactivateListener = new Listener() {

				public void handleEvent(Event event) {
					final Object[] listeners = fFocusListeners.getListeners();
					for (Object element : listeners) {
						((FocusListener) element).focusLost(new FocusEvent(
								event));
					}
				}
			};
			fBrowser.getShell()
					.addListener(SWT.Deactivate, fDeactivateListener);
		}
		fFocusListeners.add(listener);
	}

	/*
	 * @see IInformationControl#removeFocusListener(FocusListener)
	 */
	public void removeFocusListener(FocusListener listener) {
		fBrowser.removeFocusListener(listener);

		/*
		 * FIXME: This is a workaround for bug
		 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=84532 (Browser widget
		 * does not send focusLost event)
		 */
		fFocusListeners.remove(listener);
		if (fFocusListeners.isEmpty()) {
			fBrowser.getShell().removeListener(SWT.Deactivate,
					fDeactivateListener);
			fDeactivateListener = null;
		}
	}

	/*
	 * @see IInformationControlExtension#hasContents()
	 */
	public boolean hasContents() {
		return fBrowserHasContent;
	}
}
