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
package org.erlide.ui.views.console;

import java.util.List;

import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlExtension;
import org.eclipse.jface.text.IInformationControlExtension2;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

/**
 * Source viewer based implementation of
 * {@link org.eclipse.jface.text.IInformationControl}. Displays information in
 * a source viewer.
 *
 *
 */
public class ConsoleHistoryInformationControl implements IInformationControl,
		IInformationControlExtension, IInformationControlExtension2,
		DisposeListener {

	/** Border thickness in pixels. */
	private static final int BORDER = 1;

	/** The control's shell */
	Shell fShell;

	/** The control's text widget */
	Table fControl;

	/** The optional status field. */
	private Label fStatusField;

	/** The separator for the optional status field. */
	private Label fSeparator;

	/** The font of the optional status text label. */
	private Font fStatusTextFont;

	/** The maximal widget width. */
	private int fMaxWidth;

	/** The maximal widget height. */
	private int fMaxHeight;

	@SuppressWarnings("unused")
	/* TODO: Console Usage */
	private final ErlangConsoleView fConsole;

	/**
	 * Creates a source viewer information control with the given shell as
	 * parent and the given font.
	 *
	 * @param parent
	 *            the parent shell
	 * @param symbolicFontName
	 *            the symbolic font name
	 */
	public ConsoleHistoryInformationControl(Shell parent,
			String symbolicFontName, ErlangConsoleView c) {
		this(parent, SWT.NO_TRIM | SWT.TOOL, SWT.NONE, symbolicFontName, null,
				c);
	}

	/**
	 * Creates a source viewer information control with the given shell as
	 * parent. The given shell styles are applied to the created shell. The
	 * given styles are applied to the created styled text widget. The text
	 * widget will be initialized with the given font. The status field will
	 * contain the given text or be hidden.
	 *
	 * @param parent
	 *            the parent shell
	 * @param shellStyle
	 *            the additional styles for the shell
	 * @param style
	 *            the additional styles for the styled text widget
	 * @param symbolicFontName
	 *            the symbolic font name
	 * @param statusFieldText
	 *            the text to be used in the optional status field or
	 *            <code>null</code> if the status field should be hidden
	 * @param console
	 */
	public ConsoleHistoryInformationControl(Shell parent, int shellStyle,
			int style, String symbolicFontName, String statusFieldText,
			ErlangConsoleView console) {
		this.fConsole = console;
		final ErlangConsoleView c = console;

		GridLayout layout;
		GridData gd;

		fShell = new Shell(parent, SWT.NO_FOCUS | SWT.ON_TOP | shellStyle);
		final Display display = fShell.getDisplay();
		fShell.setBackground(display.getSystemColor(SWT.COLOR_BLACK));

		Composite composite = fShell;
		layout = new GridLayout(1, false);
		final int border = ((shellStyle & SWT.NO_TRIM) == 0) ? 0 : BORDER;
		layout.marginHeight = border;
		layout.marginWidth = border;
		composite.setLayout(layout);
		gd = new GridData(GridData.FILL_HORIZONTAL);
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

		fControl = new Table(fShell, SWT.BORDER | SWT.V_SCROLL);
		gd = new GridData(GridData.BEGINNING | GridData.FILL_BOTH);
		fControl.setLayoutData(gd);
		fControl.setForeground(parent.getDisplay().getSystemColor(
				SWT.COLOR_INFO_FOREGROUND));
		fControl.setBackground(parent.getDisplay().getSystemColor(
				SWT.COLOR_INFO_BACKGROUND));
		fControl.setFont(JFaceResources.getFont(symbolicFontName));
		fControl.setLinesVisible(true);
		new TableColumn(fControl, SWT.NONE);

		fControl.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				if (e.character == 0x1B) // ESC
				{
					fShell.dispose();
				} else if (e.character == 13) // RET
				{
					final String str = (String) fControl.getSelection()[0]
							.getData();
					c.setInput(str);
					fShell.dispose();
				}
			}
		});

		// Status field
		if (statusFieldText != null) {

			// Horizontal separator line
			fSeparator = new Label(composite, SWT.SEPARATOR | SWT.HORIZONTAL
					| SWT.LINE_DOT);
			fSeparator.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

			// Status field label
			fStatusField = new Label(composite, SWT.RIGHT);
			fStatusField.setText(statusFieldText);
			final Font font = fStatusField.getFont();
			final FontData[] fontDatas = font.getFontData();
			for (int i = 0; i < fontDatas.length; i++) {
				fontDatas[i].setHeight(fontDatas[i].getHeight() * 9 / 10);
			}
			fStatusTextFont = new Font(fStatusField.getDisplay(), fontDatas);
			fStatusField.setFont(fStatusTextFont);
			final GridData gd2 = new GridData(GridData.FILL_VERTICAL
					| GridData.FILL_HORIZONTAL
					| GridData.HORIZONTAL_ALIGN_BEGINNING
					| GridData.VERTICAL_ALIGN_BEGINNING);
			fStatusField.setLayoutData(gd2);
			fStatusField.setForeground(display
					.getSystemColor(SWT.COLOR_WIDGET_DARK_SHADOW));
			fStatusField.setBackground(display
					.getSystemColor(SWT.COLOR_INFO_BACKGROUND));
		}

		addDisposeListener(this);
	}

	/**
	 * @see org.eclipse.jface.text.IInformationControlExtension2#setInput(java.lang.Object)
	 * @param input
	 *            the input object
	 */
	@SuppressWarnings("unchecked")
	public void setInput(Object input) {
		fControl.removeAll();
		if (input instanceof List) {
			final List<String> list = (List<String>) input;
			for (int i = 0; i < list.size(); i++) {
				TableItem it = new TableItem(fControl, SWT.MULTI);
				String str = list.get(i);
				it.setData(str);
				it.setText(shorten(str));
			}
			fControl.setSelection(fControl.getItemCount() - 5);
		} else {
			fControl.removeAll();
		}
	}

	private String shorten(String str) {
		//int w = fControl.getClientArea().width - 10;
		String s = str.trim().replaceAll("\n", " ");
		// TODO use char width to compute how much fits
		//int end = 20;
		return s;//.substring(0, end);
	}

	/*
	 * @see IInformationControl#setInformation(String)
	 */
	public void setInformation(String content) {
	}

	/*
	 * @see IInformationControl#setVisible(boolean)
	 */
	public void setVisible(boolean visible) {
		fShell.setVisible(visible);
	}

	/*
	 * @see org.eclipse.swt.events.DisposeListener#widgetDisposed(org.eclipse.swt.events.DisposeEvent)
	 */
	public void widgetDisposed(DisposeEvent event) {
		if (fStatusTextFont != null && !fStatusTextFont.isDisposed()) {
			fStatusTextFont.dispose();
		}

		fStatusTextFont = null;
		fShell = null;
		fControl = null;
	}

	/*
	 * @see org.eclipse.jface.text.IInformationControl#dispose()
	 */
	public final void dispose() {
		if (fShell != null && !fShell.isDisposed()) {
			fShell.dispose();
		} else {
			widgetDisposed(null);
		}
	}

	/*
	 * @see IInformationControl#setSize(int, int)
	 */
	public void setSize(int width, int height) {

		if (fStatusField != null) {
			final GridData gd = (GridData) fControl.getLayoutData();
			final Point statusSize = fStatusField.computeSize(SWT.DEFAULT,
					SWT.DEFAULT, true);
			final Point separatorSize = fSeparator.computeSize(SWT.DEFAULT,
					SWT.DEFAULT, true);
			gd.heightHint = height - statusSize.y - separatorSize.y;
		}
		fShell.setSize(width, height);
		TableColumn ct = fControl.getColumn(0);
		ct.setWidth(fControl.getClientArea().width);

		if (fStatusField != null) {
			fShell.pack(true);
		}
	}

	/*
	 * @see IInformationControl#setLocation(Point)
	 */
	public void setLocation(Point location) {
		final Rectangle trim = fShell.computeTrim(0, 0, 0, 0);
		final Point textLocation = fControl.getLocation();
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
		final Point size = fShell.computeSize(SWT.DEFAULT, SWT.DEFAULT);
		size.x = Math.min(size.x, fMaxWidth);
		size.y = Math.min(size.y, fMaxHeight);
		return size;
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
		fControl.setForeground(foreground);
	}

	/*
	 * @see IInformationControl#setBackgroundColor(Color)
	 */
	public void setBackgroundColor(Color background) {
		fControl.setBackground(background);
	}

	/*
	 * @see IInformationControl#isFocusControl()
	 */
	public boolean isFocusControl() {
		return fControl.isFocusControl();
	}

	/*
	 * @see IInformationControl#setFocus()
	 */
	public void setFocus() {
		fShell.forceFocus();
		fControl.setFocus();
	}

	/*
	 * @see IInformationControl#addFocusListener(FocusListener)
	 */
	public void addFocusListener(FocusListener listener) {
		fControl.addFocusListener(listener);
	}

	/*
	 * @see IInformationControl#removeFocusListener(FocusListener)
	 */
	public void removeFocusListener(FocusListener listener) {
		fControl.removeFocusListener(listener);
	}

	/*
	 * @see IInformationControlExtension#hasContents()
	 */
	public boolean hasContents() {
		return fControl.getItemCount() > 0;
	}
}
