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
package org.erlide.ui.views;

import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlExtension;
import org.eclipse.jface.text.source.SourceViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
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
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.erlide.ui.editors.erl.ColorManager;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.erl.ErlangSourceViewerConfiguration;

/**
 * Source viewer based implementation of
 * {@link org.eclipse.jface.text.IInformationControl}. Displays information in a
 * source viewer.
 * 
 * 
 */
public class SourceViewerInformationControl implements IInformationControl,
        IInformationControlExtension, DisposeListener {

    /** Border thickness in pixels. */
    private static final int BORDER = 1;

    /** The control's shell */
    Shell fShell;

    /** The control's text widget */
    private StyledText fText;

    /** The control's source viewer */
    private final SourceViewer fViewer;

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

    /**
     * Creates a source viewer information control with the given shell as
     * parent and the given font.
     * 
     * @param parent
     *            the parent shell
     * @param symbolicFontName
     *            the symbolic font name
     */
    public SourceViewerInformationControl(final Shell parent,
            final String symbolicFontName) {
        this(parent, SWT.NO_TRIM | SWT.TOOL, SWT.NONE, symbolicFontName, null);
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
     */
    public SourceViewerInformationControl(final Shell parent,
            final int shellStyle, final int style,
            final String symbolicFontName, final String statusFieldText) {
        GridLayout layout;
        GridData gd;

        fShell = new Shell(parent, SWT.NO_FOCUS | SWT.ON_TOP | shellStyle);
        final Display display = fShell.getDisplay();
        fShell.setBackground(display.getSystemColor(SWT.COLOR_BLACK));

        Composite composite = fShell;
        layout = new GridLayout(1, false);
        final int border = (shellStyle & SWT.NO_TRIM) == 0 ? 0 : BORDER;
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

        // Source viewer
        fViewer = new SourceViewer(composite, null, style);
        fViewer.configure(new ErlangSourceViewerConfiguration(ErlangEditor
                .getErlangEditorPreferenceStore(), new ColorManager()));
        fViewer.setEditable(false);

        fText = fViewer.getTextWidget();
        gd = new GridData(GridData.BEGINNING | GridData.FILL_BOTH);
        fText.setLayoutData(gd);
        fText.setForeground(parent.getDisplay().getSystemColor(
                SWT.COLOR_INFO_FOREGROUND));
        fText.setBackground(parent.getDisplay().getSystemColor(
                SWT.COLOR_INFO_BACKGROUND));
        fText.setFont(JFaceResources.getFont(symbolicFontName));

        fText.addKeyListener(new KeyListener() {

            @Override
            public void keyPressed(final KeyEvent e) {
                if (e.character == 0x1B) {
                    fShell.dispose();
                }
            }

            @Override
            public void keyReleased(final KeyEvent e) {
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
            for (final FontData element : fontDatas) {
                element.setHeight(element.getHeight() * 9 / 10);
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
    public void setInput(final Object input) {
        if (input instanceof String) {
            setInformation((String) input);
        } else {
            setInformation(null);
        }
    }

    /*
     * @see IInformationControl#setInformation(String)
     */
    @Override
    public void setInformation(final String content) {
        if (content == null) {
            fViewer.setInput(null);
            return;
        }

        final IDocument doc = new Document(content);
        fViewer.setInput(doc);
    }

    /*
     * @see IInformationControl#setVisible(boolean)
     */
    @Override
    public void setVisible(final boolean visible) {
        fShell.setVisible(visible);
    }

    /*
     * @see
     * org.eclipse.swt.events.DisposeListener#widgetDisposed(org.eclipse.swt
     * .events.DisposeEvent)
     */
    @Override
    public void widgetDisposed(final DisposeEvent event) {
        if (fStatusTextFont != null && !fStatusTextFont.isDisposed()) {
            fStatusTextFont.dispose();
        }

        fStatusTextFont = null;
        fShell = null;
        fText = null;
    }

    /*
     * @see org.eclipse.jface.text.IInformationControl#dispose()
     */
    @Override
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
    @Override
    public void setSize(final int width, final int height) {

        if (fStatusField != null) {
            final GridData gd = (GridData) fViewer.getTextWidget()
                    .getLayoutData();
            final Point statusSize = fStatusField.computeSize(SWT.DEFAULT,
                    SWT.DEFAULT, true);
            final Point separatorSize = fSeparator.computeSize(SWT.DEFAULT,
                    SWT.DEFAULT, true);
            gd.heightHint = height - statusSize.y - separatorSize.y;
        }
        fShell.setSize(width, height);

        if (fStatusField != null) {
            fShell.pack(true);
        }
    }

    /*
     * @see IInformationControl#setLocation(Point)
     */
    @Override
    public void setLocation(final Point location) {
        final Rectangle trim = fShell.computeTrim(0, 0, 0, 0);
        final Point textLocation = fText.getLocation();
        location.x += trim.x - textLocation.x;
        location.y += trim.y - textLocation.y;
        fShell.setLocation(location);
    }

    /*
     * @see IInformationControl#setSizeConstraints(int, int)
     */
    @Override
    public void setSizeConstraints(final int maxWidth, final int maxHeight) {
        fMaxWidth = maxWidth;
        fMaxHeight = maxHeight;
    }

    /*
     * @see IInformationControl#computeSizeHint()
     */
    @Override
    public Point computeSizeHint() {
        final Point size = fShell.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        size.x = Math.min(size.x, fMaxWidth);
        size.y = Math.min(size.y, fMaxHeight);
        return size;
    }

    /*
     * @see IInformationControl#addDisposeListener(DisposeListener)
     */
    @Override
    public void addDisposeListener(final DisposeListener listener) {
        fShell.addDisposeListener(listener);
    }

    /*
     * @see IInformationControl#removeDisposeListener(DisposeListener)
     */
    @Override
    public void removeDisposeListener(final DisposeListener listener) {
        fShell.removeDisposeListener(listener);
    }

    /*
     * @see IInformationControl#setForegroundColor(Color)
     */
    @Override
    public void setForegroundColor(final Color foreground) {
        fText.setForeground(foreground);
    }

    /*
     * @see IInformationControl#setBackgroundColor(Color)
     */
    @Override
    public void setBackgroundColor(final Color background) {
        fText.setBackground(background);
    }

    /*
     * @see IInformationControl#isFocusControl()
     */
    @Override
    public boolean isFocusControl() {
        return fText.isFocusControl();
    }

    /*
     * @see IInformationControl#setFocus()
     */
    @Override
    public void setFocus() {
        fShell.forceFocus();
        fText.setFocus();
    }

    /*
     * @see IInformationControl#addFocusListener(FocusListener)
     */
    @Override
    public void addFocusListener(final FocusListener listener) {
        fText.addFocusListener(listener);
    }

    /*
     * @see IInformationControl#removeFocusListener(FocusListener)
     */
    @Override
    public void removeFocusListener(final FocusListener listener) {
        fText.removeFocusListener(listener);
    }

    /*
     * @see IInformationControlExtension#hasContents()
     */
    @Override
    public boolean hasContents() {
        return fText.getCharCount() > 0;
    }
}
