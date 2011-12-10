/*******************************************************************************
 * Copyright (c) 2004 Stefan Zeiger and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.novocode.com/legal/epl-v10.html
 * 
 * Contributors:
 *     Stefan Zeiger (szeiger@novocode.com) - initial API and implementation
 *******************************************************************************/

package com.novocode.naf.swt.custom;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.graphics.Region;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.swt.widgets.Widget;

/**
 * A Shell wrapper which creates balloon popup windows.
 * 
 * <p>
 * By default, a balloon window has no title bar or system controls. The
 * following styles are supported:
 * </p>
 * 
 * <ul>
 * <li>SWT.ON_TOP - Keep the window on top of other windows</li>
 * <li>SWT.TOOL - Add a drop shadow to the window (on supported platforms)</li>
 * <li>SWT.CLOSE - Show a "close" control on the title bar (implies SWT.TITLE)</li>
 * <li>SWT.TITLE - Show a title bar</li>
 * </ul>
 * 
 * @author Stefan Zeiger (szeiger@novocode.com)
 * @since Jul 2, 2004
 * @version $Id$
 */

public class BalloonWindow {

    final Shell shell;
    private final Composite contents;
    private Label titleLabel;
    private Canvas titleImageLabel;
    private final int style;
    private int preferredAnchor = SWT.BOTTOM | SWT.RIGHT;
    private boolean autoAnchor = true;
    private int locX = Integer.MIN_VALUE, locY = Integer.MIN_VALUE;
    private int marginLeft = 12;
    private int marginRight = 12;
    private int marginTop = 5;
    private int marginBottom = 10;
    private int titleSpacing = 3;
    private int titleWidgetSpacing = 8;
    private ToolBar systemControlsBar;
    final ArrayList<Control> selectionControls = new ArrayList<Control>();
    boolean addedGlobalListener;
    final ArrayList<Listener> selectionListeners = new ArrayList<Listener>();

    public BalloonWindow(final Shell parent, final int style) {
        this(null, parent, style);
    }

    public BalloonWindow(final Display display, final int style) {
        this(display, null, style);
    }

    private BalloonWindow(final Display display, final Shell parent,
            final int style) {
        this.style = style;
        final int shellStyle = style & (SWT.ON_TOP | SWT.TOOL);
        shell = display != null ? new Shell(display, SWT.NO_TRIM | shellStyle)
                : new Shell(parent, SWT.NO_TRIM | shellStyle);
        contents = new Composite(shell, SWT.NONE);

        final Color c = new Color(shell.getDisplay(), 255, 255, 225);
        shell.setBackground(c);
        shell.setForeground(shell.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        contents.setBackground(shell.getBackground());
        contents.setForeground(shell.getForeground());

        selectionControls.add(shell);
        selectionControls.add(contents);

        final Listener globalListener = new Listener() {

            @Override
            public void handleEvent(final Event event) {
                final Widget w = event.widget;
                for (int i = selectionControls.size() - 1; i >= 0; i--) {
                    if (selectionControls.get(i) == w) {
                        if ((style & SWT.CLOSE) != 0) {
                            for (int j = selectionListeners.size() - 1; j >= 0; j--) {
                                selectionListeners.get(j).handleEvent(event);
                            }
                        } else {
                            shell.close();
                        }
                        event.doit = false;
                    }
                }
            }
        };

        shell.addListener(SWT.Show, new Listener() {

            @Override
            public void handleEvent(final Event event) {
                if (!addedGlobalListener) {
                    shell.getDisplay().addFilter(SWT.MouseDown, globalListener);
                    addedGlobalListener = true;
                }
            }
        });

        shell.addListener(SWT.Hide, new Listener() {

            @Override
            public void handleEvent(final Event event) {
                if (addedGlobalListener) {
                    shell.getDisplay().removeFilter(SWT.MouseDown,
                            globalListener);
                    addedGlobalListener = false;
                }
            }
        });

        shell.addListener(SWT.Dispose, new Listener() {

            @Override
            public void handleEvent(final Event event) {
                if (addedGlobalListener) {
                    shell.getDisplay().removeFilter(SWT.MouseDown,
                            globalListener);
                    addedGlobalListener = false;
                }
                c.dispose();
            }
        });
    }

    /**
     * Adds a control to the list of controls which close the balloon window.
     * The background, title image and title text are included by default.
     */

    public void addSelectionControl(final Control c) {
        selectionControls.add(c);
    }

    public void addListener(final int type, final Listener l) {
        if (type == SWT.Selection) {
            selectionListeners.add(l);
        }
    }

    /**
     * Set the location of the anchor. This must be one of the following values:
     * SWT.NONE, SWT.LEFT|SWT.TOP, SWT.RIGHT|SWT.TOP, SWT.LEFT|SWT.BOTTOM,
     * SWT.RIGHT|SWT.BOTTOM
     */

    public void setAnchor(final int anchor) {
        switch (anchor) {
        case SWT.NONE:
        case SWT.LEFT | SWT.TOP:
        case SWT.RIGHT | SWT.TOP:
        case SWT.LEFT | SWT.BOTTOM:
        case SWT.RIGHT | SWT.BOTTOM:
            break;
        default:
            throw new IllegalArgumentException("Illegal anchor value " + anchor);
        }
        preferredAnchor = anchor;
    }

    public void setAutoAnchor(final boolean autoAnchor) {
        this.autoAnchor = autoAnchor;
    }

    public void setLocation(final int x, final int y) {
        locX = x;
        locY = y;
    }

    public void setLocation(final Point p) {
        locX = p.x;
        locY = p.y;
    }

    public void setText(final String title) {
        shell.setText(title);
    }

    public void setImage(final Image image) {
        shell.setImage(image);
    }

    public void setMargins(final int marginLeft, final int marginRight,
            final int marginTop, final int marginBottom) {
        this.marginLeft = marginLeft;
        this.marginRight = marginRight;
        this.marginTop = marginTop;
        this.marginBottom = marginBottom;
    }

    public void setMargins(final int marginX, final int marginY) {
        setMargins(marginX, marginX, marginY, marginY);
    }

    public void setMargins(final int margin) {
        setMargins(margin, margin, margin, margin);
    }

    public void setTitleSpacing(final int titleSpacing) {
        this.titleSpacing = titleSpacing;
    }

    public void setTitleWidgetSpacing(final int titleImageSpacing) {
        titleWidgetSpacing = titleImageSpacing;
    }

    public Shell getShell() {
        return shell;
    }

    public Composite getContents() {
        return contents;
    }

    public void prepareForOpen() {
        final Point contentsSize = contents.getSize();
        Point titleSize = new Point(0, 0);

        final boolean showTitle = (style & (SWT.CLOSE | SWT.TITLE)) != 0;
        if (showTitle) {
            if (titleLabel == null) {
                titleLabel = new Label(shell, SWT.NONE);
                titleLabel.setBackground(shell.getBackground());
                titleLabel.setForeground(shell.getForeground());
                final FontData[] fds = shell.getFont().getFontData();
                for (final FontData element : fds) {
                    element.setStyle(element.getStyle() | SWT.BOLD);
                }
                final Font font = new Font(shell.getDisplay(), fds);
                titleLabel.addListener(SWT.Dispose, new Listener() {

                    @Override
                    public void handleEvent(final Event event) {
                        font.dispose();
                    }
                });
                titleLabel.setFont(font);
                selectionControls.add(titleLabel);
            }
            final String titleText = shell.getText();
            titleLabel.setText(titleText == null ? "" : titleText);
            titleLabel.pack();
            titleSize = titleLabel.getSize();

            final Image titleImage = shell.getImage();
            if (titleImageLabel == null && shell.getImage() != null) {
                titleImageLabel = new Canvas(shell, SWT.NONE);
                titleImageLabel.setBackground(shell.getBackground());
                titleImageLabel.setBounds(titleImage.getBounds());
                titleImageLabel.addListener(SWT.Paint, new Listener() {

                    @Override
                    public void handleEvent(final Event event) {
                        event.gc.drawImage(titleImage, 0, 0);
                    }
                });
                final Point tilSize = titleImageLabel.getSize();
                titleSize.x += tilSize.x + titleWidgetSpacing;
                if (tilSize.y > titleSize.y) {
                    titleSize.y = tilSize.y;
                }
                selectionControls.add(titleImageLabel);
            }

            if (systemControlsBar == null && (style & SWT.CLOSE) != 0) {
                // Color closeFG = shell.getForeground(), closeBG =
                // shell.getBackground();
                // Color closeFG =
                // shell.getDisplay().getSystemColor(SWT.COLOR_DARK_GRAY),
                // closeBG = shell.getBackground();
                final Color closeFG = shell.getDisplay().getSystemColor(
                        SWT.COLOR_WIDGET_FOREGROUND), closeBG = shell
                        .getDisplay().getSystemColor(
                                SWT.COLOR_WIDGET_BACKGROUND);
                final Image closeImage = createCloseImage(shell.getDisplay(),
                        closeBG, closeFG);
                shell.addListener(SWT.Dispose, new Listener() {

                    @Override
                    public void handleEvent(final Event event) {
                        closeImage.dispose();
                    }
                });
                systemControlsBar = new ToolBar(shell, SWT.FLAT);
                systemControlsBar.setBackground(closeBG);
                systemControlsBar.setForeground(closeFG);
                final ToolItem closeItem = new ToolItem(systemControlsBar,
                        SWT.PUSH);
                closeItem.setImage(closeImage);
                closeItem.addListener(SWT.Selection, new Listener() {

                    @Override
                    public void handleEvent(final Event event) {
                        shell.close();
                    }
                });
                systemControlsBar.pack();
                final Point closeSize = systemControlsBar.getSize();
                titleSize.x += closeSize.x + titleWidgetSpacing;
                if (closeSize.y > titleSize.y) {
                    titleSize.y = closeSize.y;
                }
            }

            titleSize.y += titleSpacing;
            if (titleSize.x > contentsSize.x) {
                contentsSize.x = titleSize.x;
                contents.setSize(contentsSize.x, contentsSize.y);
            }
            contentsSize.y += titleSize.y;
        }

        final Rectangle screen = shell.getDisplay().getClientArea();

        int anchor = preferredAnchor;
        if (anchor != SWT.NONE && autoAnchor && locX != Integer.MIN_VALUE) {
            if ((anchor & SWT.LEFT) != 0) {
                if (locX + contentsSize.x + marginLeft + marginRight - 16 >= screen.x
                        + screen.width) {
                    anchor = anchor - SWT.LEFT + SWT.RIGHT;
                }
            } else {
                if (locX - contentsSize.x - marginLeft - marginRight + 16 < screen.x) {
                    anchor = anchor - SWT.RIGHT + SWT.LEFT;
                }
            }
            if ((anchor & SWT.TOP) != 0) {
                if (locY + contentsSize.y + 20 + marginTop + marginBottom >= screen.y
                        + screen.height) {
                    anchor = anchor - SWT.TOP + SWT.BOTTOM;
                }
            } else {
                if (locY - contentsSize.y - 20 - marginTop - marginBottom < screen.y) {
                    anchor = anchor - SWT.BOTTOM + SWT.TOP;
                }
            }
        }

        final Point shellSize = anchor == SWT.NONE ? new Point(contentsSize.x
                + marginLeft + marginRight, contentsSize.y + marginTop
                + marginBottom) : new Point(contentsSize.x + marginLeft
                + marginRight, contentsSize.y + marginTop + marginBottom + 20);

        if (shellSize.x < 54 + marginLeft + marginRight) {
            shellSize.x = 54 + marginLeft + marginRight;
        }
        if (anchor == SWT.NONE) {
            if (shellSize.y < 10 + marginTop + marginBottom) {
                shellSize.y = 10 + marginTop + marginBottom;
            }
        } else {
            if (shellSize.y < 30 + marginTop + marginBottom) {
                shellSize.y = 30 + marginTop + marginBottom;
            }
        }

        shell.setSize(shellSize);
        final int titleLocY = marginTop + ((anchor & SWT.TOP) != 0 ? 20 : 0);
        contents.setLocation(marginLeft, titleSize.y + titleLocY);
        if (showTitle) {
            final int realTitleHeight = titleSize.y - titleSpacing;
            if (titleImageLabel != null) {
                titleImageLabel.setLocation(marginLeft, titleLocY
                        + (realTitleHeight - titleImageLabel.getSize().y) / 2);
                titleLabel.setLocation(marginLeft + titleImageLabel.getSize().x
                        + titleWidgetSpacing, titleLocY
                        + (realTitleHeight - titleLabel.getSize().y) / 2);
            } else {
                titleLabel.setLocation(marginLeft, titleLocY
                        + (realTitleHeight - titleLabel.getSize().y) / 2);
            }
            if (systemControlsBar != null) {
                systemControlsBar
                        .setLocation(
                                shellSize.x - marginRight
                                        - systemControlsBar.getSize().x,
                                titleLocY
                                        + (realTitleHeight - systemControlsBar
                                                .getSize().y) / 2);
            }
        }

        final Region region = new Region();
        region.add(createOutline(shellSize, anchor, true));

        shell.setRegion(region);
        shell.addListener(SWT.Dispose, new Listener() {

            @Override
            public void handleEvent(final Event event) {
                region.dispose();
            }
        });

        final int[] outline = createOutline(shellSize, anchor, false);
        shell.addListener(SWT.Paint, new Listener() {

            @Override
            public void handleEvent(final Event event) {
                event.gc.drawPolygon(outline);
            }
        });

        if (locX != Integer.MIN_VALUE) {
            final Point shellLoc = new Point(locX, locY);
            if ((anchor & SWT.BOTTOM) != 0) {
                shellLoc.y = shellLoc.y - shellSize.y + 1;
            }
            if ((anchor & SWT.LEFT) != 0) {
                shellLoc.x -= 15;
            } else if ((anchor & SWT.RIGHT) != 0) {
                shellLoc.x = shellLoc.x - shellSize.x + 16;
            }

            if (autoAnchor) {
                if (shellLoc.x < screen.x) {
                    shellLoc.x = screen.x;
                } else if (shellLoc.x > screen.x + screen.width - shellSize.x) {
                    shellLoc.x = screen.x + screen.width - shellSize.x;
                }

                if (anchor == SWT.NONE) {
                    if (shellLoc.y < screen.y) {
                        shellLoc.y = screen.y;
                    } else if (shellLoc.y > screen.y + screen.height
                            - shellSize.y) {
                        shellLoc.y = screen.y + screen.height - shellSize.y;
                    }
                }
            }

            shell.setLocation(shellLoc);
        }
    }

    public void open() {
        prepareForOpen();
        shell.open();
    }

    public void close() {
        shell.close();
    }

    public void setVisible(final boolean visible) {
        if (visible) {
            prepareForOpen();
        }
        shell.setVisible(visible);
    }

    private static int[] createOutline(final Point size, final int anchor,
            final boolean outer) {
        final int o = outer ? 1 : 0;
        final int w = size.x + o;
        final int h = size.y + o;

        switch (anchor) {
        case SWT.RIGHT | SWT.BOTTOM:
            return new int[] {
                    // top and top right
                    5, 0, w - 6, 0,
                    w - 6,
                    1,
                    w - 4,
                    1,
                    w - 4,
                    2,
                    w - 3,
                    2,
                    w - 3,
                    3,
                    w - 2,
                    3,
                    w - 2,
                    5,
                    w - 1,
                    5,
                    // right and bottom right
                    w - 1, h - 26, w - 2,
                    h - 26,
                    w - 2,
                    h - 24,
                    w - 3,
                    h - 24,
                    w - 3,
                    h - 23,
                    w - 4,
                    h - 23,
                    w - 4,
                    h - 22,
                    w - 6,
                    h - 22,
                    w - 6,
                    h - 21,
                    // bottom with anchor
                    w - 16, h - 21, w - 16, h - 1, w - 16 - o, h - 1,
                    w - 16 - o, h - 2, w - 17 - o, h - 2, w - 17 - o, h - 3,
                    w - 18 - o, h - 3, w - 18 - o, h - 4, w - 19 - o, h - 4,
                    w - 19 - o, h - 5, w - 20 - o, h - 5, w - 20 - o, h - 6,
                    w - 21 - o, h - 6, w - 21 - o, h - 7, w - 22 - o, h - 7,
                    w - 22 - o, h - 8, w - 23 - o, h - 8, w - 23 - o, h - 9,
                    w - 24 - o, h - 9, w - 24 - o, h - 10, w - 25 - o, h - 10,
                    w - 25 - o, h - 11, w - 26 - o, h - 11, w - 26 - o, h - 12,
                    w - 27 - o, h - 12, w - 27 - o, h - 13, w - 28 - o, h - 13,
                    w - 28 - o, h - 14, w - 29 - o, h - 14, w - 29 - o, h - 15,
                    w - 30 - o, h - 15, w - 30 - o, h - 16, w - 31 - o, h - 16,
                    w - 31 - o, h - 17, w - 32 - o, h - 17, w - 32 - o, h - 18,
                    w - 33 - o, h - 18, w - 33 - o, h - 19, w - 34 - o, h - 19,
                    w - 34 - o, h - 20, w - 35 - o, h - 20, w - 35 - o, h - 21,
                    // bottom left
                    5, h - 21, 5, h - 22, 3, h - 22, 3, h - 23, 2, h - 23, 2,
                    h - 24, 1, h - 24, 1, h - 26, 0, h - 26,
                    // left and top left
                    0, 5, 1, 5, 1, 3, 2, 3, 2, 2, 3, 2, 3, 1, 5, 1 };
        case SWT.LEFT | SWT.BOTTOM:
            return new int[] {
                    // top and top right
                    5, 0, w - 6, 0, w - 6, 1, w - 4,
                    1,
                    w - 4,
                    2,
                    w - 3,
                    2,
                    w - 3,
                    3,
                    w - 2,
                    3,
                    w - 2,
                    5,
                    w - 1,
                    5,
                    // right and bottom right
                    w - 1, h - 26, w - 2, h - 26, w - 2, h - 24,
                    w - 3,
                    h - 24,
                    w - 3,
                    h - 23,
                    w - 4,
                    h - 23,
                    w - 4,
                    h - 22,
                    w - 6,
                    h - 22,
                    w - 6,
                    h - 21,
                    // bottom with anchor
                    34 + o, h - 21, 34 + o, h - 20, 33 + o, h - 20, 33 + o,
                    h - 19, 32 + o, h - 19, 32 + o, h - 18, 31 + o, h - 18,
                    31 + o, h - 17, 30 + o, h - 17, 30 + o, h - 16, 29 + o,
                    h - 16, 29 + o, h - 15, 28 + o, h - 15, 28 + o, h - 14,
                    27 + o, h - 14, 27 + o, h - 13, 26 + o, h - 13, 26 + o,
                    h - 12, 25 + o, h - 12, 25 + o, h - 11, 24 + o, h - 11,
                    24 + o, h - 10, 23 + o, h - 10, 23 + o, h - 9, 22 + o,
                    h - 9, 22 + o, h - 8, 21 + o, h - 8, 21 + o, h - 7, 20 + o,
                    h - 7, 20 + o, h - 6, 19 + o, h - 6, 19 + o, h - 5, 18 + o,
                    h - 5, 18 + o, h - 4, 17 + o, h - 4, 17 + o, h - 3, 16 + o,
                    h - 3, 16 + o, h - 2, 15 + o, h - 2, 15, h - 1, 15, h - 21,
                    // bottom left
                    5, h - 21, 5, h - 22, 3, h - 22, 3, h - 23, 2, h - 23, 2,
                    h - 24, 1, h - 24, 1, h - 26, 0, h - 26,
                    // left and top left
                    0, 5, 1, 5, 1, 3, 2, 3, 2, 2, 3, 2, 3, 1, 5, 1 };
        case SWT.RIGHT | SWT.TOP:
            return new int[] {
                    // top with anchor
                    5, 20, w - 35 - o, 20, w - 35 - o, 19, w - 34 - o, 19,
                    w - 34 - o, 18, w - 33 - o, 18, w - 33 - o, 17, w - 32 - o,
                    17, w - 32 - o, 16, w - 31 - o, 16, w - 31 - o, 15,
                    w - 30 - o, 15, w - 30 - o, 14, w - 29 - o, 14, w - 29 - o,
                    13, w - 28 - o, 13, w - 28 - o, 12, w - 27 - o, 12,
                    w - 27 - o, 11, w - 26 - o, 11, w - 26 - o, 10, w - 25 - o,
                    10, w - 25 - o, 9, w - 24 - o, 9, w - 24 - o, 8,
                    w - 23 - o, 8, w - 23 - o, 7, w - 22 - o, 7, w - 22 - o, 6,
                    w - 21 - o, 6, w - 21 - o, 5, w - 20 - o, 5, w - 20 - o, 4,
                    w - 19 - o, 4, w - 19 - o, 3, w - 18 - o, 3, w - 18 - o, 2,
                    w - 17 - o, 2, w - 17 - o, 1, w - 16 - o, 1, w - 16 - o, 0,
                    w - 16,
                    0,
                    w - 16,
                    20,
                    // top and top right
                    w - 6, 20, w - 6, 21, w - 4, 21, w - 4, 22, w - 3, 22,
                    w - 3, 23, w - 2, 23, w - 2, 25,
                    w - 1,
                    25,
                    // right and bottom right
                    w - 1, h - 6, w - 2, h - 6, w - 2, h - 4, w - 3, h - 4,
                    w - 3, h - 3, w - 4, h - 3, w - 4, h - 2, w - 6, h - 2,
                    w - 6, h - 1,
                    // bottom and bottom left
                    5, h - 1, 5, h - 2, 3, h - 2, 3, h - 3, 2, h - 3, 2, h - 4,
                    1, h - 4, 1, h - 6, 0, h - 6,
                    // left and top left
                    0, 25, 1, 25, 1, 23, 2, 23, 2, 22, 3, 22, 3, 21, 5, 21 };
        case SWT.LEFT | SWT.TOP:
            return new int[] {
                    // top with anchor
                    5, 20, 15, 20, 15, 0, 15 + o, 0, 16 + o, 1, 16 + o, 2,
                    17 + o, 2, 17 + o, 3, 18 + o, 3, 18 + o, 4, 19 + o, 4,
                    19 + o, 5, 20 + o, 5, 20 + o, 6, 21 + o, 6, 21 + o, 7,
                    22 + o, 7, 22 + o, 8, 23 + o, 8, 23 + o, 9, 24 + o, 9,
                    24 + o, 10, 25 + o, 10, 25 + o, 11, 26 + o, 11, 26 + o, 12,
                    27 + o, 12, 27 + o, 13, 28 + o, 13, 28 + o, 14, 29 + o, 14,
                    29 + o, 15, 30 + o, 15, 30 + o, 16, 31 + o, 16, 31 + o, 17,
                    32 + o, 17, 32 + o, 18, 33 + o, 18, 33 + o, 19,
                    34 + o,
                    19,
                    34 + o,
                    20,
                    // top and top right
                    w - 6, 20, w - 6, 21, w - 4, 21, w - 4, 22, w - 3, 22,
                    w - 3, 23, w - 2, 23, w - 2, 25,
                    w - 1,
                    25,
                    // right and bottom right
                    w - 1, h - 6, w - 2, h - 6, w - 2, h - 4, w - 3, h - 4,
                    w - 3, h - 3, w - 4, h - 3, w - 4, h - 2, w - 6, h - 2,
                    w - 6, h - 1,
                    // bottom and bottom left
                    5, h - 1, 5, h - 2, 3, h - 2, 3, h - 3, 2, h - 3, 2, h - 4,
                    1, h - 4, 1, h - 6, 0, h - 6,
                    // left and top left
                    0, 25, 1, 25, 1, 23, 2, 23, 2, 22, 3, 22, 3, 21, 5, 21 };
        default:
            return new int[] {
                    // top and top right
                    5, 0, w - 6, 0, w - 6, 1, w - 4, 1, w - 4, 2, w - 3, 2,
                    w - 3, 3, w - 2, 3, w - 2, 5,
                    w - 1,
                    5,
                    // right and bottom right
                    w - 1, h - 6, w - 2, h - 6, w - 2, h - 4, w - 3, h - 4,
                    w - 3, h - 3, w - 4, h - 3, w - 4, h - 2, w - 6, h - 2,
                    w - 6, h - 1,
                    // bottom and bottom left
                    5, h - 1, 5, h - 2, 3, h - 2, 3, h - 3, 2, h - 3, 2, h - 4,
                    1, h - 4, 1, h - 6, 0, h - 6,
                    // left and top left
                    0, 5, 1, 5, 1, 3, 2, 3, 2, 2, 3, 2, 3, 1, 5, 1 };
        }
    }

    private static final Image createCloseImage(final Display display,
            final Color bg, final Color fg) {
        final int size = 11, off = 1;
        final Image image = new Image(display, size, size);
        final GC gc = new GC(image);
        gc.setBackground(bg);
        gc.fillRectangle(image.getBounds());
        gc.setForeground(fg);
        gc.drawLine(0 + off, 0 + off, size - 1 - off, size - 1 - off);
        gc.drawLine(1 + off, 0 + off, size - 1 - off, size - 2 - off);
        gc.drawLine(0 + off, 1 + off, size - 2 - off, size - 1 - off);
        gc.drawLine(size - 1 - off, 0 + off, 0 + off, size - 1 - off);
        gc.drawLine(size - 1 - off, 1 + off, 1 + off, size - 1 - off);
        gc.drawLine(size - 2 - off, 0 + off, 0 + off, size - 2 - off);
        /*
         * gc.drawLine(1, 0, size-2, 0); gc.drawLine(1, size-1, size-2, size-1);
         * gc.drawLine(0, 1, 0, size-2); gc.drawLine(size-1, 1, size-1, size-2);
         */
        gc.dispose();
        return image;
    }
}
