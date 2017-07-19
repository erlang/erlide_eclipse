/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.prefs;

import org.eclipse.swt.graphics.RGB;

import com.google.common.base.Objects;

public class HighlightStyle {

    private RGB color;
    private int styles;

    public HighlightStyle(final RGB color, final int style) {
        this.color = color;
        styles = style;
    }

    public HighlightStyle(final HighlightStyle def) {
        color = def.color;
        styles = def.styles;
    }

    public RGB getColor() {
        return color;
    }

    public void setColor(final RGB color) {
        this.color = color;
    }

    public int getStyles() {
        return styles;
    }

    public void setStyles(final int styles) {
        this.styles = styles;
    }

    public boolean hasStyle(final int flag) {
        return (styles & flag) == flag;
    }

    public void setStyle(final int flag, final boolean b) {
        if (b) {
            styles |= flag;
        } else {
            styles &= ~flag;
        }
    }

    @Override
    public boolean equals(final Object obj) {
        return Objects.equal(this, obj);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(color, styles);
    }
}
