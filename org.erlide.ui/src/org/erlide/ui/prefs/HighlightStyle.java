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

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.jface.resource.StringConverter;
import org.eclipse.swt.graphics.RGB;

public class HighlightStyle {
    public static final String STYLE_KEY = "style";
    public static final String COLOR_KEY = "color";

    private RGB color;
    private int style;

    // private HighlightStyle dflt;

    public RGB getColor() {
        return color;
    }

    public void setColor(final RGB color) {
        this.color = color;
    }

    public int getStyle() {
        return style;
    }

    public void setStyle(final int style) {
        this.style = style;
    }

    public HighlightStyle(final RGB color, final int style) {
        this.color = color;
        this.style = style;
    }

    public HighlightStyle() {
    }

    public void store(final IEclipsePreferences node) {
        if (node != null) {
            node.put(COLOR_KEY, StringConverter.asString(getColor()));
            node.putInt(STYLE_KEY, style);
        }
    }

    public void load(final IEclipsePreferences node, final HighlightStyle def) {
        // dflt = def;
        if (node != null) {
            color = StringConverter.asRGB(node.get(COLOR_KEY,
                    StringConverter.asString(def.getColor())));
            style = node.getInt(STYLE_KEY, def.getStyle());
        }
    }

    public void load(final String qualifier, final HighlightStyle def) {
        final IPreferencesService service = Platform.getPreferencesService();
        // dflt = def;
        color = StringConverter.asRGB(service.getString(qualifier, COLOR_KEY,
                StringConverter.asString(def.getColor()), null));
        style = service.getInt(qualifier, STYLE_KEY, def.getStyle(), null);
    }

    public boolean hasStyle(final int flag) {
        return (style & flag) == flag;
    }

    public void setStyle(final int flag, final boolean b) {
        if (b) {
            style |= flag;
        } else {
            style &= ~flag;
        }
    }

}
