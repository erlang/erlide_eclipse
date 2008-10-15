package org.erlide.ui.prefs;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.jface.resource.StringConverter;
import org.eclipse.swt.graphics.RGB;

public class HighlightData {
	private static final String STYLE_KEY = "style";
	private static final String COLOR_KEY = "color";

	private RGB color;
	private int style;

	public RGB getColor() {
		return color;
	}

	public void setColor(RGB color) {
		this.color = color;
	}

	public int getStyle() {
		return style;
	}

	public void setStyle(int style) {
		this.style = style;
	}

	public HighlightData(RGB color, int style) {
		this.color = color;
		this.style = style;
	}

	public HighlightData() {
	}

	public void store(IEclipsePreferences node) {
		if (node != null) {
			node.put(COLOR_KEY, StringConverter.asString(getColor()));
			node.putInt(STYLE_KEY, style);
		}
	}

	public void load(IEclipsePreferences node, HighlightData def) {
		if (node != null) {
			color = StringConverter.asRGB(node.get(COLOR_KEY, StringConverter
					.asString(def.getColor())));
			style = node.getInt(STYLE_KEY, def.getStyle());
		}
	}

	public void load(String qualifier, HighlightData def) {
		IPreferencesService service = Platform.getPreferencesService();
		color = StringConverter.asRGB(service.getString(qualifier, COLOR_KEY,
				StringConverter.asString(def.getColor()), null));
		style = service.getInt(qualifier, STYLE_KEY, def.getStyle(), null);
	}
}
