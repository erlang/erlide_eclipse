package org.erlide.ui.prefs;

import java.util.Map;

import org.eclipse.jface.text.TextAttribute;
import org.eclipse.swt.graphics.Color;
import org.erlide.ui.editors.erl.scanner.ErlCodeScanner;
import org.erlide.ui.util.IColorManager;

public class SyntaxColorPreviewHighlightScanner extends ErlCodeScanner {
    // implements IPropertyChangeListener {

    final Map<TokenHighlight, HighlightStyle> styles;

    public class PreviewTextAttribute extends TextAttribute {
        final TokenHighlight th;

        public PreviewTextAttribute(final TokenHighlight th) {
            super(null);
            this.th = th;
        }

        @Override
        public Color getForeground() {
            return fColorManager.getColor(styles.get(th).getColor());
        }

        @Override
        public int getStyle() {
            return styles.get(th).getStyle();
        }

    }

    public SyntaxColorPreviewHighlightScanner(final IColorManager lmanager,
            final Map<TokenHighlight, HighlightStyle> styles) {
        super(lmanager);
        this.styles = styles;
    }

    @Override
    protected TextAttribute getTextAttribute(final TokenHighlight th) {
        return new PreviewTextAttribute(th);
    }

    // private HighlightStyle loadHighlightStyle(final String qualifier) {
    // final RGB color = StringConverter.asRGB(fStore.getString(qualifier
    // + HighlightStyle.COLOR_KEY));
    // final int style = fStore.getInt(qualifier + HighlightStyle.STYLE_KEY);
    // return new HighlightStyle(color, style);
    // }
    //
    // public void propertyChange(final PropertyChangeEvent event) {
    // ;
    // }
}
