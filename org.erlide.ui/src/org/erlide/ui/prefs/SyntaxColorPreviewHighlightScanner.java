package org.erlide.ui.prefs;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.swt.graphics.Color;
import org.erlide.ui.editors.erl.scanner.ErlCodeScanner;
import org.erlide.ui.util.IColorManager;

public class SyntaxColorPreviewHighlightScanner extends ErlCodeScanner {
    // implements IPropertyChangeListener {

    private final IPreferenceStore store;

    public class PreviewTextAttribute extends TextAttribute {
        final TokenHighlight th;

        public PreviewTextAttribute(final TokenHighlight th) {
            super(null);
            this.th = th;
        }

        @Override
        public Color getForeground() {
            return fColorManager.getColor(th.getStyle(store).getColor());
        }

        @Override
        public int getStyle() {
            return th.getStyle(store).getStyles();
        }

    }

    public SyntaxColorPreviewHighlightScanner(final IColorManager lmanager,
            final IPreferenceStore store) {
        super(lmanager);
        this.store = store;
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
