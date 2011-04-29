package org.erlide.ui.editors.scratchpad;

import org.eclipse.jface.preference.IPreferenceStore;
import org.erlide.ui.editors.erl.ErlangSourceViewerConfiguration;
import org.erlide.ui.util.IColorManager;

public class ErlangScratchPadConfiguration extends
        ErlangSourceViewerConfiguration {

    private final ErlangScratchPad erlangScratchPad;

    public ErlangScratchPadConfiguration(final IPreferenceStore store,
            final IColorManager colorManager,
            final ErlangScratchPad erlangScratchPad) {
        super(store, colorManager);
        this.erlangScratchPad = erlangScratchPad;
    }

}
