package org.erlide.ui.editors.scratchpad;

import org.eclipse.jface.preference.IPreferenceStore;
import org.erlide.model.erlang.IErlModule;
import org.erlide.model.root.IErlProject;
import org.erlide.ui.editors.erl.EditorConfiguration;
import org.erlide.ui.util.IColorManager;

public class ErlangScratchPadConfiguration extends EditorConfiguration {

    private final ErlangScratchPad erlangScratchPad;

    public ErlangScratchPadConfiguration(final IPreferenceStore store,
            final IColorManager colorManager,
            final ErlangScratchPad erlangScratchPad) {
        super(store, erlangScratchPad, colorManager);
        this.erlangScratchPad = erlangScratchPad;
    }

    @Override
    protected IErlProject getProject() {
        return erlangScratchPad.getProject();
    }

    @Override
    protected IErlModule getModule() {
        return null;
    }
}
