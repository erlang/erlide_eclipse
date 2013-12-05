package org.erlide.ui.editors.scratchpad;

import org.eclipse.jface.preference.IPreferenceStore;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.ui.editors.erl.AbstractErlangEditor;
import org.erlide.ui.editors.erl.EditorConfiguration;
import org.erlide.ui.util.IColorManager;

public class ErlangScratchPadConfiguration extends EditorConfiguration {

    private final AbstractErlangEditor abstractErlangEditor;

    public ErlangScratchPadConfiguration(final IPreferenceStore store,
            final IColorManager colorManager, final AbstractErlangEditor erlangScratchPad) {
        super(store, erlangScratchPad, colorManager);
        abstractErlangEditor = erlangScratchPad;
    }

    @Override
    protected IErlProject getProject() {
        return abstractErlangEditor.getProject();
    }

    @Override
    protected IErlModule getModule() {
        return null;
    }
}
