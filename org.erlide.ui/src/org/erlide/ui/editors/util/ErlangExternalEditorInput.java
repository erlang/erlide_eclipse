package org.erlide.ui.editors.util;

import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.ide.FileStoreEditorInput;
import org.erlide.core.model.erlang.IErlModule;

public class ErlangExternalEditorInput extends FileStoreEditorInput implements
        IErlangExternalEditorInput {

    final private IErlModule module;

    public ErlangExternalEditorInput(final IFileStore fileStore,
            final IErlModule module) {
        super(fileStore);
        this.module = module;
    }

    @Override
    public IErlModule getModule() {
        return module;
    }

    @Override
    public String getFactoryId() {
        return ErlangExternalEditorInputFactory.ID;
    }

    @Override
    public void saveState(final IMemento memento) {
        ErlangExternalEditorInputFactory.saveState(memento, this);
    }

}
