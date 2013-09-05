package org.erlide.ui.editors.util;

import org.eclipse.ui.IEditorInput;
import org.erlide.engine.model.erlang.IErlModule;

public interface IErlangExternalEditorInput extends IEditorInput {
    IErlModule getModule();
}
