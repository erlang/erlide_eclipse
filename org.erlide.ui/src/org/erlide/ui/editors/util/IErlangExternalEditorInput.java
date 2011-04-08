package org.erlide.ui.editors.util;

import org.eclipse.ui.IEditorInput;
import org.erlide.core.model.erlang.IErlModule;

public interface IErlangExternalEditorInput extends IEditorInput {
    IErlModule getModule();
}
