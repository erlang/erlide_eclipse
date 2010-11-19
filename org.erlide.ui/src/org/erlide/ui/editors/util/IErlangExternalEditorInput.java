package org.erlide.ui.editors.util;

import org.eclipse.ui.IEditorInput;
import org.erlide.core.erlang.IErlModule;

public interface IErlangExternalEditorInput extends IEditorInput {
    IErlModule getModule();
}
