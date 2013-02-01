package org.erlide.ui.editors.erl;

import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.editors.text.TextEditor;
import org.erlide.core.model.erlang.ErlToken;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.IErlScanner;
import org.erlide.core.model.root.IErlElement;

public abstract class AbstractErlangEditor extends TextEditor {

    public abstract void reconcileNow();

    public abstract IErlElement getElementAt(int offset, boolean b);

    public abstract ErlToken getTokenAt(int offset);

    public abstract IErlModule getModule();

    public abstract IDocument getDocument();

    public abstract IErlScanner getScanner();

}
