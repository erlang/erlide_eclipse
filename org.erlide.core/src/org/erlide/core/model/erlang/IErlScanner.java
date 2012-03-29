package org.erlide.core.model.erlang;

import org.erlide.utils.IDisposable;

public interface IErlScanner extends IDisposable {

    String getName();

    void addRef();

    boolean willDispose();

    void replaceText(final int offset, final int removeLength,
            final String newText);

    ErlToken getTokenAt(final int offset);

    String getText();

}
