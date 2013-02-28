package org.erlide.model.erlang;

import org.erlide.util.IDisposable;

public interface IErlScanner extends IDisposable {

    String getName();

    void initialScan(final String initialText, final String path,
            final boolean logging);

    void replaceText(final int offset, final int removeLength,
            final String newText);

    ErlToken getTokenAt(final int offset);

    String getText();

    void addref();

}
