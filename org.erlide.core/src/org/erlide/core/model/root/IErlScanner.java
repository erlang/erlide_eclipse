package org.erlide.core.model.root;

public interface IErlScanner {

    String getName();

    void addRef();

    boolean willDispose();

    void dispose();

    void replaceText(final int offset, final int removeLength,
            final String newText);

    ErlToken getTokenAt(final int offset);

    String getText();

}
