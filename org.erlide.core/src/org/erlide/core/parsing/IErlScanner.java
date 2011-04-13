package org.erlide.core.parsing;

public interface IErlScanner {

    public abstract void addRef();

    public abstract boolean willDispose();

    public abstract void dispose();

    public abstract void replaceText(final int offset, final int removeLength,
            final String newText);

    public abstract ErlToken getTokenAt(final int offset);

    public abstract String getText();

}
